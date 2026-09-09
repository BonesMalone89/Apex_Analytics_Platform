# =========================================================================
# APEX PLATFORM: WESTERN BLOTTING & PROTEIN PREPARATION CORE
# =========================================================================

library(shiny)
library(dplyr)
library(ggplot2)
library(readr)
library(tibble)
library(DT)
library(stringr)
library(tidyr)

# --- UI Layout Component ---
ui_western_layout <- function() {
  sidebarLayout(
    sidebarPanel(
      tags$h4("🧪 Plate Reader / Bradford Ingestion"),
      radioButtons("wb_input_mode", "Data Input Method:",
                   choices = c("Paste from Notepad" = "paste", "Upload Raw Text (.txt/.csv)" = "upload"),
                   inline = TRUE),
      
      conditionalPanel(
        condition = "input.wb_input_mode == 'paste'",
        textAreaInput("wb_pasted_text", "Paste Notepad Plate Reader Export:", 
                      placeholder = "Well ID\tName\tWell\tConc/Dil\t562\tCount\tMean\tStd Dev\tCV (%)\nSPL18\t\tC1\t\t1.094\t3\t0.978\t0.102\t10.397\n\t\tC2\t\t0.906\nSTD1\t\tA1\t0\t0.079\t2\t0.079\t0.001\t0.901",
                      rows = 8)
      ),
      
      conditionalPanel(
        condition = "input.wb_input_mode == 'upload'",
        fileInput("wb_upload_file", "Upload Text File (.txt, .tsv, .dat):", accept = c(".txt", ".tsv", ".dat", ".csv"))
      ),
      
      actionButton("parse_plate_btn", "⚡ Ingest & Process Plate Reader Data", class = "btn-primary btn-block"),
      hr(),
      
      tags$h4("🎯 Gel Loading Parameters"),
      fluidRow(
        column(6, numericInput("wb_target_protein_ug", "Target Protein (µg):", value = 30, min = 1, max = 100, step = 5)),
        column(6, numericInput("wb_target_volume_ul", "Total Well Vol (µL):", value = 25, min = 5, max = 60, step = 5))
      ),
      fluidRow(
        column(6, selectInput("wb_buffer_stock", "Loading Buffer:", choices = c("4X Laemmli" = 4, "6X Laemmli" = 6, "2X Laemmli" = 2), selected = 4)),
        column(6, numericInput("wb_sample_dilution_factor", "Sample Dilution Factor:", value = 1, min = 1, max = 100, step = 1))
      ),
      hr(),
      
      tags$h4("🧹 Standard Curve Quality Control"),
      uiOutput("wb_std_filter_ui")
    ),
    
    mainPanel(
      tabsetPanel(
        type = "pills",
        
        # --- TAB 1: STANDARD CURVE & GEL LOADING RECIPE ---
        tabPanel("📊 Standard Curve & Gel Loading",
                 br(),
                 fluidRow(
                   column(6, 
                          div(class = "dash-card",
                              tags$h5(tags$strong("Protein Standard Curve (BCA / Bradford)")),
                              plotOutput("plot_bradford_curve", height = "300px"),
                              uiOutput("bradford_stats_summary_ui")
                          )
                   ),
                   column(6,
                          div(class = "dash-card",
                              tags$h5(tags$strong("Gel Loading Master Summary")),
                              tags$small(tags$em("Calculated volume requirements based on linear standard regression.")),
                              uiOutput("wb_aliquot_metrics_ui")
                          )
                   )
                 ),
                 hr(),
                 tags$h4("📋 Bench Pipetting Master Recipe Sheet"),
                 DTOutput("wb_loading_recipe_dt"),
                 br(),
                 downloadButton("download_wb_recipe_csv", "Export Loading Protocol (CSV)", class = "btn-sm btn-info")
        ),
        
        # --- TAB 2: ANTIBODY DILUTION CALCULATOR ---
        tabPanel("🧪 Antibody Dilution Engine",
                 br(),
                 wellPanel(
                   style = "background-color: #f8f9fa; border: 1px solid #e3e6f0;",
                   tags$h4(tags$strong("🔬 Primary & Secondary Antibody Dilution")),
                   fluidRow(
                     column(4, numericInput("ab_total_volume_ml", "Incubation Volume (mL):", value = 5, min = 0.5, max = 50, step = 0.5)),
                     column(4, numericInput("ab_dilution_ratio", "Dilution Factor (1:X):", value = 1000, min = 50, max = 20000, step = 250)),
                     column(4, selectInput("ab_blocking_buffer", "Diluent / Buffer:", choices = c("5% BSA in TBST", "5% Non-Fat Milk in TBST", "1X TBST", "1X PBST")))
                   )
                 ),
                 br(),
                 fluidRow(
                   column(12,
                          div(class = "dash-card",
                              tags$h4(tags$strong("📝 Master Mix Preparation")),
                              uiOutput("ab_recipe_output_ui")
                          )
                   )
                 )
        ),
        
        # --- TAB 3: RAW PARSED PLATE DATA ---
        tabPanel("📑 Raw Ingestion Summary",
                 br(),
                 tags$h4("Raw Plate Reader Extracted Values"),
                 DTOutput("wb_raw_parsed_dt")
        )
      )
    )
  )
}

# --- Server Logic Component ---
server_western_logic <- function(input, output, session, vars, ...) {
  
  # Default starter data matching your plate reader export structure
  bradford_raw_data <- reactiveVal(
    tibble(
      Well_ID = c("STD1", "STD2", "STD3", "STD4", "STD5", "STD6", "STD7", "STD8", "SPL18"),
      Type = c(rep("Standard", 8), "Sample"),
      Conc_Nominal = c(0, 2, 4, 6, 8, 10, 16, 18, NA),
      Mean_OD = c(0.079, 0.189, 0.309, 0.435, 0.528, 0.655, 0.064, 0.065, 0.978),
      Std_Dev = c(0.001, 0.016, 0.023, 0.034, 0.006, 0.013, 0.032, 0.029, 0.102),
      CV_Pct  = c(0.9, 8.2, 7.6, 7.8, 1.2, 2.1, 50.1, 44.9, 10.4)
    )
  )
  
  # --- Plate Reader Line-by-Line Parser ---
  observeEvent(input$parse_plate_btn, {
    raw_text <- ""
    
    if (input$wb_input_mode == "paste") {
      req(input$wb_pasted_text)
      raw_text <- input$wb_pasted_text
    } else {
      req(input$wb_upload_file)
      raw_text <- read_file(input$wb_upload_file$datapath)
    }
    
    parsed_summary <- tryCatch({
      # Split text into individual non-empty lines
      raw_lines <- unlist(strsplit(raw_text, "[\r\n]+"))
      raw_lines <- trimws(raw_lines)
      raw_lines <- raw_lines[raw_lines != ""]
      
      if (length(raw_lines) < 2) return(NULL)
      
      # Header line
      header_parts <- trimws(unlist(strsplit(raw_lines[1], "\t")))
      
      # Determine column index positions dynamically from header
      id_idx   <- grep("Well\\s*ID|Sample|ID", header_parts, ignore.case = TRUE)[1]
      if (is.na(id_idx)) id_idx <- 1
      
      conc_idx <- grep("Conc", header_parts, ignore.case = TRUE)[1]
      od_idx   <- grep("562|595|OD", header_parts, ignore.case = TRUE)[1]
      mean_idx <- grep("^Mean$", header_parts, ignore.case = TRUE)[1]
      sd_idx   <- grep("Std\\s*Dev|SD", header_parts, ignore.case = TRUE)[1]
      cv_idx   <- grep("CV", header_parts, ignore.case = TRUE)[1]
      
      records_list <- list()
      
      for (line_str in raw_lines[-1]) {
        # Split line on tabs (preserving empty tokens)
        tokens <- unlist(strsplit(line_str, "\t", fixed = TRUE))
        tokens <- trimws(tokens)
        
        # Check if line begins with a valid Well ID (e.g., STD1, SPL18)
        well_id_val <- if (length(tokens) >= id_idx) tokens[id_idx] else ""
        
        if (well_id_val != "") {
          # Nominal standard concentration
          conc_val <- NA_real_
          if (!is.na(conc_idx) && length(tokens) >= conc_idx && tokens[conc_idx] != "") {
            conc_val <- suppressWarnings(as.numeric(tokens[conc_idx]))
          }
          
          # Mean OD (prefer Mean column, fallback to OD column)
          mean_od_val <- NA_real_
          if (!is.na(mean_idx) && length(tokens) >= mean_idx && tokens[mean_idx] != "") {
            mean_od_val <- suppressWarnings(as.numeric(tokens[mean_idx]))
          } else if (!is.na(od_idx) && length(tokens) >= od_idx && tokens[od_idx] != "") {
            mean_od_val <- suppressWarnings(as.numeric(tokens[od_idx]))
          }
          
          # Standard deviation
          sd_val <- 0.0
          if (!is.na(sd_idx) && length(tokens) >= sd_idx && tokens[sd_idx] != "") {
            sd_val <- suppressWarnings(as.numeric(tokens[sd_idx]))
          }
          if (is.na(sd_val)) sd_val <- 0.0
          
          # CV (%)
          cv_val <- 0.0
          if (!is.na(cv_idx) && length(tokens) >= cv_idx && tokens[cv_idx] != "") {
            cv_val <- suppressWarnings(as.numeric(tokens[cv_idx]))
          } else if (!is.na(mean_od_val) && mean_od_val > 0) {
            cv_val <- (sd_val / mean_od_val) * 100
          }
          if (is.na(cv_val)) cv_val <- 0.0
          
          if (!is.na(mean_od_val)) {
            records_list[[length(records_list) + 1]] <- tibble(
              Well_ID      = well_id_val,
              Type         = if_else(grepl("^STD", well_id_val, ignore.case = TRUE), "Standard", "Sample"),
              Conc_Nominal = conc_val,
              Mean_OD      = mean_od_val,
              Std_Dev      = sd_val,
              CV_Pct       = round(cv_val, 2)
            )
          }
        }
      }
      
      bind_rows(records_list)
    }, error = function(e) {
      NULL
    })
    
    if (is.null(parsed_summary) || nrow(parsed_summary) == 0) {
      showNotification("Failed to parse plate reader text. Verify tab-delimited Notepad layout.", type = "error")
      return()
    }
    
    bradford_raw_data(parsed_summary)
    showNotification(paste("Successfully parsed", nrow(parsed_summary), "wells from plate reader export."), type = "message")
  })
  
  # --- Standard Curve QC Filter UI (Exclude Outliers like STD7/STD8) ---
  output$wb_std_filter_ui <- renderUI({
    df <- bradford_raw_data()
    std_ids <- df %>% filter(Type == "Standard") %>% pull(Well_ID)
    
    if (length(std_ids) == 0) return(tags$p("No standards detected."))
    
    # Auto-select all standards except obvious dropouts (< 0.1 OD at high nominal concentration)
    default_selected <- df %>% 
      filter(Type == "Standard") %>% 
      filter(!(Conc_Nominal > 10 & Mean_OD < 0.15)) %>% 
      pull(Well_ID)
    
    checkboxGroupInput("wb_active_standards", "Active Standard Wells in Fit:",
                       choices = std_ids, selected = default_selected, inline = TRUE)
  })
  
  # --- Standard Curve Regression Engine ---
  standard_curve_model <- reactive({
    df <- bradford_raw_data()
    req(nrow(df) > 0, input$wb_active_standards)
    
    std_df <- df %>% 
      filter(Well_ID %in% input$wb_active_standards, Type == "Standard", !is.na(Conc_Nominal))
    
    if (nrow(std_df) < 3) return(NULL)
    
    # Identify blank (0 nominal conc or minimum standard)
    blank_val <- std_df %>% filter(Conc_Nominal == 0) %>% pull(Mean_OD) %>% mean(na.rm = TRUE)
    if (is.na(blank_val) || is.infinite(blank_val)) blank_val <- min(std_df$Mean_OD, na.rm = TRUE)
    
    std_df <- std_df %>% mutate(Net_OD = Mean_OD - blank_val)
    
    fit <- lm(Net_OD ~ Conc_Nominal, data = std_df)
    slope <- coef(fit)[2]
    intercept <- coef(fit)[1]
    r_squared <- summary(fit)$r.squared
    
    list(
      model = fit,
      slope = unname(slope),
      intercept = unname(intercept),
      r_squared = r_squared,
      blank_val = blank_val,
      std_data = std_df
    )
  })
  
  # --- Render Standard Curve Plot ---
  output$plot_bradford_curve <- renderPlot({
    sc <- standard_curve_model()
    req(sc)
    
    ggplot(sc$std_data, aes(x = Conc_Nominal, y = Net_OD)) +
      geom_point(size = 3.5, color = "#1f4e78") +
      geom_smooth(method = "lm", se = FALSE, color = "#c0392b", linewidth = 1) +
      labs(
        x = "Standard Concentration (µg/µL or mg/mL)",
        y = expression(paste(Delta, "OD (Blank Subtracted)"))
      ) +
      theme_classic(base_size = 13) +
      theme(
        axis.text = element_text(color = "black", face = "bold"),
        axis.title = element_text(face = "bold")
      )
  })
  
  # --- Standard Curve Stats UI ---
  output$bradford_stats_summary_ui <- renderUI({
    sc <- standard_curve_model()
    if (is.null(sc)) return(tags$p("Select at least 3 standard points to build regression."))
    
    tags$div(
      style = "padding: 8px; background: #eef2f7; border-radius: 4px; margin-top: 10px;",
      tags$p(tags$strong("Linear Fit: "), sprintf("Net OD = %.4f × [Conc] + %.4f", sc$slope, sc$intercept)),
      tags$p(tags$strong("Goodness of Fit (R²): "), sprintf("%.4f", sc$r_squared)),
      tags$p(tags$strong("Blank Baseline (OD): "), sprintf("%.4f", sc$blank_val))
    )
  })
  
  # --- Gel Loading Calculations ---
  calculated_recipes <- reactive({
    sc <- standard_curve_model()
    df <- bradford_raw_data()
    req(sc, nrow(df) > 0)
    
    target_ug    <- input$wb_target_protein_ug
    total_vol    <- input$wb_target_volume_ul
    buffer_stock <- as.numeric(input$wb_buffer_stock)
    dil_factor   <- input$wb_sample_dilution_factor
    
    buffer_vol <- round(total_vol / buffer_stock, 1)
    
    unknowns_df <- df %>% filter(Type == "Sample")
    if (nrow(unknowns_df) == 0) return(tibble())
    
    unknowns_df %>% 
      mutate(
        Net_OD        = Mean_OD - sc$blank_val,
        Conc_ug_ul    = round(pmax(0, (Net_OD - sc$intercept) / sc$slope) * dil_factor, 2),
        Sample_Vol_ul = round(target_ug / Conc_ug_ul, 1),
        Buffer_Vol_ul = buffer_vol,
        Water_Vol_ul  = round(pmax(0, total_vol - Sample_Vol_ul - buffer_vol), 1),
        Total_Vol_ul  = total_vol,
        Status = case_when(
          Sample_Vol_ul > (total_vol - buffer_vol) ~ "⚠️ Lysate Too Dilute",
          Conc_ug_ul <= 0 ~ "⚠️ Out of Bounds",
          TRUE ~ "✅ Optimal"
        )
      )
  })
  
  # --- Loading Metrics Summary UI ---
  output$wb_aliquot_metrics_ui <- renderUI({
    recipes <- calculated_recipes()
    if (nrow(recipes) == 0) return(tags$p("No experimental sample lysates found in parsed dataset."))
    
    avg_conc <- mean(recipes$Conc_ug_ul, na.rm = TRUE)
    avg_vol  <- mean(recipes$Sample_Vol_ul[recipes$Status == "✅ Optimal"], na.rm = TRUE)
    
    tagList(
      tags$div(
        style = "margin-top: 10px;",
        tags$p(tags$strong("Unknown Samples Processed: "), nrow(recipes)),
        tags$p(tags$strong("Mean Lysate Concentration: "), sprintf("%.2f µg/µL", avg_conc)),
        tags$p(tags$strong("Mean Lysate Volume / Well: "), sprintf("%.1f µL (for %d µg target)", avg_vol, input$wb_target_protein_ug)),
        tags$p(tags$strong("Laemmli Buffer Volume / Well: "), sprintf("%.1f µL (%sX Stock)", round(input$wb_target_volume_ul / as.numeric(input$wb_buffer_stock), 1), input$wb_buffer_stock))
      )
    )
  })
  
  # --- Loading Recipe DT Table ---
  output$wb_loading_recipe_dt <- renderDT({
    recipes <- calculated_recipes()
    req(nrow(recipes) > 0)
    
    display_df <- recipes %>% 
      select(
        `Sample ID` = Well_ID,
        `Raw OD` = Mean_OD,
        `Conc (µg/µL)` = Conc_ug_ul,
        `Lysate Vol (µL)` = Sample_Vol_ul,
        `Buffer Vol (µL)` = Buffer_Vol_ul,
        `dH2O Vol (µL)` = Water_Vol_ul,
        `Total Vol (µL)` = Total_Vol_ul,
        `Pipetting Status` = Status
      )
    
    datatable(display_df, options = list(pageLength = 10, dom = 'tip'), rownames = FALSE) %>% 
      formatStyle('Pipetting Status', 
                  backgroundColor = styleEqual(c("✅ Optimal", "⚠️ Lysate Too Dilute", "⚠️ Out of Bounds"), 
                                               c("#d4edda", "#fff3cd", "#f8d7da")),
                  fontWeight = 'bold')
  })
  
  # --- Raw Parsed Table ---
  output$wb_raw_parsed_dt <- renderDT({
    df <- bradford_raw_data()
    req(nrow(df) > 0)
    datatable(df, options = list(pageLength = 10, dom = 'tip'), rownames = FALSE)
  })
  
  # --- Antibody Dilution Master Mix Output ---
  output$ab_recipe_output_ui <- renderUI({
    total_vol_ml   <- input$ab_total_volume_ml
    dilution_ratio <- input$ab_dilution_ratio
    buffer_type    <- input$ab_blocking_buffer
    
    ab_vol_ul      <- round((total_vol_ml * 1000) / dilution_ratio, 2)
    buffer_vol_ml  <- total_vol_ml
    
    tagList(
      tags$div(
        style = "padding: 15px; background: #e8f4f8; border-left: 5px solid #17a2b8; border-radius: 4px;",
        tags$h4(tags$strong("Target Dilution: "), sprintf("1 : %s", format(dilution_ratio, big.mark = ","))),
        tags$hr(),
        tags$p(tags$strong("Primary / Secondary Antibody: "), sprintf("%.2f µL", ab_vol_ul)),
        tags$p(tags$strong("Diluent Volume: "), sprintf("%.2f mL (%s)", buffer_vol_ml, buffer_type)),
        tags$p(tags$strong("Total Solution Volume: "), sprintf("%.2f mL", total_vol_ml))
      )
    )
  })
  
  # --- CSV Download Handler ---
  output$download_wb_recipe_csv <- downloadHandler(
    filename = function() { paste0("Western_Loading_Master_Sheet_", Sys.Date(), ".csv") },
    content = function(file) {
      recipes <- calculated_recipes()
      write_csv(recipes, file)
    }
  )
}