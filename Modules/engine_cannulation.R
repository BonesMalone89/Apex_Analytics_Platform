# =========================================================================
# APEX PLATFORM: ADVANCED SURGICAL CANNULATION & HEMODYNAMICS CORE
# =========================================================================

library(shiny)
library(dplyr)
library(ggplot2)
library(readr)
library(tibble)
library(DT)
library(stringr)

# --- UI Layout Component ---
ui_cannulation_layout <- function() {
  sidebarLayout(
    sidebarPanel(
      tags$h4("Step 1: Session Metadata"),
      fluidRow(
        column(6, textInput("can_rat_id", "Animal ID:", value = "SHR-01")),
        column(6, selectizeInput("can_group_select", "Group Assignment:", choices = NULL))
      ),
      hr(),
      
      tags$h4("Step 2: Log Direct Hemodynamics"),
      fluidRow(
        column(6, numericInput("can_sbp_entry", "Systolic BP (SBP):", value = 200, min = 0, max = 350, step = 0.5)),
        column(6, numericInput("can_dbp_entry", "Diastolic BP (DBP):", value = 150, min = 0, max = 300, step = 0.5))
      ),
      fluidRow(
        column(6, numericInput("can_map_entry", "Mean Arterial (MAP):", value = 175, min = 0, max = 320, step = 0.5)),
        column(6, numericInput("can_hr_entry", "Heart Rate (BPM):", value = 340, min = 100, max = 600, step = 1))
      ),
      fluidRow(
        column(6, numericInput("can_max_dpdt_entry", "Max dP/dt (mmHg/s):", value = 1400, min = 0, max = 5000, step = 10)),
        column(6, numericInput("can_min_dpdt_entry", "Min dP/dt (mmHg/s):", value = -850, min = -5000, max = 0, step = 10))
      ),
      numericInput("can_pti_entry", "Pressure Time Index (PTI):", value = 16.5, min = 0, max = 100, step = 0.1),
      br(),
      actionButton("append_cannulation_btn", "💾 Secure Record to Surgical Track", class = "btn-success btn-block"),
      hr(),
      
      tags$h4("Step 3: CSV Batch Ingestion & Maintenance"),
      fileInput("upload_cannulation_csv", "Ingest Aggregated CSV Batch:", accept = c(".csv")),
      br(),
      actionButton("clear_cannulation_btn", "🧼 Clear Project Cannulation Dataset", class = "btn-danger btn-block")
    ),
    
    mainPanel(
      tabsetPanel(
        type = "pills",
        
        # --- TAB 1: PRIMARY HEMODYNAMIC PROFILE (SBP, DBP, MAP, PP) ---
        tabPanel("📈 Primary Hemodynamic Profile",
                 br(),
                 tags$h4("Primary Cannulation Data (SBP, DBP, MAP, Pulse Pressure)"),
                 tags$p(tags$small(tags$em("Bars indicate Mean \u00b1 SD; points represent individual animals with integrated t-test p-values."))),
                 br(),
                 fluidRow(
                   column(6, div(class = "dash-card", plotOutput("plot_can_sbp", height = "300px"))),
                   column(6, div(class = "dash-card", plotOutput("plot_can_dbp", height = "300px")))
                 ),
                 br(),
                 fluidRow(
                   column(6, div(class = "dash-card", plotOutput("plot_can_map", height = "300px"))),
                   column(6, div(class = "dash-card", plotOutput("plot_can_pp", height = "300px")))
                 ),
                 br(),
                 fluidRow(
                   column(4, downloadButton("download_can_triad_pdf", "Save Hemodynamic Plots (PDF)", class = "btn-xs btn-info")),
                   column(4, downloadButton("download_can_master_dataset", "Export Cannulation CSV", class = "btn-xs btn-default"))
                 ),
                 hr(),
                 tags$h4("📑 Cohort Hemodynamic Summary Matrix"),
                 DTOutput("can_summary_matrix_dt")
        ),
        
        # --- TAB 2: VASCULAR REMODELING & CARDIAC LOAD ---
        tabPanel("🔬 Vascular Remodeling & Load",
                 br(),
                 tags$h4("🧬 Arterial Stiffness, Runoff Recoil & Ventricular Workload"),
                 br(),
                 fluidRow(
                   column(4, div(class = "dash-card", 
                                 tags$h5(tags$strong("Max dP/dt")),
                                 plotOutput("plot_can_max_dpdt", height = "280px"))),
                   column(4, div(class = "dash-card", 
                                 tags$h5(tags$strong("Min dP/dt")),
                                 plotOutput("plot_can_min_dpdt", height = "280px"))),
                   column(4, div(class = "dash-card", 
                                 tags$h5(tags$strong("Myocardial Load (PTI)")),
                                 plotOutput("plot_can_pti", height = "280px")))
                 ),
                 br(),
                 fluidRow(
                   column(12,
                          div(class = "dash-card",
                              tags$h5(tags$strong("MAP vs. Pulse Pressure")),
                              plotOutput("plot_can_bivariate_scatter", height = "340px")
                          )
                   )
                 )
        ),
        
        # --- TAB 3: HISTORICAL LEDGER ---
        tabPanel("🗂️ Historical Entry Database",
                 br(),
                 tags$h4("Active Project Surgical Cannulation Ledger"),
                 DTOutput("cannulation_ledger_dt_table"),
                 br(),
                 actionButton("delete_selected_can_row_btn", "❌ Delete Highlighted Entry from Registry", class = "btn-sm btn-danger")
        ),
        
        # --- TAB 4: ADVANCED STATISTICS ---
        tabPanel("📊 Statistics Panel",
                 br(),
                 wellPanel(
                   style = "background-color: #f8f9fa; border: 1px solid #e3e6f0;",
                   tags$h4(tags$strong("🎛️ Analysis Controls")),
                   fluidRow(
                     column(6, uiOutput("can_stats_cohort_filter_ui")),
                     column(6, selectInput("can_stats_target_metric", "Select Physiological Target for Variance Analysis:",
                                           choices = c("Systolic BP (SBP)" = "SBP",
                                                       "Diastolic BP (DBP)" = "DBP",
                                                       "Mean Arterial Pressure (MAP)" = "MAP",
                                                       "Pulse Pressure (PP)" = "PP",
                                                       "Heart Rate (HR)" = "HR",
                                                       "Inotropy (Max dP/dt)" = "Max_dPdt",
                                                       "Arterial Runoff (Min dP/dt)" = "Min_dPdt",
                                                       "Myocardial Workload (PTI)" = "PTI")))
                   )
                 ),
                 hr(),
                 tags$h4("🧪 Model Summary: Variance Profile (One-Way Global ANOVA)"),
                 DTOutput("can_anova_dt_table"),
                 hr(),
                 tags$h4("📑 Post-Hoc Comparison Matrix (Tukey HSD / Independent t-test)"),
                 DTOutput("can_posthoc_dt_table")
        )
      )
    )
  )
}

# --- Server Logic Component ---
server_cannulation_logic <- function(input, output, session, vars, can_titles = NULL, ...) {
  
  cannulation_db_path <- "cannulation_terminal_database.csv"
  
  # Reactive subset for current active study project
  active_can_project_data <- reactive({
    req(input$global_project)
    df <- vars$cannulation_historical_data()
    if (nrow(df) == 0) return(df)
    
    df %>% 
      filter(trimws(Project) == trimws(input$global_project)) %>% 
      mutate(
        Group = trimws(Group),
        across(any_of(c("SBP", "DBP", "MAP", "PP", "HR", "Max_dPdt", "Min_dPdt", "PTI", "Value")), as.numeric)
      ) %>% 
      mutate(PP = if ("PP" %in% colnames(.) && !all(is.na(PP))) PP else (SBP - DBP))
  })
  
  # Cohort selector dropdown synchronization
  observe({
    meta <- vars$metadata_registry()
    req(input$global_project)
    active_groups <- meta %>% 
      filter(trimws(Project_Key) == trimws(input$global_project)) %>% 
      pull(Group_Key) %>% 
      trimws() %>% 
      unique()
    updateSelectizeInput(session, "can_group_select", choices = active_groups)
  })
  
  # --- Append Single Record ---
  observeEvent(input$append_cannulation_btn, {
    req(input$global_project, input$can_rat_id, input$can_group_select)
    
    sbp_val <- input$can_sbp_entry
    dbp_val <- input$can_dbp_entry
    pp_val  <- sbp_val - dbp_val
    
    new_surgical_row <- tibble(
      Project   = trimws(input$global_project),
      Animal_ID = trimws(input$can_rat_id),
      Group     = trimws(input$can_group_select),
      SBP       = sbp_val,
      DBP       = dbp_val,
      MAP       = input$can_map_entry,
      PP        = pp_val,
      HR        = input$can_hr_entry,
      Max_dPdt  = input$can_max_dpdt_entry,
      Min_dPdt  = input$can_min_dpdt_entry,
      PTI       = input$can_pti_entry
    )
    
    current_db <- vars$cannulation_historical_data()
    updated_db <- bind_rows(current_db, new_surgical_row) %>% 
      group_by(Project, Animal_ID) %>% slice_tail(n = 1) %>% ungroup()
    
    vars$cannulation_historical_data(updated_db)
    write_csv(updated_db, cannulation_db_path)
    
    current_num <- as.numeric(str_extract(input$can_rat_id, "\\d+"))
    if (!is.na(current_num)) {
      updateTextInput(session, "can_rat_id", value = gsub("\\d+", current_num + 1, input$can_rat_id))
    }
    
    showNotification(paste("Direct hemodynamic record logged for", input$can_rat_id), type = "message")
  })
  
  # --- Ingest Aggregated Batch CSV ---
  observeEvent(input$upload_cannulation_csv, {
    req(input$upload_cannulation_csv)
    uploaded_df <- read_csv(input$upload_cannulation_csv$datapath, show_col_types = FALSE)
    
    colnames(uploaded_df) <- trimws(colnames(uploaded_df))
    
    if (!"Project" %in% colnames(uploaded_df)) uploaded_df$Project <- trimws(input$global_project)
    uploaded_df$Project <- trimws(uploaded_df$Project)
    if ("Group" %in% colnames(uploaded_df)) uploaded_df$Group <- trimws(uploaded_df$Group)
    
    if ("Rat_ID" %in% colnames(uploaded_df) && !"Animal_ID" %in% colnames(uploaded_df)) {
      uploaded_df <- uploaded_df %>% rename(Animal_ID = Rat_ID)
    }
    if (!"PP" %in% colnames(uploaded_df) && all(c("SBP", "DBP") %in% colnames(uploaded_df))) {
      uploaded_df <- uploaded_df %>% mutate(PP = SBP - DBP)
    }
    
    current_db <- vars$cannulation_historical_data()
    updated_db <- bind_rows(current_db %>% filter(trimws(Project) != trimws(input$global_project)), uploaded_df)
    
    vars$cannulation_historical_data(updated_db)
    write_csv(updated_db, cannulation_db_path)
    
    showNotification(paste("Ingested", nrow(uploaded_df), "cannulation records successfully."), type = "message")
  })
  
  # --- Delete Highlighted Row ---
  observeEvent(input$delete_selected_can_row_btn, {
    req(input$cannulation_ledger_dt_table_rows_selected)
    df <- active_can_project_data() %>% arrange(Group, Animal_ID)
    row_to_kill <- df[input$cannulation_ledger_dt_table_rows_selected, ]
    
    updated_db <- vars$cannulation_historical_data() %>% 
      filter(!(trimws(Project) == trimws(row_to_kill$Project) & trimws(Animal_ID) == trimws(row_to_kill$Animal_ID)))
    
    vars$cannulation_historical_data(updated_db)
    write_csv(updated_db, cannulation_db_path)
    showNotification("Record excised from database.", type = "warning")
  })
  
  # --- Clear Active Project Dataset ---
  observeEvent(input$clear_cannulation_btn, {
    cleared <- vars$cannulation_historical_data() %>% filter(trimws(Project) != trimws(input$global_project))
    vars$cannulation_historical_data(cleared)
    write_csv(cleared, cannulation_db_path)
    showNotification("Project cannulation dataset cleared.", type = "warning")
  })
  
  # --- Reusable Bar + Jitter Plot Generator with Embedded T-Test Annotation & Right-Side Publication Legend ---
  build_can_bar_plot <- function(metric_col, y_title, plot_title) {
    df <- active_can_project_data()
    req(nrow(df) > 0, metric_col %in% colnames(df))
    
    # Extract metadata colors with trimmed strings
    meta_colors <- vars$metadata_registry() %>% 
      filter(trimws(Project_Key) == trimws(input$global_project)) %>% 
      mutate(Group_Key = trimws(Group_Key), Group_Color = trimws(Group_Color)) %>% 
      distinct(Group_Key, Group_Color)
    
    clean_df <- df %>% 
      filter(!is.na(.data[[metric_col]]), !is.na(Group), Group != "") %>% 
      mutate(Group = trimws(Group))
    req(nrow(clean_df) > 0)
    
    groups_present <- unique(clean_df$Group)
    
    summary_stats <- clean_df %>%
      group_by(Group) %>% 
      summarise(
        mean_val = mean(.data[[metric_col]], na.rm = TRUE),
        sd_val   = ifelse(n() > 1, sd(.data[[metric_col]], na.rm = TRUE), 0),
        .groups  = "drop"
      ) %>% 
      left_join(meta_colors, by = c("Group" = "Group_Key"))
    
    # Build resilient color map: use metadata color, fallback to high-contrast palette if missing
    standard_fallback_colors <- c("#2E7D32", "#C62828", "#1565C0", "#7B1FA2", "#E65100", "#00838F")
    
    palette_vector <- setNames(summary_stats$Group_Color, summary_stats$Group)
    
    # If any group color is NA, fill with standard distinct colors rather than gray
    for (i in seq_along(palette_vector)) {
      if (is.na(palette_vector[i]) || palette_vector[i] == "") {
        palette_vector[i] <- standard_fallback_colors[((i - 1) %% length(standard_fallback_colors)) + 1]
      }
    }
    
    # Calculate global range of all plotted elements
    all_lows  <- c(summary_stats$mean_val - summary_stats$sd_val, clean_df[[metric_col]])
    all_highs <- c(summary_stats$mean_val + summary_stats$sd_val, clean_df[[metric_col]])
    
    global_min <- min(all_lows, na.rm = TRUE)
    global_max <- max(all_highs, na.rm = TRUE)
    data_range <- max(global_max - global_min, 1)
    
    is_negative_metric <- global_max <= 0
    
    # Base Plot Construction
    p <- ggplot() +
      geom_bar(data = summary_stats, aes(x = Group, y = mean_val, fill = Group), 
               stat = "identity", width = 0.35, color = "black", alpha = 0.55) +
      geom_errorbar(data = summary_stats, 
                    aes(x = Group, 
                        ymin = if (is_negative_metric) mean_val - sd_val else pmax(0, mean_val - sd_val), 
                        ymax = if (is_negative_metric) pmin(0, mean_val + sd_val) else mean_val + sd_val), 
                    width = 0.12, linewidth = 0.8, color = "black") +
      geom_point(data = clean_df, aes(x = Group, y = .data[[metric_col]], color = Group), 
                 size = 3.5, position = position_jitter(width = 0.08, height = 0), alpha = 0.9) +
      scale_fill_manual(values = palette_vector, name = "Cohort") +
      scale_color_manual(values = palette_vector, name = "Cohort") +
      labs(title = plot_title, x = NULL, y = y_title) +
      theme_classic(base_size = 13) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        axis.text = element_text(color = "black", face = "bold"),
        legend.position = "right",
        legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(face = "bold", size = 9),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.key = element_rect(fill = "transparent", color = NA)
      )
    
    # Add subtle zero baseline if plotting negative values
    if (is_negative_metric) {
      p <- p + geom_hline(yintercept = 0, color = "gray40", linewidth = 0.6)
    }
    
    # Statistical Annotation Layer (Two-Group Comparison)
    if (length(groups_present) == 2) {
      g1_vals <- clean_df %>% filter(Group == groups_present[1]) %>% pull(all_of(metric_col))
      g2_vals <- clean_df %>% filter(Group == groups_present[2]) %>% pull(all_of(metric_col))
      
      if (length(g1_vals) >= 2 && length(g2_vals) >= 2) {
        ttest_obj <- tryCatch(t.test(g1_vals, g2_vals, var.equal = FALSE), error = function(e) NULL)
        
        if (!is.null(ttest_obj)) {
          pval <- ttest_obj$p.value
          stars <- if (pval < 0.001) "***" else if (pval < 0.01) "**" else if (pval < 0.05) "*" else "ns"
          p_label <- if (pval < 0.001) "p < 0.001 ***" else paste0("p = ", sprintf("%.3f", pval), " ", stars)
          
          # Taller bracket legs and clear label elevation
          y_cap     <- global_max + (0.10 * data_range)
          y_bracket <- global_max + (0.35 * data_range)
          y_text    <- global_max + (0.55 * data_range)
          
          bracket_df <- tibble(
            x = c(1, 1, 2, 2),
            y = c(y_cap, y_bracket, y_bracket, y_cap)
          )
          
          p <- p +
            geom_line(data = bracket_df, aes(x = x, y = y), linewidth = 0.7, color = "black") +
            annotate("text", x = 1.5, y = y_text, label = p_label, fontface = "bold", size = 3.8, color = "black") +
            scale_y_continuous(expand = expansion(mult = if (is_negative_metric) c(0.12, 0.30) else c(0, 0.30)))
          
          return(p)
        }
      }
    }
    
    p <- p + scale_y_continuous(expand = expansion(mult = if (is_negative_metric) c(0.12, 0.12) else c(0, 0.12)))
    return(p)
  }
  
  # --- Render View 1 Plots (Primary Profile) ---
  output$plot_can_sbp <- renderPlot({ build_can_bar_plot("SBP", "Pressure (mmHg)", "Systolic BP (SBP)") })
  output$plot_can_dbp <- renderPlot({ build_can_bar_plot("DBP", "Pressure (mmHg)", "Diastolic BP (DBP)") })
  output$plot_can_map <- renderPlot({ build_can_bar_plot("MAP", "Pressure (mmHg)", "Mean Arterial (MAP)") })
  output$plot_can_pp  <- renderPlot({ build_can_bar_plot("PP", "Pressure (mmHg)", "Pulse Pressure (PP)") })
  
  # --- Render View 2 Plots (Vascular Remodeling & Mechanics) ---
  output$plot_can_max_dpdt <- renderPlot({ build_can_bar_plot("Max_dPdt", "Max dP/dt (mmHg/s)", " ") })
  output$plot_can_min_dpdt <- renderPlot({ build_can_bar_plot("Min_dPdt", "Min dP/dt (mmHg/s)", " ") })
  output$plot_can_pti      <- renderPlot({ build_can_bar_plot("PTI", "PTI (mmHg\u00b7s)", " ") })
  
  # --- Render Bivariate Scatter Plot ---
  output$plot_can_bivariate_scatter <- renderPlot({
    df <- active_can_project_data()
    req(nrow(df) > 0, "MAP" %in% colnames(df), "PP" %in% colnames(df))
    
    meta_colors <- vars$metadata_registry() %>% 
      filter(trimws(Project_Key) == trimws(input$global_project)) %>% 
      mutate(Group_Key = trimws(Group_Key), Group_Color = trimws(Group_Color)) %>% 
      distinct(Group_Key, Group_Color)
    
    palette_vector <- setNames(meta_colors$Group_Color, meta_colors$Group_Key)
    
    ggplot(df, aes(x = MAP, y = PP, color = Group, fill = Group)) +
      geom_point(size = 4, alpha = 0.9) +
      stat_ellipse(geom = "polygon", alpha = 0.15, level = 0.80, type = "t") +
      scale_color_manual(values = palette_vector, name = "Cohort") +
      scale_fill_manual(values = palette_vector, name = "Cohort") +
      labs(
        title = "MAP vs. Pulse Pressure",
        x = "Mean Arterial Pressure (mmHg)",
        y = "Pulse Pressure (mmHg)"
      ) +
      theme_classic(base_size = 14) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text = element_text(color = "black", face = "bold"),
        legend.position = "right",
        legend.title = element_text(face = "bold", size = 11),
        legend.text = element_text(face = "bold", size = 10)
      )
  })
  
  # --- Summary Matrix Table ---
  output$can_summary_matrix_dt <- renderDT({
    df <- active_can_project_data()
    req(nrow(df) > 0)
    
    metrics_present <- intersect(c("SBP", "DBP", "MAP", "PP", "HR", "Max_dPdt", "Min_dPdt", "PTI"), colnames(df))
    
    summary_df <- df %>% 
      group_by(Group) %>% 
      summarise(
        `N` = n(),
        across(all_of(metrics_present), list(
          Mean = ~ round(mean(.x, na.rm = TRUE), 1),
          SD   = ~ round(sd(.x, na.rm = TRUE), 1)
        )),
        .groups = "drop"
      )
    
    datatable(summary_df, options = list(dom = 't', pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })
  
  # --- Ledger Table ---
  output$cannulation_ledger_dt_table <- renderDT({
    df <- active_can_project_data()
    req(nrow(df) > 0)
    datatable(df %>% arrange(Group, Animal_ID), selection = 'single', filter = 'top', 
              options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # --- Statistics Filters & Engines ---
  output$can_stats_cohort_filter_ui <- renderUI({
    df <- active_can_project_data()
    if (nrow(df) == 0) return(NULL)
    checkboxGroupInput("can_stats_groups", "Select Cohorts for Test Pool:",
                       choices = unique(df$Group), selected = unique(df$Group), inline = TRUE)
  })
  
  # Global ANOVA Engine
  output$can_anova_dt_table <- renderDT({
    df <- active_can_project_data()
    req(input$can_stats_groups, input$can_stats_target_metric)
    
    filtered_df <- df %>% 
      filter(Group %in% input$can_stats_groups, !is.na(.data[[input$can_stats_target_metric]]))
    
    if (length(unique(filtered_df$Group)) < 2 || nrow(filtered_df) < 4) {
      return(datatable(tibble(`Notice` = "Select at least 2 active cohort check-boxes with sufficient data."), options = list(dom = 't'), rownames = FALSE))
    }
    
    anova_formula <- as.formula(paste(input$can_stats_target_metric, "~ Group"))
    anova_model   <- summary(aov(anova_formula, data = filtered_df))[[1]]
    
    anova_tibble <- tibble(
      `Source of Variation`     = c("Experimental Cohort Group", "Residual Error (Within Variance)"),
      `Degrees of Freedom (Df)` = anova_model$Df,
      `Sum of Squares (SS)`     = round(anova_model$`Sum Sq`, 2),
      `Mean Squares (MS)`       = round(anova_model$`Mean Sq`, 2),
      `F-Statistic`             = c(round(anova_model$`F value`[1], 2), NA),
      `P-Value Magnitude`       = c(anova_model$`Pr(>F)`[1], NA)
    )
    
    datatable(anova_tibble, options = list(dom = 't', ordering = FALSE), rownames = FALSE) %>% 
      formatStyle('P-Value Magnitude', backgroundColor = styleInterval(c(0.05), c('#d4edda', 'transparent')), fontWeight = styleInterval(c(0.05), c('bold', 'normal'))) %>% 
      formatSignif(columns = c('P-Value Magnitude'), digits = 4)
  })
  
  # Post-Hoc / T-Test Engine
  output$can_posthoc_dt_table <- renderDT({
    df <- active_can_project_data()
    req(input$can_stats_groups, input$can_stats_target_metric)
    
    filtered_df <- df %>% 
      filter(Group %in% input$can_stats_groups, !is.na(.data[[input$can_stats_target_metric]]))
    
    if (length(unique(filtered_df$Group)) < 2 || nrow(filtered_df) < 3) {
      return(datatable(tibble(`Notice` = "Select at least 2 groups with active data."), options = list(dom = 't'), rownames = FALSE))
    }
    
    target_metric <- input$can_stats_target_metric
    
    if (length(unique(filtered_df$Group)) == 2) {
      groups_vec <- unique(filtered_df$Group)
      g1_data <- filtered_df %>% filter(Group == groups_vec[1]) %>% pull(all_of(target_metric))
      g2_data <- filtered_df %>% filter(Group == groups_vec[2]) %>% pull(all_of(target_metric))
      
      ttest_res <- t.test(g1_data, g2_data, var.equal = FALSE)
      
      two_group_stats <- tibble(
        `Comparison Factor`  = paste(groups_vec[1], "vs", groups_vec[2]),
        `Mean 1 (SD)`        = paste0(round(mean(g1_data), 1), " ± ", round(sd(g1_data), 1)),
        `Mean 2 (SD)`        = paste0(round(mean(g2_data), 1), " ± ", round(sd(g2_data), 1)),
        `Delta Difference`   = round(mean(g2_data) - mean(g1_data), 2),
        `t-Statistic`        = round(ttest_res$statistic, 3),
        `Degrees of Freedom` = round(ttest_res$parameter, 1),
        `Welch P-Value`      = ttest_res$p.value
      )
      
      return(
        datatable(two_group_stats, options = list(dom = 't'), rownames = FALSE) %>% 
          formatStyle('Welch P-Value', backgroundColor = styleInterval(c(0.05), c('#d4edda', 'transparent')), fontWeight = styleInterval(c(0.05), c('bold', 'normal'))) %>% 
          formatSignif(columns = c('Welch P-Value'), digits = 4)
      )
    } else {
      anova_formula <- as.formula(paste(target_metric, "~ Group"))
      tukey_matrix  <- TukeyHSD(aov(anova_formula, data = filtered_df), "Group")$Group
      tukey_tibble  <- as_tibble(tukey_matrix, rownames = "Pairwise Cohort Comparison") %>% 
        transmute(
          `Pairwise Comparison` = `Pairwise Cohort Comparison`, 
          `Mean Difference`     = round(diff, 2), 
          `Lower 95% CI`        = round(lwr, 2), 
          `Upper 95% CI`        = round(upr, 2), 
          `Adjusted P-Value`    = `p adj`
        )
      
      return(
        datatable(tukey_tibble, options = list(dom = 't'), rownames = FALSE) %>% 
          formatStyle('Adjusted P-Value', backgroundColor = styleInterval(c(0.05), c('#d4edda', 'transparent')), fontWeight = styleInterval(c(0.05), c('bold', 'normal'))) %>% 
          formatSignif(columns = c('Adjusted P-Value'), digits = 4)
      )
    }
  })
  
  # --- Downloads ---
  output$download_can_master_dataset <- downloadHandler(
    filename = function() { paste0("Cannulation_Data_", input$global_project, "_", Sys.Date(), ".csv") },
    content  = function(file) { write_csv(active_can_project_data(), file) }
  )
  
  output$download_can_triad_pdf <- downloadHandler(
    filename = function() { paste0("Hemodynamic_Profile_", input$global_project, "_", Sys.Date(), ".pdf") },
    content  = function(file) {
      pdf(file, width = 11, height = 8)
      gridExtra::grid.arrange(
        build_can_bar_plot("SBP", "Pressure (mmHg)", "Systolic BP (SBP)"),
        build_can_bar_plot("DBP", "Pressure (mmHg)", "Diastolic BP (DBP)"),
        build_can_bar_plot("MAP", "Pressure (mmHg)", "Mean Arterial (MAP)"),
        build_can_bar_plot("PP", "Pressure (mmHg)", "Pulse Pressure (PP)"),
        ncol = 2, nrow = 2
      )
      dev.off()
    }
  )
}