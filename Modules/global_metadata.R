# =========================================================================
# GLOBAL METADATA ENGINE & STUDY BLUEPRINT MODULE
# =========================================================================

library(shiny)
library(dplyr)
library(stringr)
library(readr)
library(tibble)

# --- Universal Helper: Trajectory Pooling & Divergence Anchor ---
# Available globally across all assay modules (BP, PWV, Weight, etc.)
prep_longitudinal_trajectory <- function(df, tx_week = 1, pool_baseline = TRUE, 
                                         baseline_label = "Baseline (Pooled)", 
                                         baseline_color = "#34495e") {
  req(df, nrow(df) > 0)
  
  # Ensure SubRun exists safely (default to Run 1 if not present)
  if (!("SubRun" %in% colnames(df))) {
    df$SubRun <- "Run 1"
  }
  
  # Ensure standard numeric indexing
  df_timed <- df %>%
    mutate(
      Week_Num = as.numeric(stringr::str_extract(as.character(Timeline), "-?\\d+")),
      Week_Num = ifelse(is.na(Week_Num), 0, Week_Num),
      Sub_Num  = as.numeric(stringr::str_extract(as.character(SubRun), "\\d+")),
      Sub_Num  = ifelse(is.na(Sub_Num), 1, Sub_Num),
      Time_Index = Week_Num + ((Sub_Num - 1) * 0.2)
    )
  
  color_map <- df_timed %>% distinct(Group, Group_Color)
  assigned_palette <- setNames(color_map$Group_Color, color_map$Group)
  
  if (isTRUE(pool_baseline)) {
    # 1. Baseline Phase (All animals aggregated)
    baseline_summary <- df_timed %>%
      filter(Week_Num < tx_week) %>%
      group_by(Time_Index, Timeline) %>%
      summarise(
        mean_val = mean(Value, na.rm = TRUE),
        sd_val   = if (n() > 1) sd(Value, na.rm = TRUE) else 0,
        .groups  = "drop"
      ) %>%
      mutate(Group = baseline_label, Group_Color = baseline_color, is_anchor = FALSE)
    
    # 2. Treatment Phase (Distinct cohort branches)
    treatment_summary <- df_timed %>%
      filter(Week_Num >= tx_week) %>%
      group_by(Group, Group_Color, Time_Index, Timeline) %>%
      summarise(
        mean_val = mean(Value, na.rm = TRUE),
        sd_val   = if (n() > 1) sd(Value, na.rm = TRUE) else 0,
        .groups  = "drop"
      ) %>%
      mutate(is_anchor = FALSE)
    
    # 3. Anchor Divergence Point (Line-continuity only, no duplicate error bars/points)
    if (nrow(baseline_summary) > 0 && nrow(treatment_summary) > 0) {
      last_baseline <- baseline_summary %>% slice_max(Time_Index, n = 1)
      distinct_groups <- treatment_summary %>% distinct(Group, Group_Color)
      
      anchor_points <- distinct_groups %>%
        mutate(
          Time_Index = last_baseline$Time_Index,
          Timeline   = last_baseline$Timeline,
          mean_val   = last_baseline$mean_val,
          sd_val     = NA_real_,  # Strips SD so duplicate error bars are omitted
          is_anchor  = TRUE       # Used strictly for line drawing
        )
      
      treatment_summary <- bind_rows(anchor_points, treatment_summary) %>%
        arrange(Group, Time_Index)
    }
    
    summary_stats <- bind_rows(baseline_summary, treatment_summary)
    assigned_palette <- c(setNames(baseline_color, baseline_label), assigned_palette)
    
  } else {
    # Independent tracks throughout
    summary_stats <- df_timed %>%
      group_by(Group, Group_Color, Time_Index, Timeline) %>%
      summarise(
        mean_val = mean(Value, na.rm = TRUE),
        sd_val   = if (n() > 1) sd(Value, na.rm = TRUE) else 0,
        .groups  = "drop"
      ) %>%
      mutate(is_anchor = FALSE)
  }
  
  return(list(
    stats = summary_stats,
    palette = assigned_palette,
    tx_week = tx_week,
    is_pooled = isTRUE(pool_baseline)
  ))
}

# --- UI Shared Header Layout Function ---
ui_global_header <- function() {
  wellPanel(
    style = "background-color: #f8f9fa; border: 1px solid #e3e6f0; padding: 15px; margin-bottom: 20px;",
    fluidRow(
      column(5, 
             selectInput("global_project", "Active Research Project Context (Global):", choices = NULL, width = "100%")
      ),
      column(7, 
             tags$label("Study Management & Timeline Controls:"), tags$br(),
             actionButton("open_global_create_modal_btn", "➕ New Study", class = "btn-sm btn-success"),
             actionButton("open_study_config_modal_btn", "⚙️ Configure Timeline", class = "btn-sm btn-primary"),
             actionButton("purge_global_project_btn", "❌ Delete Study", class = "btn-sm btn-danger")
      )
    )
  )
}

# --- Server Metadata Infrastructure Function ---
server_global_metadata <- function(input, output, session, vars) {
  
  meta_filepath           <- "global_metadata_registry.csv"
  pwv_db_filepath         <- "pwv_master_database.csv"
  bp_db_filepath          <- "bp_master_database.csv"
  cannulation_db_filepath <- "cannulation_terminal_database.csv"
  registry_db_filepath    <- "animal_registry.csv"
  weight_db_filepath      <- "animal_weight_database.csv"
  
  # --- 1. Recover Core Metadata Registry System ---
  initial_meta <- if (file.exists(meta_filepath)) {
    read_csv(meta_filepath, col_types = cols(
      Project_Key = col_character(), Project_Label = col_character(),
      Group_Key = col_character(), Group_Color = col_character(), 
      Max_Weeks = col_double(), Tx_Start_Week = col_double(), Pool_Baseline = col_logical()
    ))
  } else {
    tibble(
      Project_Key = character(), Project_Label = character(),
      Group_Key = character(), Group_Color = character(), 
      Max_Weeks = numeric(), Tx_Start_Week = numeric(), Pool_Baseline = logical()
    )
  }
  
  if (nrow(initial_meta) > 0) {
    if (!("Tx_Start_Week" %in% colnames(initial_meta))) initial_meta$Tx_Start_Week <- 1
    if (!("Pool_Baseline" %in% colnames(initial_meta))) initial_meta$Pool_Baseline <- TRUE
  }
  vars$metadata_registry <- reactiveVal(initial_meta)
  
  # --- 2. Recover PWV Database File Records ---
  initial_pwv_db <- if (file.exists(pwv_db_filepath)) {
    read_csv(pwv_db_filepath, col_types = cols(
      Project = col_character(), Rat_ID = col_character(), Group = col_character(),
      Group_Color = col_character(), Timeline = col_character(), SubRun = col_character(),
      Distance_mm = col_double(), Velocity_ms = col_double()
    ))
  } else {
    tibble(Project = character(), Rat_ID = character(), Group = character(),
           Group_Color = character(), Timeline = character(), SubRun = character(),
           Distance_mm = numeric(), Velocity_ms = numeric())
  }
  vars$pwv_historical_data <- reactiveVal(initial_pwv_db)
  
  # --- 3. Recover BP Database File Records ---
  initial_bp_db <- if (file.exists(bp_db_filepath)) {
    read_csv(bp_db_filepath, col_types = cols(
      Project = col_character(), Animal_ID = col_character(), Group = col_character(), 
      Group_Color = col_character(), Timeline = col_character(), SubRun = col_character(), Mean_Systolic = col_double()
    ))
  } else {
    tibble(Project = character(), Animal_ID = character(), Group = character(), 
           Group_Color = character(), Timeline = character(), SubRun = character(), Mean_Systolic = numeric())
  }
  vars$bp_historical_data <- reactiveVal(initial_bp_db)
  
  # --- 4. Recover Surgical Cannulation Data ---
  initial_can_db <- if (file.exists(cannulation_db_filepath)) {
    read_csv(cannulation_db_filepath, col_types = cols(
      Project = col_character(), Animal_ID = col_character(), Group = col_character(), Value = col_double()
    ))
  } else {
    tibble(Project = character(), Animal_ID = character(), Group = character(), Value = numeric())
  }
  vars$cannulation_historical_data <- reactiveVal(initial_can_db)
  
  # --- 5. Recover Animal Weight Database ---
  initial_weight_db <- if (file.exists(weight_db_filepath)) {
    read_csv(weight_db_filepath, col_types = cols(
      Project = col_character(), Animal_ID = col_character(), Group = col_character(),
      Group_Color = col_character(), Timeline = col_character(), SubRun = col_character(),
      Value = col_double()
    ))
  } else {
    tibble(Project = character(), Animal_ID = character(), Group = character(),
           Group_Color = character(), Timeline = character(), SubRun = character(),
           Value = numeric())
  }
  vars$weight_historical_data <- reactiveVal(initial_weight_db)
  
  # --- 6. Recover Centralized Animal Master Registry ---
  initial_registry_db <- if (file.exists(registry_db_filepath)) {
    read_csv(registry_db_filepath, col_types = cols(
      Project = col_character(), Animal_ID = col_character(), Species = col_character(),
      Sex = col_character(), Strain = col_character(), Genotype = col_character(),
      DOB = col_character(), Arrival_Date = col_character(), Cage_ID = col_character(),
      Vendor = col_character(), Cohort_Group = col_character(), IACUC_Protocol = col_character(), 
      Status = col_character(), Biobank_Tissues = col_character(), Custom_Notes = col_character()
    ))
  } else {
    tibble(Project = character(), Animal_ID = character(), Species = character(),
           Sex = character(), Strain = character(), Genotype = character(),
           DOB = character(), Arrival_Date = character(), Cage_ID = character(),
           Vendor = character(), Cohort_Group = character(), IACUC_Protocol = character(), 
           Status = character(), Biobank_Tissues = character(), Custom_Notes = character())
  }
  vars$animal_master_registry <- reactiveVal(initial_registry_db)
  
  vars$group_ui_count <- reactiveVal(1)
  
  # --- Sync Global Project Selector ---
  observe({
    meta <- vars$metadata_registry()
    if (nrow(meta) == 0) {
      updateSelectInput(session, "global_project", choices = c("No Studies Loaded - Create One" = ""))
      return()
    }
    proj_choices <- meta %>% distinct(Project_Label, Project_Key)
    vec_choices <- setNames(proj_choices$Project_Key, proj_choices$Project_Label)
    updateSelectInput(session, "global_project", choices = vec_choices)
  })
  
  # --- Sync Module Selectors Upon Project Change ---
  observeEvent(input$global_project, {
    req(input$global_project)
    meta <- vars$metadata_registry()
    active_subset <- meta %>% filter(Project_Key == input$global_project)
    
    if (nrow(active_subset) > 0) {
      weeks_count <- active_subset$Max_Weeks[1]
      weeks_vector <- paste0("W", 0:weeks_count)
      
      updateSelectizeInput(session, "pwv_group", choices = unique(active_subset$Group_Key))
      updateSelectizeInput(session, "pwv_week", choices = weeks_vector)
      
      updateSelectizeInput(session, "bp_group", choices = unique(active_subset$Group_Key))
      updateSelectizeInput(session, "bp_week", choices = weeks_vector)
    }
  })
  
  # --- Timeline Modal ---
  observeEvent(input$open_study_config_modal_btn, {
    req(input$global_project)
    meta <- vars$metadata_registry() %>% filter(Project_Key == input$global_project)
    req(nrow(meta) > 0)
    
    cur_weeks <- meta$Max_Weeks[1]
    cur_tx    <- if ("Tx_Start_Week" %in% colnames(meta)) meta$Tx_Start_Week[1] else 1
    cur_pool  <- if ("Pool_Baseline" %in% colnames(meta)) meta$Pool_Baseline[1] else TRUE
    
    showModal(modalDialog(
      title = paste0("⚙️ Timeline & Phase Configuration: ", meta$Project_Label[1]),
      size = "m", easyClose = TRUE,
      
      wellPanel(
        style = "background-color: #f8f9fa; border: 1px solid #e3e6f0;",
        numericInput("cfg_max_weeks", "Total Study Duration (Weeks):", value = cur_weeks, min = 1, max = 52),
        hr(),
        checkboxInput("cfg_pool_baseline", "Pool Baseline into Unified Trajectory Line", value = cur_pool),
        numericInput("cfg_tx_week", "Treatment Initiation (Week Num):", value = cur_tx, min = 0, max = 50),
        tags$small(tags$em("Timepoints before this week will be rendered as 'Baseline (Pooled)'. The final baseline point will automatically anchor the diverging treatment curves."))
      ),
      
      footer = tagList(
        modalButton("Cancel"),
        actionButton("save_study_config_btn", "💾 Save Configuration", class = "btn-success")
      )
    ))
  })
  
  observeEvent(input$save_study_config_btn, {
    req(input$global_project)
    
    updated_meta <- vars$metadata_registry() %>%
      mutate(
        Max_Weeks     = if_else(Project_Key == input$global_project, as.numeric(input$cfg_max_weeks), Max_Weeks),
        Tx_Start_Week = if_else(Project_Key == input$global_project, as.numeric(input$cfg_tx_week), Tx_Start_Week),
        Pool_Baseline = if_else(Project_Key == input$global_project, isTRUE(input$cfg_pool_baseline), Pool_Baseline)
      )
    
    vars$metadata_registry(updated_meta)
    write_csv(updated_meta, meta_filepath)
    
    removeModal()
    showNotification("Study timeline settings updated across all modules.", type = "message")
  })
  
  # --- Study Creation Wizard ---
  observeEvent(input$open_global_create_modal_btn, {
    vars$group_ui_count(1)
    showModal(modalDialog(
      title = "➕ Initialize New Research Study Project Blueprint",
      size = "l", easyClose = FALSE,
      
      fluidRow(
        column(6, textInput("wizard_proj_name", "Study/Project Name:", placeholder = "e.g., SHR Cohort 1")),
        column(3, numericInput("wizard_proj_weeks", "Study Duration (Wks):", value = 12, min = 1, max = 52)),
        column(3, numericInput("wizard_tx_week", "Tx Start Week:", value = 1, min = 0, max = 50))
      ),
      checkboxInput("wizard_pool_baseline", "Enable Dynamic Baseline Pooling", value = TRUE),
      hr(),
      tags$h5(tags$strong("Define Project Cohorts & Line Color Palettes")),
      tags$div(id = "wizard_group_rows_container", uiOutput("wizard_dynamic_group_rows")),
      br(),
      actionButton("wizard_add_row_btn", "➕ Add Cohort Group Row", class = "btn-xs btn-primary"),
      
      footer = tags$div(
        modalButton("❌ Cancel Initialization"),
        actionButton("wizard_save_project_btn", "🚀 Finalize and Save Global Study", class = "btn-success")
      )
    ))
  })
  
  output$wizard_dynamic_group_rows <- renderUI({
    count <- vars$group_ui_count()
    lapply(1:count, function(i) {
      default_hex <- c("#2E7D32", "#C62828", "#1565C0", "#7B1FA2", "#E65100")[((i - 1) %% 5) + 1]
      fluidRow(
        id = paste0("wizard_row_", i),
        column(6, textInput(paste0("wizard_group_name_", i), label = paste("Group", i, "Name:"), value = ifelse(i==1, "Control", ""))),
        column(4, 
               tags$label(paste("Group", i, "Color:")), tags$br(),
               tags$input(type = "color", id = paste0("wizard_group_color_", i), value = default_hex,
                          onchange = paste0("Shiny.setInputValue('wizard_group_color_", i, "', this.value);"),
                          style = "width:100%; height:34px; border-radius:4px; border:1px solid #ccc;"),
               tags$script(paste0("Shiny.setInputValue('wizard_group_color_", i, "', document.getElementById('wizard_group_color_", i, "').value);"))
        )
      )
    })
  })
  
  observeEvent(input$wizard_add_row_btn, { vars$group_ui_count(vars$group_ui_count() + 1) })
  
  observeEvent(input$wizard_save_project_btn, {
    req(input$wizard_proj_name)
    clean_key <- str_replace_all(tolower(input$wizard_proj_name), "\\s+", "_")
    count <- vars$group_ui_count()
    
    group_keys <- c()
    group_colors <- c()
    
    for (i in 1:count) {
      g_name <- input[[paste0("wizard_group_name_", i)]]
      g_color <- input[[paste0("wizard_group_color_", i)]]
      if (!is.null(g_name) && g_name != "") {
        group_keys <- c(group_keys, str_trim(g_name))
        group_colors <- c(group_colors, ifelse(is.null(g_color), "#333333", g_color))
      }
    }
    
    new_blueprint_rows <- tibble(
      Project_Key   = clean_key, 
      Project_Label = input$wizard_proj_name,
      Group_Key     = group_keys, 
      Group_Color   = group_colors, 
      Max_Weeks     = input$wizard_proj_weeks,
      Tx_Start_Week = input$wizard_tx_week,
      Pool_Baseline = isTRUE(input$wizard_pool_baseline)
    )
    
    updated_meta <- bind_rows(vars$metadata_registry(), new_blueprint_rows) %>% 
      distinct(Project_Key, Group_Key, .keep_all = TRUE)
    
    vars$metadata_registry(updated_meta)
    write_csv(updated_meta, meta_filepath)
    
    updateSelectInput(session, "global_project", choices = setNames(updated_meta$Project_Key, updated_meta$Project_Label), selected = clean_key)
    removeModal()
  })
  
  # --- Purge Logic ---
  observeEvent(input$purge_global_project_btn, {
    req(input$global_project)
    showModal(modalDialog(
      title = "⚠️ CRITICAL GLOBAL BLANKET DELETION REQUEST",
      tags$p("Are you absolutely certain? This completely purges this project profile layout and deletes ALL saved PWV, Blood Pressure, Cannulation, Weight, and Animal Registry rows associated with it."),
      footer = tags$div(
        modalButton("❌ Cancel Wipe Request"),
        actionButton("confirm_global_purge_btn", "🔥 Yes, Purge Everything", class = "btn-danger")
      )
    ))
  })
  
  observeEvent(input$confirm_global_purge_btn, {
    req(input$global_project)
    
    # Purge Meta
    updated_meta <- vars$metadata_registry() %>% filter(Project_Key != input$global_project)
    vars$metadata_registry(updated_meta)
    write_csv(updated_meta, meta_filepath)
    
    # Purge PWV
    cleared_pwv <- vars$pwv_historical_data() %>% filter(Project != input$global_project)
    vars$pwv_historical_data(cleared_pwv)
    write_csv(cleared_pwv, pwv_db_filepath)
    
    # Purge BP
    cleared_bp <- vars$bp_historical_data() %>% filter(Project != input$global_project)
    vars$bp_historical_data(cleared_bp)
    write_csv(cleared_bp, bp_db_filepath)
    
    # Purge Cannulation
    cleared_can <- vars$cannulation_historical_data() %>% filter(Project != input$global_project)
    vars$cannulation_historical_data(cleared_can)
    write_csv(cleared_can, cannulation_db_filepath)
    
    # Purge Animal Weight
    cleared_wt <- vars$weight_historical_data() %>% filter(Project != input$global_project)
    vars$weight_historical_data(cleared_wt)
    write_csv(cleared_wt, weight_db_filepath)
    
    # Purge Animal Registry
    cleared_reg <- vars$animal_master_registry() %>% filter(Project != input$global_project)
    vars$animal_master_registry(cleared_reg)
    write_csv(cleared_reg, registry_db_filepath)
    
    removeModal()
  })
}