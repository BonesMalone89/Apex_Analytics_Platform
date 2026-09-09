# =========================================================================
# APEX PLATFORM: LONGITUDINAL WEIGHT MONITORING & GROWTH ANALYSIS ENGINE
# =========================================================================

# --- UI Layout Component ---
ui_weight_layout <- function() {
  sidebarLayout(
    sidebarPanel(
      tags$h4("Step 1: Session Parameters"),
      selectizeInput("weight_group", "Group Assignment:", choices = NULL),
      fluidRow(
        column(6, selectizeInput("weight_animal_id", "Subject ID:", choices = c("Select ID..." = ""))),
        column(6, selectizeInput("weight_week", "Timeline Point:", choices = NULL))
      ),
      hr(),
      
      tags$h4("Step 2: Log Weight Point"),
      numericInput("weight_grams", "Mass (Grams):", value = 300, min = 0, step = 0.1),
      dateInput("weight_log_date", "Measurement Date:", value = Sys.Date()),
      textAreaInput("weight_notes", "Observational Notes:", rows = 2, placeholder = "e.g., Active, normal grooming..."),
      
      br(),
      actionButton("append_weight_btn", "💾 Append to Historical Dataset", class = "btn-success btn-block"),
      hr(),
      
      tags$h4("Step 3: Graph Display Bounds"),
      # Converted to server-side UI outputs to preserve limits without reverting on boot
      uiOutput("weight_bounds_ui_controls"),
      br(),
      actionButton("open_weight_graph_modal_btn", "🎨 Customize Graph Titles", class = "btn-xs btn-default btn-block"),
      br(),
      actionButton("warn_clear_weight_btn", "🧼 Clear Project Weight Dataset", class = "btn-danger btn-block")
    ),
    
    mainPanel(
      tabsetPanel(
        type = "pills",
        tabPanel("📈 Weight Gain Trends",
                 br(),
                 fluidRow(
                   column(5,
                          tags$h4("📋 Session Processing Summary"),
                          verbatimTextOutput("weight_session_summary_box")
                   ),
                   column(7,
                          div(class = "dash-card",
                          tags$h4("📈 Long-Term Weight Trends"),
                          plotOutput("weight_historical_trend_plot", height = "320px"),
                          ),
                          br(),
                          fluidRow(
                            column(4, downloadButton("download_weight_master_dataset", "Export CSV", class = "btn-xs")),
                            column(4, downloadButton("download_weight_plot_pdf", "Save PDF", class = "btn-xs btn-info")),
                            column(4, downloadButton("download_weight_plot_png", "Save PNG", class = "btn-xs btn-info"))
                          )
                   )
                 ),
                 hr(),
                 tags$h4("🗂️Historical Entry Database"),
                 tags$p(tags$small(tags$em("Click on any row below and press the red button to delete an experimental record error."))),
                 DTOutput("weight_master_database_view_table"),
                 br(),
                 actionButton("delete_selected_weight_row_btn", "❌ Delete Highlighted Entry from Registry", class = "btn-sm btn-danger")
        ),
        tabPanel("📊 Statistics Panel",
                 br(),
                 wellPanel(
                   style = "background-color: #f8f9fa; border: 1px solid #e3e6f0;",
                   tags$h4(tags$strong("🎛️ Analysis Controls")),
                   uiOutput("weight_stats_time_filter_ui")
                 ),
                 hr(),
                 tags$h4("🧪 Model Summary: Variance Profile (Two-Way Global ANOVA)"),
                 tags$p(tags$small(tags$em("Evaluates selected cohorts dynamically across the entire longitudinal timeline."))),
                 DTOutput("weight_anova_dt_table"),
                 hr(),
                 tags$h4("📑 Post-Hoc Comparison Matrix (Isolated Tukey's HSD / t-test)"),
                 tags$p(tags$small(tags$em("Evaluates selected cohorts isolated purely at the single temporal point chosen above."))),
                 DTOutput("weight_posthoc_dt_table")
        )
      )
    )
  )
}

# --- Server Logic Function ---
server_weight_logic <- function(input, output, session, vars, weight_titles) {
  
  weight_db_path <- "animal_weight_database.csv"
  
  # --- Initialize Weight Database ---
  initial_weight_db <- if (file.exists(weight_db_path)) {
    read_csv(weight_db_path, col_types = cols(
      Project = col_character(), Animal_ID = col_character(), Group = col_character(),
      Group_Color = col_character(), Timeline = col_character(), Mass_g = col_double(),
      Log_Date = col_character(), Notes = col_character()
    ))
  } else {
    tibble(Project = character(), Animal_ID = character(), Group = character(),
           Group_Color = character(), Timeline = character(), Mass_g = numeric(),
           Log_Date = character(), Notes = character())
  }
  vars$weight_historical_data <- reactiveVal(initial_weight_db)
  
  # Persistent memory holders for limits across application reboots
  reactive_ymin <- reactiveVal(150)
  reactive_ymax <- reactiveVal(500)
  
  observe({
    if(!is.null(input$weight_ymin)) reactive_ymin(input$weight_ymin)
    if(!is.null(input$weight_ymax)) reactive_ymax(input$weight_ymax)
  })
  
  output$weight_bounds_ui_controls <- renderUI({
    fluidRow(
      column(6, numericInput("weight_ymin", "Y-Min Limit (g):", value = 50, min = 0, max = 1000, step = 10)),
      column(6, numericInput("weight_ymax", "Y-Max Limit (g):", value = 450, min = 50, max = 1000, step = 10))
    )
  })
  
  weight_project_data <- reactive({ 
    vars$weight_historical_data() %>% filter(Project == input$global_project) 
  })
  
  observe({
    meta <- vars$metadata_registry() %>% filter(Project_Key == input$global_project)
    if(nrow(meta) > 0) {
      updateSelectizeInput(session, "weight_group", choices = unique(meta$Group_Key))
      max_w <- meta$Max_Weeks[1]
      weeks_vec <- paste("Week", 1:max_w)
      updateSelectizeInput(session, "weight_week", choices = c("Baseline/Arrival", weeks_vec, "Pre-Op", "Post-Op", "Terminal Harvest"))
    }
  })
  
  observe({
    df <- vars$animal_master_registry()
    req(input$weight_group)
    if (!is.null(df) && nrow(df) > 0) {
      active_scope <- df %>% filter(Project == input$global_project & Status == "Active" & Cohort_Group == input$weight_group)
      updateSelectizeInput(session, "weight_animal_id", choices = sort(unique(active_scope$Animal_ID)), server = TRUE)
    }
  })
  
  observeEvent(input$open_weight_graph_modal_btn, {
    showModal(modalDialog(
      title = "🎨 Weight Title Labels Customizer",
      textInput("modal_weight_title", "Custom Graph Title:", value = weight_titles$title),
      textInput("modal_weight_xlabel", "X-Axis Label:", value = weight_titles$xlab),
      textInput("modal_weight_ylabel", "Y-Axis Label:", value = weight_titles$ylab),
      actionButton("save_weight_labels_btn", "💾 Apply Text Updates", class = "btn-block btn-success"),
      easyClose = TRUE, footer = modalButton("Dismiss")
    ))
  })
  
  observeEvent(input$save_weight_labels_btn, {
    weight_titles$title <- input$modal_weight_title
    weight_titles$xlab  <- input$modal_weight_xlabel
    weight_titles$ylab  <- input$modal_weight_ylabel 
    removeModal()
  })
  
  output$weight_session_summary_box <- renderText({
    req(input$weight_animal_id, input$weight_grams)
    paste0("Target Group Assignment: [", input$weight_group, "]\n",
           "Subject Selected: ", input$weight_animal_id, "\n",
           "Timeline Point: ", input$weight_week, "\n",
           "Input Metric: ", input$weight_grams, " g\n",
           "Log Date: ", input$weight_log_date)
  })
  
  observeEvent(input$append_weight_btn, {
    req(input$weight_animal_id, input$weight_grams, input$weight_week)
    meta <- vars$metadata_registry()
    target_hex <- meta %>% filter(Project_Key == input$global_project & Group_Key == input$weight_group) %>% pull(Group_Color) %>% first()
    if(is.null(target_hex) || is.na(target_hex)) target_hex <- "#333333"
    
    new_row <- tibble(
      Project = input$global_project,
      Animal_ID = input$weight_animal_id,
      Group = input$weight_group,
      Group_Color = target_hex,
      Timeline = input$weight_week,
      Mass_g = as.numeric(input$weight_grams),
      Log_Date = as.character(input$weight_log_date),
      Notes = ifelse(is.na(input$weight_notes) || input$weight_notes == "", "None", input$weight_notes)
    )
    
    updated_db <- bind_rows(vars$weight_historical_data(), new_row) %>%
      group_by(Project, Animal_ID, Group, Timeline) %>% slice_tail(n = 1) %>% ungroup()
    
    vars$weight_historical_data(updated_db)
    write_csv(updated_db, weight_db_path)
    updateTextAreaInput(session, "weight_notes", value = "")
    showNotification(paste("Securely appended mass metrics for", input$weight_animal_id), type = "message")
  })
  
  reactive_weight_plot <- reactive({
    df <- weight_project_data()
    req(nrow(df) > 0, input$weight_ymin, input$weight_ymax)
    
    meta <- vars$metadata_registry() %>% filter(Project_Key == input$global_project)
    weeks_count <- if(nrow(meta) > 0) meta$Max_Weeks[1] else 12
    milestone_order <- c("Baseline/Arrival", paste("Week", 1:weeks_count), "Pre-Op", "Post-Op", "Terminal Harvest")
    
    summary_stats <- df %>%
      filter(Timeline %in% milestone_order) %>%
             mutate(Timeline_Factor = factor(Timeline, levels = milestone_order, ordered = TRUE)) %>%
               group_by(Group, Group_Color, Timeline_Factor, Timeline) %>%
               summarise(mean_wt = mean(Mass_g), sd_wt = ifelse(n() > 1, sd(Mass_g), 0), .groups = "drop")
             
             assigned_palette <- setNames(summary_stats$Group_Color, summary_stats$Group)
             
             ggplot(summary_stats, aes(x = Timeline_Factor, y = mean_wt, color = Group, group = Group)) +
               geom_line(linewidth = 1.3) +
               geom_errorbar(aes(ymin = mean_wt - sd_wt, ymax = mean_wt + sd_wt), width = 0.15, linewidth = 0.8) +
               geom_point(size = 3.5) +
               scale_color_manual(values = assigned_palette) +
               coord_cartesian(ylim = c(input$weight_ymin, input$weight_ymax)) +
               labs(title = weight_titles$title, x = weight_titles$xlab, y = weight_titles$ylab, color = "Group", caption = "Data points represent Mean ± SD") +
               theme_classic(base_size = 14) +
               theme(axis.text.x = element_text(angle = 30, hjust = 1, face = "bold", color = "black"), axis.text.y = element_text(color = "black"), plot.title = element_text(hjust = 0.5, face = "bold"), plot.caption = element_text(size = 10, face = "italic", color = "gray30"))
  })
    
    output$weight_historical_trend_plot <- renderPlot({ reactive_weight_plot() })
    
    output$weight_master_database_view_table <- renderDT({
      df <- weight_project_data()
      req(nrow(df) > 0)
      datatable(df %>% arrange(Group, Timeline, Animal_ID) %>%
                  select(`Cohort Group` = Group, `Animal ID` = Animal_ID, `Study Point` = Timeline, `Mass (g)` = Mass_g, `Log Date` = Log_Date, Notes),
                selection = 'single', filter = 'top', options = list(pageLength = 5, autoWidth = TRUE))
    })
    
    observeEvent(input$delete_selected_weight_row_btn, {
      req(input$weight_master_database_view_table_rows_selected)
      df <- weight_project_data() %>% arrange(Group, Timeline, Animal_ID)
      row_to_kill <- df[input$weight_master_database_view_table_rows_selected, ]
      
      updated_db <- vars$weight_historical_data() %>% 
        filter(!(Project == row_to_kill$Project & Animal_ID == row_to_kill$Animal_ID & Group == row_to_kill$Group & Timeline == row_to_kill$Timeline))
      
      vars$weight_historical_data(updated_db)
      write_csv(updated_db, weight_db_path)
      showNotification("Target mass entry surgically dropped from database tracking ledger.", type = "warning")
    })
    
    output$weight_stats_time_filter_ui <- renderUI({
      df <- weight_project_data()
      if(nrow(df) == 0) return(NULL)
      tagList(
        selectInput("weight_stats_week", "Isolate Analysis Point:", choices = sort(unique(df$Timeline)), selected = max(df$Timeline), width = "100%"),
        checkboxGroupInput("weight_stats_groups", "Select Cohorts to Include:", choices = unique(df$Group), selected = unique(df$Group), inline = TRUE)
      )
    })
    
    output$weight_anova_dt_table <- renderDT({
      df <- weight_project_data()
      req(input$weight_stats_groups)
      filtered_df <- df %>% filter(Group %in% input$weight_stats_groups)
      if (length(unique(filtered_df$Group)) < 2 || length(unique(filtered_df$Timeline)) < 2 || nrow(filtered_df) < 4) {
        return(datatable(tibble(`Notice` = "Requires at least 2 groups and 2 timepoints."), options = list(dom = 't'), rownames = FALSE))
      }
      anova_model <- summary(aov(Mass_g ~ Group * Timeline, data = filtered_df))[[1]]
      anova_tibble <- tibble(`Source of Variation` = c("Group", "Timeline", "Interaction", "Residuals"), `Df` = anova_model$Df, `SS` = round(anova_model$`Sum Sq`, 2), `MS` = round(anova_model$`Mean Sq`, 2), `F-Statistic` = round(anova_model$`F value`, 2), `P-Value Magnitude` = anova_model$`Pr(>F)`)
      datatable(anova_tibble, options = list(dom = 't', ordering = FALSE), rownames = FALSE) %>%
        formatStyle('P-Value Magnitude', backgroundColor = styleInterval(c(0.05), c('#d4edda', 'transparent')), fontWeight = styleInterval(c(0.05), c('bold', 'normal'))) %>%
        formatSignif(columns = c('P-Value Magnitude'), digits = 4)
    })
    
    output$weight_posthoc_dt_table <- renderDT({
      df <- weight_project_data()
      req(input$weight_stats_week, input$weight_stats_groups)
      snapshot_df <- df %>% filter(Timeline == input$weight_stats_week, Group %in% input$weight_stats_groups)
      if (length(unique(snapshot_df$Group)) < 2 || nrow(snapshot_df) < 3) {
        return(datatable(tibble(`Notice` = "Select at least 2 groups with active data."), options = list(dom = 't'), rownames = FALSE))
      }
      if (length(unique(snapshot_df$Group)) == 2) {
        groups_vec <- unique(snapshot_df$Group)
        t_res <- t.test(snapshot_df$Mass_g[snapshot_df$Group == groups_vec[1]], snapshot_df$Mass_g[snapshot_df$Group == groups_vec[2]], var.equal = TRUE)
        datatable(tibble(`Test` = "Independent t-test", `df` = round(t_res$parameter, 1), `p-value` = t_res$p.value), options = list(dom = 't'), rownames = FALSE) %>%
          formatStyle('p-value', backgroundColor = styleInterval(c(0.05), c('#d4edda', 'transparent')), fontWeight = styleInterval(c(0.05), c('bold', 'normal'))) %>%
          formatSignif(columns = c('p-value'), digits = 4)
      } else {
        tukey_matrix <- TukeyHSD(aov(Mass_g ~ Group, data = snapshot_df), "Group")$Group
        tukey_tibble <- as_tibble(tukey_matrix, rownames = "Comparison") %>% transmute(`Pairwise Comparison` = Comparison, `Mean Diff` = round(diff, 2), `Adjusted P-Value` = `p adj`)
        datatable(tukey_tibble, options = list(dom = 't'), rownames = FALSE) %>%
          formatStyle('Adjusted P-Value', backgroundColor = styleInterval(c(0.05), c('#d4edda', 'transparent')), fontWeight = styleInterval(c(0.05), c('bold', 'normal'))) %>%
          formatSignif(columns = c('Adjusted P-Value'), digits = 4)
      }
    })
}

# --- Server Logic Function ---
server_weight_logic <- function(input, output, session, vars, weight_titles) {
  
  weight_db_path <- "animal_weight_database.csv"
  
  # --- Initialize Weight Database ---
  initial_weight_db <- if (file.exists(weight_db_path)) {
    read_csv(weight_db_path, col_types = cols(
      Project = col_character(), Animal_ID = col_character(), Group = col_character(),
      Group_Color = col_character(), Timeline = col_character(), Mass_g = col_double(),
      Log_Date = col_character(), Notes = col_character()
    ))
  } else {
    tibble(Project = character(), Animal_ID = character(), Group = character(),
           Group_Color = character(), Timeline = character(), Mass_g = numeric(),
           Log_Date = character(), Notes = character())
  }
  vars$weight_historical_data <- reactiveVal(initial_weight_db)
  
  # Filter weight stream strictly down to active global project scope
  weight_project_data <- reactive({ 
    vars$weight_historical_data() %>% filter(Project == input$global_project) 
  })
  
  # --- Sync Configuration Dropdowns from Global Metadata ---
  observe({
    meta <- vars$metadata_registry() %>% filter(Project_Key == input$global_project)
    if(nrow(meta) > 0) {
      updateSelectizeInput(session, "weight_group", choices = unique(meta$Group_Key))
      
      # Build timeline choices matching project bounds
      max_w <- meta$Max_Weeks[1]
      weeks_vec <- paste("Week", 1:max_w)
      updateSelectizeInput(session, "weight_week", choices = c("Baseline/Arrival", weeks_vec, "Pre-Op", "Post-Op", "Terminal Harvest"))
    }
  })
  
  # --- Monitor Selected Group to Filter Available Animal IDs ---
  observe({
    df <- vars$animal_master_registry()
    req(input$weight_group)
    if (!is.null(df) && nrow(df) > 0) {
      active_scope <- df %>% filter(Project == input$global_project & Status == "Active" & Cohort_Group == input$weight_group)
      updateSelectizeInput(session, "weight_animal_id", choices = sort(unique(active_scope$Animal_ID)), server = TRUE)
    }
  })
  
  # --- Graph Title Customizer Pop-up ---
  observeEvent(input$open_weight_graph_modal_btn, {
    showModal(modalDialog(
      title = "🎨 Weight Title Labels Customizer",
      textInput("modal_weight_title", "Custom Graph Title:", value = weight_titles$title),
      textInput("modal_weight_xlabel", "X-Axis Label:", value = weight_titles$xlab),
      textInput("modal_weight_ylabel", "Y-Axis Label:", value = weight_titles$ylab),
      actionButton("save_weight_labels_btn", "💾 Apply Text Updates", class = "btn-block btn-success"),
      easyClose = TRUE, footer = modalButton("Dismiss")
    ))
  })
  
  observeEvent(input$save_weight_labels_btn, {
    weight_titles$title <- input$modal_weight_title
    weight_titles$xlab  <- input$modal_weight_xlabel
    weight_titles$ylab  <- input$modal_weight_ylabel 
    removeModal()
  })
  
  # --- UI Summary Box Output ---
  output$weight_session_summary_box <- renderText({
    req(input$weight_animal_id, input$weight_grams)
    paste0("Target Group Assignment: [", input$weight_group, "]\n",
           "Subject Selected: ", input$weight_animal_id, "\n",
           "Timeline Point: ", input$weight_week, "\n",
           "Input Metric: ", input$weight_grams, " g\n",
           "Log Date: ", input$weight_log_date)
  })
  
  # --- Append Record Button Core Handler ---
  observeEvent(input$append_weight_btn, {
    req(input$weight_animal_id, input$weight_grams, input$weight_week)
    
    meta <- vars$metadata_registry()
    target_hex <- meta %>% filter(Project_Key == input$global_project & Group_Key == input$weight_group) %>% pull(Group_Color) %>% first()
    if(is.null(target_hex) || is.na(target_hex)) target_hex <- "#333333"
    
    new_row <- tibble(
      Project = input$global_project,
      Animal_ID = input$weight_animal_id,
      Group = input$weight_group,
      Group_Color = target_hex,
      Timeline = input$weight_week,
      Mass_g = as.numeric(input$weight_grams),
      Log_Date = as.character(input$weight_log_date),
      Notes = ifelse(is.na(input$weight_notes) || input$weight_notes == "", "None", input$weight_notes)
    )
    
    # Overwrite if an animal already has a record for that exact timeline point
    updated_db <- bind_rows(vars$weight_historical_data(), new_row) %>%
      group_by(Project, Animal_ID, Group, Timeline) %>% slice_tail(n = 1) %>% ungroup()
    
    vars$weight_historical_data(updated_db)
    write_csv(updated_db, weight_db_path)
    
    updateTextAreaInput(session, "weight_notes", value = "")
    showNotification(paste("Securely appended mass metrics for", input$weight_animal_id), type = "message")
  })
  
  reactive_weight_plot <- reactive({
    df <- weight_project_data()
    req(nrow(df) > 0)
    
    # 1. Pull active project metadata
    meta <- vars$metadata_registry() %>% filter(Project_Key == input$global_project)
    req(nrow(meta) > 0)
    
    weeks_count <- if (!is.na(meta$Max_Weeks[1])) meta$Max_Weeks[1] else 12
    tx_week     <- if ("Tx_Start_Week" %in% colnames(meta) && !is.na(meta$Tx_Start_Week[1])) meta$Tx_Start_Week[1] else 1
    pool_active <- if ("Pool_Baseline" %in% colnames(meta) && !is.na(meta$Pool_Baseline[1])) meta$Pool_Baseline[1] else TRUE
    
    # 2. Map discrete milestone text to standardized timeline string
    df_input <- df %>%
      mutate(
        Value = Mass_g,
        Timeline_Normalized = case_when(
          grepl("(?i)base|arrival", Timeline) ~ "W0",
          grepl("(?i)week", Timeline) ~ paste0("W", stringr::str_extract(Timeline, "\\d+")),
          grepl("(?i)pre-?op", Timeline) ~ paste0("W", weeks_count + 1),
          grepl("(?i)post-?op", Timeline) ~ paste0("W", weeks_count + 2),
          grepl("(?i)terminal|harvest", Timeline) ~ paste0("W", weeks_count + 3),
          TRUE ~ Timeline
        )
      ) %>%
      mutate(Timeline = Timeline_Normalized)
    
    # 3. Call universal helper
    traj <- prep_longitudinal_trajectory(
      df = df_input,
      tx_week = tx_week,
      pool_baseline = pool_active,
      baseline_label = "Baseline (Pooled)",
      baseline_color = "#34495e"
    )
    
    # 4. Dynamic Auto-Bounds (Never cut off data points)
    calc_min <- min(traj$stats$mean_val - traj$stats$sd_val, na.rm = TRUE)
    calc_max <- max(traj$stats$mean_val + traj$stats$sd_val, na.rm = TRUE)
    
    ymin_val <- if (!is.null(input$weight_ymin) && !is.na(input$weight_ymin)) {
      as.numeric(input$weight_ymin)
    } else {
      max(0, floor(calc_min * 0.9 / 10) * 10) # 10% bottom padding, floored
    }
    
    ymax_val <- if (!is.null(input$weight_ymax) && !is.na(input$weight_ymax)) {
      as.numeric(input$weight_ymax)
    } else {
      ceiling(calc_max * 1.1 / 10) * 10      # 10% top padding, ceilinged
    }
    
    # 5. Format X-axis breaks
    unique_indices <- sort(unique(traj$stats$Time_Index))
    
    format_label <- function(idx) {
      if (idx == 0) return("Baseline")
      if (idx > 0 && idx <= weeks_count) return(paste0("W", idx))
      if (idx == weeks_count + 1) return("Pre-Op")
      if (idx == weeks_count + 2) return("Post-Op")
      if (idx == weeks_count + 3) return("Terminal")
      return(paste0("W", idx))
    }
    
    x_labels <- sapply(unique_indices, format_label)
    
    # 6. Build ggplot
    p <- ggplot(traj$stats, aes(x = Time_Index, y = mean_val, color = Group, group = Group)) +
      geom_line(data = filter(traj$stats, !is.na(mean_val)), linewidth = 1.3) +
      geom_errorbar(aes(ymin = mean_val - sd_val, ymax = mean_val + sd_val), width = 0.15, linewidth = 0.8) +
      geom_point(size = 3.5) +
      scale_x_continuous(breaks = unique_indices, labels = x_labels) +
      scale_color_manual(values = traj$palette) +
      coord_cartesian(ylim = c(ymin_val, ymax_val)) +
      labs(
        title = weight_titles$title,
        x = weight_titles$xlab,
        y = weight_titles$ylab,
        color = "Group",
        caption = "Data points represent Mean ± SD"
      ) +
      theme_classic(base_size = 14) +
      theme(
        axis.text.x = element_text(angle = 30, hjust = 1, face = "bold", color = "black"),
        axis.text.y = element_text(color = "black"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.caption = element_text(size = 10, face = "italic", color = "gray30")
      )
    
    # 7. Add dashed treatment start marker
    if (traj$is_pooled && !is.null(traj$tx_week)) {
      p <- p + geom_vline(xintercept = traj$tx_week, linetype = "dashed", color = "gray50", linewidth = 0.8)
    }
    
    p
  })
  
  output$weight_historical_trend_plot <- renderPlot({ reactive_weight_plot() })
  
  # --- Interactive Project Ledger View ---
  output$weight_master_database_view_table <- renderDT({
    df <- weight_project_data()
    req(nrow(df) > 0)
    datatable(df %>% arrange(Group, Timeline, Animal_ID) %>%
                select(`Cohort Group` = Group, `Animal ID` = Animal_ID, `Study Point` = Timeline, `Mass (g)` = Mass_g, `Log Date` = Log_Date, Notes),
              selection = 'single', filter = 'top', options = list(pageLength = 5, autoWidth = TRUE))
  })
  
  # --- Surgical Deletion Functionality ---
  observeEvent(input$delete_selected_weight_row_btn, {
    req(input$weight_master_database_view_table_rows_selected)
    df <- weight_project_data() %>% arrange(Group, Timeline, Animal_ID)
    row_to_kill <- df[input$weight_master_database_view_table_rows_selected, ]
    
    updated_db <- vars$weight_historical_data() %>% 
      filter(!(Project == row_to_kill$Project & Animal_ID == row_to_kill$Animal_ID & 
                 Group == row_to_kill$Group & Timeline == row_to_kill$Timeline))
    
    vars$weight_historical_data(updated_db)
    write_csv(updated_db, weight_db_path)
    showNotification("Target mass entry surgically dropped from database tracking ledger.", type = "warning")
  })
  
  # --- STATISTICAL CONTROLS SHIELD ---
  output$weight_stats_time_filter_ui <- renderUI({
    df <- weight_project_data()
    if(nrow(df) == 0) return(NULL)
    
    tagList(
      selectInput("weight_stats_week", "Isolate Analysis Temporal Point (Post-Hoc Snapshot):", 
                  choices = sort(unique(df$Timeline)), selected = max(df$Timeline), width = "100%"),
      checkboxGroupInput("weight_stats_groups", "Select Cohorts to Include in Test Pool:",
                         choices = unique(df$Group), selected = unique(df$Group), inline = TRUE)
    )
  })
  
  # --- Global Two-Way ANOVA Matrix ---
  output$weight_anova_dt_table <- renderDT({
    df <- weight_project_data()
    req(input$weight_stats_groups)
    
    filtered_df <- df %>% filter(Group %in% input$weight_stats_groups)
    
    if (length(unique(filtered_df$Group)) < 2 || length(unique(filtered_df$Timeline)) < 2 || nrow(filtered_df) < 4) {
      return(datatable(tibble(`Notice` = "Select at least 2 groups and 2 timepoints to execute global ANOVA analysis.") , options = list(dom = 't'), rownames = FALSE))
    }
    
    anova_model <- summary(aov(Mass_g ~ Group * Timeline, data = filtered_df))[[1]]
    
    anova_tibble <- tibble(
      `Source of Variation` = c(
        "Experimental Cohort Group", 
        "Temporal Interval (Timeline Track)", 
        "Interaction Effect (Group x Time)", 
        "Residual Error (Within Group Variance)"
      ),
      `Degrees of Freedom (Df)` = anova_model$Df,
      `Sum of Squares (SS)` = round(anova_model$`Sum Sq`, 2),
      `Mean Squares (MS)` = round(anova_model$`Mean Sq`, 2),
      `F-Statistic` = round(anova_model$`F value`, 2),
      `P-Value Magnitude` = anova_model$`Pr(>F)`
    )
    
    datatable(anova_tibble, options = list(dom = 't', ordering = FALSE), rownames = FALSE) %>%
      formatStyle('P-Value Magnitude', 
                  backgroundColor = styleInterval(c(0.05), c('#d4edda', 'transparent')),
                  fontWeight = styleInterval(c(0.05), c('bold', 'normal'))) %>%
      formatSignif(columns = c('P-Value Magnitude'), digits = 4)
  })
  
  # --- Post-Hoc Snapshots Matrix ---
  output$weight_posthoc_dt_table <- renderDT({
    df <- weight_project_data()
    req(input$weight_stats_week, input$weight_stats_groups)
    
    snapshot_df <- df %>% 
      filter(Timeline == input$weight_stats_week, Group %in% input$weight_stats_groups)
    
    if (length(unique(snapshot_df$Group)) < 2 || nrow(snapshot_df) < 3) {
      return(datatable(tibble(`Notice` = "Select at least 2 groups with active data to compute variance comparisons."), options = list(dom = 't'), rownames = FALSE))
    }
    
    if (length(unique(snapshot_df$Group)) == 2) {
      groups_vec <- unique(snapshot_df$Group)
      g1_data <- snapshot_df %>% filter(Group == groups_vec[1]) %>% pull(Mass_g)
      g2_data <- snapshot_df %>% filter(Group == groups_vec[2]) %>% pull(Mass_g)
      
      ttest_res <- t.test(g1_data, g2_data, var.equal = TRUE)
      anova_model <- summary(aov(Mass_g ~ Group, data = snapshot_df))[[1]]
      
      two_group_stats <- tibble(
        `Statistical Test Parameters` = c(
          paste("ANOVA Factor: Group (", groups_vec[1], "vs", groups_vec[2], ")"),
          "ANOVA Residuals (Within Group Error)",
          paste("Parallel Independent t-test Metric")
        ),
        `Degrees of Freedom (df)` = c(anova_model$Df[1], anova_model$Df[2], round(ttest_res$parameter, 1)),
        `Sum/Mean Squares` = c(paste("SS:", round(anova_model$`Sum Sq`[1], 1)), paste("MS:", round(anova_model$`Mean Sq`[2], 1)), NA),
        `Calculated Statistic` = c(paste("F =", round(anova_model$`F value`[1], 2)), NA, paste("t =", round(ttest_res$statistic, 3))),
        `P-Value Magnitude (p)` = c(anova_model$`Pr(>F)`[1], NA, ttest_res$p.value)
      )
      
      return(
        datatable(two_group_stats, options = list(dom = 't'), rownames = FALSE) %>%
          formatStyle('P-Value Magnitude (p)', 
                      backgroundColor = styleInterval(c(0.05), c('#d4edda', 'transparent')),
                      fontWeight = styleInterval(c(0.05), c('bold', 'normal'))) %>%
          formatSignif(columns = c('P-Value Magnitude (p)'), digits = 4)
      )
      
    } else {
      tukey_matrix <- TukeyHSD(aov(Mass_g ~ Group, data = snapshot_df), "Group")$Group
      
      tukey_tibble <- as_tibble(tukey_matrix, rownames = "Pairwise Cohort Comparison") %>%
        transmute(
          `Pairwise Cohort Comparison` = paste("Tukey Matrix:", `Pairwise Cohort Comparison`),
          `Mean Difference (Delta)` = round(diff, 2),
          `Lower 95% CI` = round(lwr, 2),
          `Upper 95% CI` = round(upr, 2),
          `Adjusted P-Value (Tukey p-adj)` = `p adj`
        )
      
      return(
        datatable(tukey_tibble, options = list(dom = 't'), rownames = FALSE) %>%
          formatStyle('Adjusted P-Value (Tukey p-adj)', 
                      backgroundColor = styleInterval(c(0.05), c('#d4edda', 'transparent')),
                      fontWeight = styleInterval(c(0.05), c('bold', 'normal'))) %>%
          formatSignif(columns = c('Adjusted P-Value (Tukey p-adj)'), digits = 4)
      )
    }
  })
  
  # --- Data Export Downloader Blocks ---
  output$download_weight_master_dataset <- downloadHandler(
    filename = function() { paste0("Weight_Animal_Level_Data_", input$global_project, "_", Sys.Date(), ".csv") },
    content = function(file) { write_csv(weight_project_data(), file) }
  )
  output$download_weight_plot_pdf <- downloadHandler(
    filename = function() { paste0("Weight_Plot_", input$global_project, "_", Sys.Date(), ".pdf") },
    content = function(file) { ggsave(file, plot = reactive_weight_plot(), device = "pdf", width = 6.5, height = 4.8) }
  )
  output$download_weight_plot_png <- downloadHandler(
    filename = function() { paste0("Weight_Plot_", input$global_project, "_", Sys.Date(), ".png") },
    content = function(file) { ggsave(file, plot = reactive_weight_plot(), device = "png", width = 6.5, height = 4.8, dpi = 300) }
  )
  
  observeEvent(input$warn_clear_weight_btn, {
    cleared_db <- vars$weight_historical_data() %>% filter(Project != input$global_project)
    vars$weight_historical_data(cleared_db)
    write_csv(cleared_db, weight_db_path)
  })
}