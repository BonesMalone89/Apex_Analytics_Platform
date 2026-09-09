# =========================================================================
# CODA BLOOD PRESSURE PIPELINE CORE MODULE
# =========================================================================

# --- UI Layout Component ---
ui_bp_layout <- function() {
  sidebarLayout(
    sidebarPanel(
      tags$h4("Step 1: Session Parameters"),
      selectizeInput("bp_group", "Group Assignment:", choices = NULL),
      fluidRow(
        column(6, selectizeInput("bp_week", "Timeline Point:", choices = NULL)),
        column(6, selectizeInput("bp_subrun", "Measurement Run:", choices = paste("Run", 1:4), selected = "Run 1"))
      ),
      hr(),
      
      tags$h4("Step 2: Drop CODA File"),
      fileInput("bp_dropped_file", "Choose or Drop Raw Export CSV File",
                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"),
                buttonLabel = "Browse...", placeholder = "No file selected"),
      
      br(),
      actionButton("process_bp_btn", "⚡ Parse and Verify Animal Cycles", class = "btn-primary btn-block"),
      br(),
      actionButton("append_bp_database_btn", "💾 Append to Historical Dataset", class = "btn-success btn-block"),
      hr(),
      
      tags$h4("Step 3: Graph Display Controls"),
      fluidRow(
        column(6, numericInput("bp_ymin", "Y-Min Limit:", value = 100)),
        column(6, numericInput("bp_ymax", "Y-Max Limit:", value = 240))
      ),
      fluidRow(
        column(7, checkboxInput("bp_pool_baseline", "Pool Baseline Cohort", value = TRUE)),
        column(5, numericInput("bp_treatment_start", "Tx Week:", value = 1, min = 0, max = 50))
      ),
      br(),
      actionButton("open_bp_graph_modal_btn", "🎨 Customize Graph Titles", class = "btn-xs btn-default btn-block"),
      br(),
      actionButton("warn_clear_bp_btn", "🧼 Clear Project BP Dataset", class = "btn-danger btn-block")
    ),
    
    mainPanel(
      tabsetPanel(
        type = "pills",
        tabPanel("📈 Long-Term Systolic Trends",
                 br(),
                 fluidRow(
                   column(5,
                          tags$h4("📋 Session Processing Evaluation Summary"),
                          verbatimTextOutput("bp_session_summary_box")
                   ),
                   column(7,
                          div(class = "dash-card",
                              tags$h4("📈 Long-Term Blood Pressure Trends"),
                              plotOutput("bp_historical_trend_plot", height = "320px")
                          ),
                          br(),
                          fluidRow(
                            column(4, downloadButton("download_bp_master_dataset", "Export CSV", class = "btn-xs")),
                            column(4, downloadButton("download_bp_plot_pdf", "Save PDF", class = "btn-xs btn-info")),
                            column(4, downloadButton("download_bp_plot_png", "Save PNG", class = "btn-xs btn-info"))
                          )
                   )
                 ),
                 hr(),
                 tags$h4("🗂️ Historical Entry Database"),
                 tags$p(tags$small(tags$em("Click on any row below and press the red button to delete an experimental record error."))),
                 DTOutput("bp_master_database_view_table"),
                 br(),
                 actionButton("delete_selected_bp_row_btn", "❌ Delete Highlighted Entry from Registry", class = "btn-sm btn-danger")
        ),
        tabPanel("📊 Statistics Panel",
                 br(),
                 wellPanel(
                   style = "background-color: #f8f9fa; border: 1px solid #e3e6f0;",
                   tags$h4(tags$strong("🎛️ Analysis Controls")),
                   uiOutput("bp_stats_time_filter_ui")
                 ),
                 hr(),
                 tags$h4("🧪 Model Summary: Variance Profile (Two-Way Global ANOVA)"),
                 tags$p(tags$small(tags$em("Evaluates selected cohorts dynamically across the entire longitudinal timeline."))),
                 DTOutput("bp_anova_dt_table"),
                 hr(),
                 tags$h4("📑 Post-Hoc Comparison Matrix (Isolated Tukey's HSD / t-test)"),
                 tags$p(tags$small(tags$em("Evaluates selected cohorts isolated purely at the single temporal point chosen above."))),
                 DTOutput("bp_posthoc_dt_table")
        )
      )
    )
  )
}

# --- Server Logic Component ---
server_bp_logic <- function(input, output, session, vars, bp_titles) {
  
  bp_project_data <- reactive({ vars$bp_historical_data() %>% filter(Project == input$global_project) })
  
  observeEvent(input$open_bp_graph_modal_btn, {
    showModal(modalDialog(
      title = "🎨 Blood Pressure Title Labels Customizer",
      textInput("modal_bp_title", "Custom Graph Title:", value = bp_titles$title),
      textInput("modal_bp_xlabel", "X-Axis Label:", value = bp_titles$xlab),
      textInput("modal_bp_ylabel", "Y-Axis Label:", value = bp_titles$ylab),
      actionButton("save_bp_labels_btn", "💾 Apply Text Updates", class = "btn-block btn-success"),
      easyClose = TRUE, footer = modalButton("Dismiss")
    ))
  })
  
  observeEvent(input$save_bp_labels_btn, {
    bp_titles$title <- input$modal_bp_title
    bp_titles$xlab  <- input$modal_bp_xlabel
    bp_titles$ylab  <- input$modal_bp_ylabel 
    removeModal()
  })
  
  bp_file_analysis <- eventReactive(input$process_bp_btn, {
    req(input$bp_dropped_file)
    
    raw_csv_data <- read_csv(input$bp_dropped_file$datapath, col_types = cols(.default = "c"), show_col_types = FALSE)
    
    if (is.null(raw_csv_data) || ncol(raw_csv_data) == 0) return("Error: Uploaded file appears to be empty or unreadable.")
    
    cols <- colnames(raw_csv_data)
    if (is.null(cols) || length(cols) == 0) return("Error: CSV has no header row.")
    
    cols <- ifelse(grepl("(?i)specimen|animal|subject|rat", cols), "animal_id_raw", cols)
    cols <- ifelse(grepl("(?i)systolic", cols), "systolic", cols)
    cols <- ifelse(grepl("(?i)accepted|valid|include", cols), "accepted", cols)
    cols <- ifelse(grepl("(?i)regular", cols), "regular_cycle", cols)
    
    colnames(raw_csv_data) <- cols
    
    if (!("systolic" %in% colnames(raw_csv_data))) {
      return(paste("Error: Could not locate a 'Systolic' column. Detected headers:", paste(cols, collapse = ", ")))
    }
    if (!("accepted" %in% colnames(raw_csv_data))) {
      return(paste("Error: Could not locate an 'Accepted' column. Detected headers:", paste(cols, collapse = ", ")))
    }
    if (!("animal_id_raw" %in% colnames(raw_csv_data))) {
      return(paste("Error: Could not locate 'Specimen/Animal ID' column. Detected headers:", paste(cols, collapse = ", ")))
    }
    
    processed_df <- raw_csv_data %>%
      clean_names() %>% 
      mutate(systolic = as.numeric(systolic)) %>%
      filter(tolower(accepted) %in% c("true", "1", "yes", "y"), !is.na(systolic))
    
    if ("regular_cycle" %in% colnames(raw_csv_data)) {
      processed_df <- processed_df %>% filter(tolower(regular_cycle) %in% c("true", "1", "yes", "y"))
    }
    
    if (nrow(processed_df) == 0) {
      return("Error: No data rows cleared filtration metrics (Accepted & Regular == TRUE).")
    }
    
    animal_summaries <- processed_df %>%
      group_by(animal_id_raw) %>%
      summarise(Animal_Mean = mean(systolic), Valid_Cycles = n(), .groups = "drop")
    
    animal_summaries
  })
  
  output$bp_session_summary_box <- renderText({
    req(input$bp_dropped_file)
    
    res <- bp_file_analysis()
    
    if (is.character(res)) {
      return(res)
    }
    
    if (is.null(res) || !is.data.frame(res) || nrow(res) == 0) {
      return("Awaiting valid CODA file upload and cycle verification...")
    }
    
    paste0("Target Group Assignment: [", input$bp_group, "]\n",
           "Timeline Slot: ", input$bp_week, " [", input$bp_subrun, "]\n",
           "Distinct Independent Animals Discovered: ", nrow(res), "\n",
           "Detected Animal Identifiers: ", paste(res$animal_id_raw, collapse = ", "), "\n",
           "Total Study Group Grand Mean: ", round(mean(res$Animal_Mean), 2), " mmHg")
  })
  
  observeEvent(input$append_bp_database_btn, {
    if (is.null(input$bp_dropped_file)) {
      showNotification("Cannot append: No raw CODA CSV file uploaded.", type = "error")
      return()
    }
    
    res <- tryCatch(bp_file_analysis(), error = function(e) NULL)
    
    if (is.character(res)) {
      showNotification(res, type = "error")
      return()
    }
    
    if (is.null(res) || !is.data.frame(res) || nrow(res) == 0) {
      showNotification("Cannot append: Check evaluation notes panel for missing header columns.", type = "warning")
      return()
    }
    
    meta <- vars$metadata_registry()
    target_hex <- meta %>% filter(Project_Key == input$global_project & Group_Key == input$bp_group) %>% pull(Group_Color) %>% first()
    if(is.null(target_hex) || is.na(target_hex)) target_hex <- "#333333"
    
    new_rows <- res %>%
      transmute(
        Project = input$global_project,
        Animal_ID = animal_id_raw,
        Group = input$bp_group,
        Group_Color = target_hex,
        Timeline = input$bp_week,
        SubRun = input$bp_subrun,
        Mean_Systolic = Animal_Mean
      )
    
    updated_db <- bind_rows(vars$bp_historical_data(), new_rows) %>%
      group_by(Project, Animal_ID, Group, Timeline, SubRun) %>% slice_tail(n = 1) %>% ungroup()
    
    vars$bp_historical_data(updated_db)
    write_csv(updated_db, "bp_master_database.csv")
    
    showNotification(paste("Securely appended multi-animal metrics tracker for", input$bp_group), type = "message")
  })
  
  reactive_bp_plot <- reactive({
    df <- bp_project_data()
    req(nrow(df) > 0)
    
    # 1. Pull the reactive global metadata for the active study
    meta <- vars$metadata_registry() %>% filter(Project_Key == input$global_project)
    req(nrow(meta) > 0)
    
    # 2. Standardize column name to 'Value' for the helper
    df_input <- df %>% mutate(Value = Mean_Systolic)
    
    # 3. Call the universal helper
    traj <- prep_longitudinal_trajectory(
      df = df_input, 
      tx_week = meta$Tx_Start_Week[1], 
      pool_baseline = meta$Pool_Baseline[1]
    )
    
    # 4. Build ggplot
    p <- ggplot(traj$stats, aes(x = Time_Index, y = mean_val, color = Group, group = Group)) +
      geom_line(linewidth = 1.3) +
      geom_errorbar(aes(ymin = mean_val - sd_val, ymax = mean_val + sd_val), width = 0.12, linewidth = 0.8) +
      geom_point(size = 3.5) +
      scale_x_continuous(breaks = 0:meta$Max_Weeks[1], labels = paste0("W", 0:meta$Max_Weeks[1])) +
      scale_color_manual(values = traj$palette) +
      coord_cartesian(ylim = c(input$bp_ymin, input$bp_ymax)) +
      labs(title = bp_titles$title, x = bp_titles$xlab, y = bp_titles$ylab, color = "Group") +
      theme_classic(base_size = 14) +
      theme(axis.text = element_text(color = "black", face = "bold"), plot.title = element_text(hjust = 0.5, face = "bold"))
    
    if (traj$is_pooled) {
      p <- p + geom_vline(xintercept = traj$tx_week, linetype = "dashed", color = "gray50", linewidth = 0.8)
    }
    
    p
  })
  
  output$bp_historical_trend_plot <- renderPlot({ reactive_bp_plot() })
  
  output$bp_master_database_view_table <- renderDT({
    df <- bp_project_data()
    req(nrow(df) > 0)
    datatable(df %>% arrange(Group, Timeline, SubRun, Animal_ID) %>%
                select(`Cohort Group` = Group, `Animal ID` = Animal_ID, `Study Week` = Timeline, `Run Modifier` = SubRun, `Mean Systolic (mmHg)` = Mean_Systolic),
              selection = 'single', filter = 'top', options = list(pageLength = 5, autoWidth = TRUE))
  })
  
  observeEvent(input$delete_selected_bp_row_btn, {
    req(input$bp_master_database_view_table_rows_selected)
    df <- bp_project_data() %>% arrange(Group, Timeline, SubRun, Animal_ID)
    row_to_kill <- df[input$bp_master_database_view_table_rows_selected, ]
    
    updated_db <- vars$bp_historical_data() %>% 
      filter(!(Project == row_to_kill$Project & Animal_ID == row_to_kill$Animal_ID & 
                 Group == row_to_kill$Group & Timeline == row_to_kill$Timeline & SubRun == row_to_kill$SubRun))
    
    vars$bp_historical_data(updated_db)
    write_csv(updated_db, "bp_master_database.csv")
    showNotification("Target animal entry surgically dropped from BP database tracking ledger.", type = "warning")
  })
  
  # --- BLOOD PRESSURE STATISTICAL ANALYSIS ENGINE ---
  
  output$bp_stats_time_filter_ui <- renderUI({
    df <- bp_project_data()
    if(nrow(df) == 0) return(NULL)
    
    tagList(
      selectInput("bp_stats_week", "Isolate Analysis Temporal Point (Post-Hoc Snapshot):", 
                  choices = sort(unique(df$Timeline)), selected = max(df$Timeline), width = "100%"),
      checkboxGroupInput("bp_stats_groups", "Select Cohorts to Include in Test Pool:",
                         choices = unique(df$Group), selected = unique(df$Group), inline = TRUE)
    )
  })
  
  output$bp_anova_dt_table <- renderDT({
    df <- bp_project_data()
    req(input$bp_stats_groups)
    
    filtered_df <- df %>% filter(Group %in% input$bp_stats_groups)
    
    if (length(unique(filtered_df$Group)) < 2 || length(unique(filtered_df$Timeline)) < 2 || nrow(filtered_df) < 4) {
      return(datatable(tibble(`Notice` = "Select at least 2 active cohort columns to run global longitudinal variance analysis.") , options = list(dom = 't'), rownames = FALSE))
    }
    
    anova_model <- summary(aov(Mean_Systolic ~ Group * Timeline, data = filtered_df))[[1]]
    
    anova_tibble <- tibble(
      `Source of Variation (Factor Trajectory)` = c(
        paste("Experimental Cohort Group [Subset Matrix]"), 
        "Temporal Interval (Timeline Track)", 
        "Interaction Effect (Group × Time Divergence)", 
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
  
  output$bp_posthoc_dt_table <- renderDT({
    df <- bp_project_data()
    req(input$bp_stats_week, input$bp_stats_groups)
    
    snapshot_df <- df %>% 
      filter(Timeline == input$bp_stats_week, Group %in% input$bp_stats_groups)
    
    if (length(unique(snapshot_df$Group)) < 2 || nrow(snapshot_df) < 3) {
      return(datatable(tibble(`Notice` = "Select at least 2 groups with active data to compute variance comparisons."), options = list(dom = 't'), rownames = FALSE))
    }
    
    if (length(unique(snapshot_df$Group)) == 2) {
      groups_vec <- unique(snapshot_df$Group)
      g1_data <- snapshot_df %>% filter(Group == groups_vec[1]) %>% pull(Mean_Systolic)
      g2_data <- snapshot_df %>% filter(Group == groups_vec[2]) %>% pull(Mean_Systolic)
      
      ttest_res <- t.test(g1_data, g2_data, var.equal = TRUE)
      anova_model <- summary(aov(Mean_Systolic ~ Group, data = snapshot_df))[[1]]
      
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
      tukey_matrix <- TukeyHSD(aov(Mean_Systolic ~ Group, data = snapshot_df), "Group")$Group
      
      tukey_tibble <- as_tibble(tukey_matrix, rownames = "Pairwise Cohort Comparison") %>%
        transmute(
          `Pairwise Cohort Comparison` = paste("Tukey Matrix:", `Pairwise Cohort Comparison`),
          `Mean Difference (Δ)` = round(diff, 2),
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
  
  output$download_bp_master_dataset <- downloadHandler(
    filename = function() { paste0("BP_Animal_Level_Data_", input$global_project, "_", Sys.Date(), ".csv") },
    content = function(file) { write_csv(bp_project_data(), file) }
  )
  output$download_bp_plot_pdf <- downloadHandler(
    filename = function() { paste0("BP_Plot_", input$global_project, "_", Sys.Date(), ".pdf") },
    content = function(file) { ggsave(file, plot = reactive_bp_plot(), device = "pdf", width = 6.5, height = 4.8) }
  )
  output$download_bp_plot_png <- downloadHandler(
    filename = function() { paste0("BP_Plot_", input$global_project, "_", Sys.Date(), ".png") },
    content = function(file) { ggsave(file, plot = reactive_bp_plot(), device = "png", width = 6.5, height = 4.8, dpi = 300) }
  )
  
  observeEvent(input$warn_clear_bp_btn, {
    cleared_db <- vars$bp_historical_data() %>% filter(Project != input$global_project)
    vars$bp_historical_data(cleared_db)
    write_csv(cleared_db, "bp_master_database.csv")
  })
}