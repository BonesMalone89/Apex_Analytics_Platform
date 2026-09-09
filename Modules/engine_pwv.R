# =========================================================================
# PULSE WAVE VELOCITY (PWV) DOPPLER CORE MODULE
# =========================================================================

# --- UI Layout Component ---
ui_pwv_layout <- function() {
  sidebarLayout(
    sidebarPanel(
      tags$h4("Step 1: Subject Metadata"),
      fluidRow(
        column(6, textInput("pwv_rat_id", "Animal ID:", value = "Animal 1")),
        column(6, selectizeInput("pwv_group", "Group Assignment:", choices = NULL))
      ),
      
      fluidRow(
        column(6, selectizeInput("pwv_week", "Timeline Point:", choices = NULL)),
        column(6, selectizeInput("pwv_subrun", "Measurement Run:", choices = paste("Run", 1:4), selected = "Run 1"))
      ),
      
      numericInput("pwv_distance", "Measurement Distance (mm):", value = 40, min = 10, max = 100, step = 0.5),
      
      hr(),
      tags$details(
        tags$summary(style = "cursor: pointer; color: #2c3e50; font-weight: bold;", "⚙️ Advanced Stability Filters (Click to expand)"),
        br(),
        sliderInput("pwv_hr_delta", "Max Allowed Heart Rate Deviation (BPM):", min = 5, max = 25, value = 15, step = 1),
        fluidRow(
          column(6, numericInput("pwv_min_vel", "Min Velocity (m/s):", value = 1.0, min = 0.1, step = 0.1)),
          column(6, numericInput("pwv_max_vel", "Max Velocity (m/s):", value = 12.0, min = 5.0, step = 0.1))
        ),
        br()
      ),
      hr(),
      
      tags$h4("Step 2: Doppler Terminal Panels"),
      sliderInput("pwv_wave_count", "Number of Annotated Waves Per Site:", min = 1, max = 4, value = 2, step = 1),
      
      uiOutput("dynamic_doppler_boxes"),
      
      br(),
      actionButton("process_pwv_btn", "⚡ Run Wave Pair-Matching", class = "btn-primary btn-block"),
      br(),
      actionButton("append_database_btn", "💾 Append to Historical Dataset", class = "btn-success btn-block"),
      hr(),
      actionButton("warn_clear_pwv_btn", "🧼 Clear Project PWV Dataset", class = "btn-danger btn-block")
    ),
    
    mainPanel(
      tabsetPanel(
        type = "pills",
        tabPanel("📊 Active Experiment Dashboard",
                 br(),
                 fluidRow(
                   column(6,
                          tags$h4("📋 Waveform Matching Results"),
                          tableOutput("pwv_matched_pairs_table"),
                          br(),
                          tags$h4("🎯 Waveform Evaluation Notes"),
                          verbatimTextOutput("pwv_session_summary_box")
                   ),
                   column(6,
                          div(class = "dash-card",
                              tags$h4("📈 Long-Term Pulse Wave Velocity Trends"),
                              plotOutput("pwv_historical_trend_plot", height = "320px")
                          ),
                          fluidRow(
                            column(12, actionButton("open_pwv_graph_modal_btn", "🎨 Customize Graph Titles", class = "btn-xs btn-default btn-block"))
                          ),
                          br(),
                          fluidRow(
                            column(4, downloadButton("download_pwv_master_dataset", "Export CSV", class = "btn-xs")),
                            column(4, downloadButton("download_pwv_plot_pdf", "Save PDF", class = "btn-xs btn-info")),
                            column(4, downloadButton("download_pwv_plot_png", "Save PNG", class = "btn-xs btn-info"))
                          )
                   )
                 ),
                 hr(),
                 tags$h4("🗂️Historical Entry Database"),
                 tags$p(tags$small(tags$em("Click on any row below and press the red button to delete an experimental record error."))),
                 DTOutput("pwv_master_database_view_table"),
                 br(),
                 actionButton("delete_selected_pwv_row_btn", "❌ Delete Highlighted Entry from Registry", class = "btn-sm btn-danger")
        ),
        tabPanel("📊 Statistics Panel",
                 br(),
                 wellPanel(
                   style = "background-color: #f8f9fa; border: 1px solid #e3e6f0;",
                   tags$h4(tags$strong("🎛️ Analysis Controls")),
                   uiOutput("pwv_stats_time_filter_ui")
                 ),
                 hr(),
                 tags$h4("🧪 Model Summary: Variance Profile (Two-Way Global ANOVA)"),
                 tags$p(tags$small(tags$em("Evaluates selected cohorts dynamically across the entire longitudinal timeline."))),
                 DTOutput("pwv_anova_dt_table"),
                 hr(),
                 tags$h4("📑 Post-Hoc Comparison Matrix (Isolated Tukey's HSD / t-test)"),
                 tags$p(tags$small(tags$em("Evaluates selected cohorts isolated purely at the single temporal point chosen above."))),
                 DTOutput("pwv_posthoc_dt_table")
        )
      )
    )
  )
}

# --- Server Logic Component ---
server_pwv_logic <- function(input, output, session, vars, pwv_titles) {
  
  pwv_project_data <- reactive({ vars$pwv_historical_data() %>% filter(Project == input$global_project) })
  
  observeEvent(input$open_pwv_graph_modal_btn, {
    showModal(modalDialog(
      title = "🎨 PWV Publication Title Customizer",
      textInput("modal_pwv_title", "Custom Graph Title:", value = pwv_titles$title),
      textInput("modal_pwv_xlabel", "X-Axis Label:", value = pwv_titles$xlab),
      textInput("modal_pwv_ylabel", "Y-Axis Label:", value = pwv_titles$ylab),
      actionButton("save_pwv_labels_btn", "💾 Apply Text Updates", class = "btn-block btn-success"),
      easyClose = TRUE, footer = modalButton("Dismiss")
    ))
  })
  
  observeEvent(input$save_pwv_labels_btn, {
    pwv_titles$title <- input$modal_pwv_title
    pwv_titles$xlab  <- input$modal_pwv_xlabel
    pwv_titles$ylab  <- input$modal_pwv_ylabel
    removeModal()
  })
  
  output$dynamic_doppler_boxes <- renderUI({
    lapply(1:input$pwv_wave_count, function(i) {
      fluidRow(
        style = "margin-bottom: 2px;",
        column(6, style = "padding-right: 2px;", textInput(paste0("arch_paste_", i), label = paste("Arch Wave", i), placeholder = "Paste raw string...")),
        column(6, style = "padding-left: 2px;", textInput(paste0("abdom_paste_", i), label = paste("Abdom Wave", i), placeholder = "Paste raw string..."))
      )
    })
  })
  
  session_analysis <- eventReactive(input$process_pwv_btn, {
    num_waves <- input$pwv_wave_count
    session_rows <- list()
    
    extract_metric <- function(text_string, label_anchor) {
      if (is.null(text_string) || length(text_string) == 0 || is.na(text_string)) return(NA_real_)
      text_string <- as.character(text_string)
      if (nchar(trimws(text_string)) == 0) return(NA_real_)
      
      pattern <- paste0("(?i)", label_anchor, "\\s+0\\.00\\s+(\\d+\\.\\d+)")
      match <- stringr::str_match(text_string, pattern)
      
      if (!is.na(match[1, 2])) {
        return(as.numeric(match[1, 2]))
      } else {
        return(NA_real_)
      }
    }
    
    for (i in 1:num_waves) {
      arch_txt  <- input[[paste0("arch_paste_", i)]]
      abdom_txt <- input[[paste0("abdom_paste_", i)]]
      
      if (!is.null(arch_txt) && !is.null(abdom_txt)) {
        hr_a  <- extract_metric(arch_txt, "Heart Rate")
        pet_a <- extract_metric(arch_txt, "Pre-ejection Time")
        hr_b  <- extract_metric(abdom_txt, "Heart Rate")
        pet_b <- extract_metric(abdom_txt, "Pre-ejection Time")
        
        if (!is.na(hr_a) && !is.na(pet_a) && !is.na(hr_b) && !is.na(pet_b)) {
          hr_diff <- abs(hr_a - hr_b)
          tt_ms   <- abs(pet_b - pet_a)
          vel     <- input$pwv_distance / tt_ms
          
          status_check <- case_when(
            hr_diff > input$pwv_hr_delta ~ "REJECT: HR Instability",
            vel < input$pwv_min_vel | vel > input$pwv_max_vel ~ "REJECT: Out of Bounds",
            TRUE ~ "VALID"
          )
          
          session_rows[[length(session_rows) + 1]] <- tibble(
            Pair_Index = i, HR_Arch = hr_a, HR_Abdom = hr_b, HR_Delta = hr_diff,
            Transit_Time_ms = tt_ms, Velocity_ms = vel, Status = status_check
          )
        }
      }
    }
    
    if (length(session_rows) == 0) return(NULL)
    
    bind_rows(session_rows)
  })
  
  output$pwv_matched_pairs_table <- renderTable({
    res <- session_analysis()
    
    validate(
      need(
        isTRUE(!is.null(res) && is.data.frame(res) && nrow(res) > 0), 
        "Awaiting valid parallel text blocks... Ensure pasted blocks contain Heart Rate and Pre-ejection Time."
      )
    )
    
    res %>%
      select(
        `Wave Set` = Pair_Index, 
        `HR Arch` = HR_Arch, 
        `HR Abdom` = HR_Abdom, 
        `Δ HR` = HR_Delta,
        `Transit Time (ms)` = Transit_Time_ms, 
        `Solved PWV (m/s)` = Velocity_ms, 
        `Quality Check` = Status
      )
  }, striped = TRUE, hover = TRUE, bordered = TRUE, align = 'c')
  
  output$pwv_session_summary_box <- renderText({
    res <- session_analysis()
    if(is.null(res) || !is.data.frame(res)) return("Awaiting wave pair-matching analysis...")
    
    valid <- res %>% filter(Status == "VALID")
    if(nrow(valid) == 0) return("⚠️ QUALITY METRIC EXCEPTION: No parallel waves survived filter rails.")
    paste0("Subject: ", input$pwv_rat_id, " [", input$pwv_group, "]\nTimeline: ", input$pwv_week, " [", input$pwv_subrun, "]\nResolved Mean PWV: ", round(mean(valid$Velocity_ms), 3), " m/s")
  })
  
  observeEvent(input$append_database_btn, {
    res <- session_analysis()
    
    if (is.null(res) || !is.data.frame(res)) {
      showNotification("Cannot append: No wave data processed yet.", type = "error")
      return()
    }
    
    df <- res %>% filter(Status == "VALID")
    
    if (nrow(df) == 0) {
      showNotification("Cannot append: No valid wave pairs survived stability filters.", type = "warning")
      return()
    }
    
    meta <- vars$metadata_registry()
    target_hex <- meta %>% filter(Project_Key == input$global_project & Group_Key == input$pwv_group) %>% pull(Group_Color) %>% first()
    if(is.null(target_hex) || is.na(target_hex)) target_hex <- "#333333"
    
    new_row <- tibble(
      Project = input$global_project, Rat_ID = input$pwv_rat_id, Group = input$pwv_group,
      Group_Color = target_hex, Timeline = input$pwv_week, SubRun = input$pwv_subrun, 
      Distance_mm = input$pwv_distance, Velocity_ms = mean(df$Velocity_ms)
    )
    
    updated_db <- bind_rows(vars$pwv_historical_data(), new_row) %>%
      group_by(Project, Rat_ID, Group, Timeline, SubRun) %>% slice_tail(n = 1) %>% ungroup()
    
    vars$pwv_historical_data(updated_db)
    write_csv(updated_db, "pwv_master_database.csv")
    
    for (i in 1:input$pwv_wave_count) {
      updateTextInput(session, paste0("arch_paste_", i), value = "")
      updateTextInput(session, paste0("abdom_paste_", i), value = "")
    }
    showNotification("PWV Data row successfully secured to historical dataset.", type = "message")
  })
  
  reactive_pwv_plot <- reactive({
    df <- pwv_project_data()
    req(nrow(df) > 0)
    
    # 1. Pull active project metadata
    meta <- vars$metadata_registry() %>% filter(Project_Key == input$global_project)
    req(nrow(meta) > 0)
    
    weeks_count <- if (!is.na(meta$Max_Weeks[1])) meta$Max_Weeks[1] else 12
    tx_week     <- if ("Tx_Start_Week" %in% colnames(meta) && !is.na(meta$Tx_Start_Week[1])) meta$Tx_Start_Week[1] else 1
    pool_active <- if ("Pool_Baseline" %in% colnames(meta) && !is.na(meta$Pool_Baseline[1])) meta$Pool_Baseline[1] else TRUE
    
    # 2. Standardize column name to 'Value' for the shared helper
    df_input <- df %>% mutate(Value = Velocity_ms)
    
    # 3. Call the universal helper function
    traj <- prep_longitudinal_trajectory(
      df = df_input,
      tx_week = tx_week,
      pool_baseline = pool_active
    )
    
    # 4. Generate ggplot
    p <- ggplot(traj$stats, aes(x = Time_Index, y = mean_val, color = Group, group = Group)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 3.5) +
      geom_errorbar(aes(ymin = mean_val - sd_val, ymax = mean_val + sd_val), width = 0.12, linewidth = 0.9) +
      scale_x_continuous(breaks = 0:weeks_count, labels = paste0("W", 0:weeks_count)) +
      scale_color_manual(values = traj$palette) +
      labs(title = pwv_titles$title, x = pwv_titles$xlab, y = pwv_titles$ylab, color = "Group") +
      theme_classic(base_size = 14) +
      theme(
        legend.position = "right",
        axis.text = element_text(color = "black", face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold")
      )
    
    # 5. Add dashed treatment initiation line if pooled
    if (traj$is_pooled && !is.null(traj$tx_week)) {
      p <- p + geom_vline(xintercept = traj$tx_week, linetype = "dashed", color = "gray50", linewidth = 0.8)
    }
    
    p
  })
  
  output$pwv_historical_trend_plot <- renderPlot({ reactive_pwv_plot() })
  
  output$pwv_master_database_view_table <- renderDT({
    df <- pwv_project_data()
    req(nrow(df) > 0)
    datatable(df %>% arrange(Group, Timeline, SubRun, Rat_ID) %>%
                select(`Animal Identifier` = Rat_ID, `Cohort Group` = Group, `Study Week` = Timeline, `Run` = SubRun, `Transducer Space (mm)` = Distance_mm, `Velocity Mean (m/s)` = Velocity_ms),
              selection = 'single', filter = 'top', options = list(pageLength = 5, autoWidth = TRUE))
  })
  
  observeEvent(input$delete_selected_pwv_row_btn, {
    req(input$pwv_master_database_view_table_rows_selected)
    df <- pwv_project_data() %>% arrange(Group, Timeline, SubRun, Rat_ID)
    row_to_kill <- df[input$pwv_master_database_view_table_rows_selected, ]
    
    updated_db <- vars$pwv_historical_data() %>% 
      filter(!(Project == row_to_kill$Project & Rat_ID == row_to_kill$Rat_ID & 
                 Group == row_to_kill$Group & Timeline == row_to_kill$Timeline & SubRun == row_to_kill$SubRun))
    
    vars$pwv_historical_data(updated_db)
    write_csv(updated_db, "pwv_master_database.csv")
    showNotification("Target entry successfully excised from PWV registry track.", type = "warning")
  })
  
  # ---------------------------------------------------------------------
  # 📊 RE-ENGINEERED PULSE WAVE VELOCITY STATISTICAL ANALYSIS ENGINE
  # ---------------------------------------------------------------------
  
  output$pwv_stats_time_filter_ui <- renderUI({
    df <- pwv_project_data()
    if(nrow(df) == 0) return(NULL)
    
    tagList(
      selectInput("pwv_stats_week", "Isolate Analysis Temporal Point (Post-Hoc Snapshot):", 
                  choices = sort(unique(df$Timeline)), selected = max(df$Timeline), width = "100%"),
      checkboxGroupInput("pwv_stats_groups", "Select Cohorts to Include in Test Pool:",
                         choices = unique(df$Group), selected = unique(df$Group), inline = TRUE)
    )
  })
  
  output$pwv_anova_dt_table <- renderDT({
    df <- pwv_project_data()
    req(input$pwv_stats_groups)
    
    filtered_df <- df %>% filter(Group %in% input$pwv_stats_groups)
    
    if (length(unique(filtered_df$Group)) < 2 || length(unique(filtered_df$Timeline)) < 2 || nrow(filtered_df) < 4) {
      return(datatable(tibble(`Notice` = "Select at least 2 active cohort columns to run global longitudinal variance analysis."), options = list(dom = 't'), rownames = FALSE))
    }
    
    anova_model <- summary(aov(Velocity_ms ~ Group * Timeline, data = filtered_df))[[1]]
    
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
  
  output$pwv_posthoc_dt_table <- renderDT({
    df <- pwv_project_data()
    req(input$pwv_stats_week, input$pwv_stats_groups)
    
    snapshot_df <- df %>% 
      filter(Timeline == input$pwv_stats_week, Group %in% input$pwv_stats_groups)
    
    if (length(unique(snapshot_df$Group)) < 2 || nrow(snapshot_df) < 3) {
      return(datatable(tibble(`Notice` = "Select at least 2 groups with active data to compute variance comparisons."), options = list(dom = 't'), rownames = FALSE))
    }
    
    if (length(unique(snapshot_df$Group)) == 2) {
      groups_vec <- unique(snapshot_df$Group)
      g1_data <- snapshot_df %>% filter(Group == groups_vec[1]) %>% pull(Velocity_ms)
      g2_data <- snapshot_df %>% filter(Group == groups_vec[2]) %>% pull(Velocity_ms)
      
      ttest_res <- t.test(g1_data, g2_data, var.equal = TRUE)
      anova_model <- summary(aov(Velocity_ms ~ Group, data = snapshot_df))[[1]]
      
      two_group_stats <- tibble(
        `Statistical Test Parameters` = c(
          paste("ANOVA Factor: Group (", groups_vec[1], "vs", groups_vec[2], ")"),
          "ANOVA Residuals (Within Group Error)",
          paste("Parallel Independent t-test Metric")
        ),
        `Degrees of Freedom (df)` = c(anova_model$Df[1], anova_model$Df[2], round(ttest_res$parameter, 1)),
        `Sum/Mean Squares` = c(paste("SS:", round(anova_model$`Sum Sq`[1], 3)), paste("MS:", round(anova_model$`Mean Sq`[2], 3)), NA),
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
      tukey_matrix <- TukeyHSD(aov(Velocity_ms ~ Group, data = snapshot_df), "Group")$Group
      
      tukey_tibble <- as_tibble(tukey_matrix, rownames = "Pairwise Cohort Comparison") %>%
        transmute(
          `Pairwise Cohort Comparison` = paste("Tukey Matrix:", `Pairwise Cohort Comparison`),
          `Mean Difference (Δ)` = round(diff, 3),
          `Lower 95% CI` = round(lwr, 3),
          `Upper 95% CI` = round(upr, 3),
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
  
  output$download_pwv_master_dataset <- downloadHandler(
    filename = function() { paste0("PWV_Master_Data_", input$global_project, "_", Sys.Date(), ".csv") },
    content = function(file) { write_csv(pwv_project_data(), file) }
  )
  output$download_pwv_plot_pdf <- downloadHandler(
    filename = function() { paste0("PWV_Plot_", input$global_project, "_", Sys.Date(), ".pdf") },
    content = function(file) { ggsave(file, plot = reactive_pwv_plot(), device = "pdf", width = 6.5, height = 4.8) }
  )
  output$download_pwv_plot_png <- downloadHandler(
    filename = function() { paste0("PWV_Plot_", input$global_project, "_", Sys.Date(), ".png") },
    content = function(file) { ggsave(file, plot = reactive_pwv_plot(), device = "png", width = 6.5, height = 4.8, dpi = 300) }
  )
  
  observeEvent(input$warn_clear_pwv_btn, {
    cleared_db <- vars$pwv_historical_data() %>% filter(Project != input$global_project)
    vars$pwv_historical_data(cleared_db)
    write_csv(cleared_db, "pwv_master_database.csv")
  })
}