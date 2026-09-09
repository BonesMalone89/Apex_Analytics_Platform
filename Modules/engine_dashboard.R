# =========================================================================
# APEX PLATFORM VISUAL EXECUTIVE HUB & INTERACTIVE WORKSTATION ENGINE
# =========================================================================

library(shiny)
library(dplyr)
library(stringr)
library(ggplot2)
library(tibble)
library(tidyr)
library(DT)

# --- UI Layout Component ---
ui_dashboard_layout <- function() {
  tagList(
    tags$head(
      tags$style(HTML("
        .modal-lg { max-width: 1200px !important; width: 90vw !important; }
        .stat-box { background: #f8f9fa; border-left: 4px solid #4e73df; padding: 10px; margin-bottom: 10px; border-radius: 4px; text-align: left; }
        .stats-header { font-size: 13px; font-weight: bold; color: #2c3e50; text-transform: uppercase; margin-bottom: 8px; }
        
        /* Fixed Column Wrapping & Table Layout Formatting Fixes */
        .compact-dt table.dataTable { margin: 0 !important; font-size: 11px !important; width: 100% !important; table-layout: fixed !important; }
        .compact-dt .dataTables_wrapper { padding: 0 !important; }
        .compact-dt td, .compact-dt th { white-space: normal !important; word-break: break-all !important; padding: 6px 4px !important; }
        
        /* Aspect Ratio Control: Prevent viewport distortion across wide displays */
        .dash-card-plot { margin: 0 auto; max-width: 650px; width: 100%; }
        
        /* Left-aligned dropdown typography adjustments */
        .left-aligned-select .selectize-input { text-align: left !important; direction: ltr !important; padding-left: 8px !important; }
        
        /* Force body-parented selectize dropdown to float above modals */
        body > .selectize-dropdown { 
          z-index: 9999 !important; 
        }
      "))
    ),
    
    # MASTER PROJECT COHORT FILTER & CHART TOGGLE CARD
    div(class = "dash-card", style = "background-color: #f8f9fa; border-left: 5px solid #4e73df; text-align: left;",
        fluidRow(
          column(4, tags$h4(style="margin-top:5px; font-weight:bold; color:#2c3e50;", "🎛️ Workstation Controls")),
          column(8, 
                 checkboxGroupInput("dash_cohort_filter", "Filter Cohorts in Active Scope:", choices = NULL, inline = TRUE),
                 hr(style = "margin: 8px 0; opacity: 0.15;"),
                 checkboxGroupInput("dash_visible_modules", "Select Active Dashboard Charts:",
                                    choices = c("Systolic Blood Pressure" = "BP", 
                                                "Pulse Wave Velocity" = "PWV", 
                                                "Body Weight Growth" = "Weight", 
                                                "System-Wide Correlation Matrix" = "Corr", 
                                                "Cannulation MAP" = "Can"),
                                    selected = c("BP", "PWV", "Weight", "Corr", "Can"), 
                                    inline = TRUE)
          )
        )
    ),
    
    # DYNAMIC HUB GRID MATRIX
    uiOutput("dynamic_dashboard_grid")
  )
}

# --- Server Logic Function ---
server_dashboard_logic <- function(input, output, session, vars) {
  
  # --- 1. Gather Raw Project Data Streams ---
  bp_raw  <- reactive({ vars$bp_historical_data() %>% filter(Project == input$global_project) })
  pwv_raw <- reactive({ vars$pwv_historical_data() %>% filter(Project == input$global_project) })
  wt_raw  <- reactive({ vars$weight_historical_data() %>% filter(Project == input$global_project) })
  can_raw <- reactive({ vars$cannulation_historical_data() %>% filter(Project == input$global_project) })
  
  # --- 2. Synchronize Master Controls with Active Metadata ---
  observe({
    meta <- vars$metadata_registry() %>% filter(Project_Key == input$global_project)
    if (nrow(meta) > 0) {
      updateCheckboxGroupInput(session, "dash_cohort_filter", choices = unique(meta$Group_Key), selected = unique(meta$Group_Key))
    }
  })
  
  # --- 3. Dynamic Layout Engine Block ---
  output$dynamic_dashboard_grid <- renderUI({
    req(input$dash_visible_modules)
    visible <- input$dash_visible_modules
    
    card_bp <- column(4, div(class = "dash-card", actionLink("expand_panel_bp_link", tagList(
      tags$h4(tags$strong("Systolic Blood Pressure")),
      div(class = "dash-card-plot", plotOutput("grid_bp_micro", height = "220px"))
    ))))
    
    card_pwv <- column(4, div(class = "dash-card", actionLink("expand_panel_pwv_link", tagList(
      tags$h4(tags$strong("Pulse Wave Velocity")),
      div(class = "dash-card-plot", plotOutput("grid_pwv_micro", height = "220px"))
    ))))
    
    card_wt <- column(4, div(class = "dash-card", actionLink("expand_panel_wt_link", tagList(
      tags$h4(tags$strong("Body Weight Growth")),
      div(class = "dash-card-plot", plotOutput("grid_wt_micro", height = "220px"))
    ))))
    
    card_corr <- column(6, div(class = "dash-card", actionLink("expand_panel_corr_link", tagList(
      tags$h4(tags$strong("System-Wide Correlation Matrix")),
      div(class = "dash-card-plot", plotOutput("grid_corr_micro", height = "240px"))
    ))))
    
    card_can <- column(6, div(class = "dash-card", actionLink("expand_panel_can_link", tagList(
      tags$h4(tags$strong("Cannulation MAP")),
      div(class = "dash-card-plot", plotOutput("grid_can_micro", height = "240px"))
    ))))
    
    # Sort and bundle active choices into rows
    row_1_elements <- list()
    if ("BP" %in% visible) row_1_elements <- c(row_1_elements, list(card_bp))
    if ("PWV" %in% visible) row_1_elements <- c(row_1_elements, list(card_pwv))
    if ("Weight" %in% visible) row_1_elements <- c(row_1_elements, list(card_wt))
    
    row_2_elements <- list()
    if ("Corr" %in% visible) row_2_elements <- c(row_2_elements, list(card_corr))
    if ("Can" %in% visible) row_2_elements <- c(row_2_elements, list(card_can))
    
    # Responsive structural column overrides
    if (length(row_1_elements) == 1) {
      row_1_elements[[1]]$attribs$class <- "col-sm-12"
    } else if (length(row_1_elements) == 2) {
      row_1_elements[[1]]$attribs$class <- "col-sm-6"
      row_1_elements[[2]]$attribs$class <- "col-sm-6"
    }
    
    if (length(row_2_elements) == 1) {
      row_2_elements[[1]]$attribs$class <- "col-sm-12"
    }
    
    tagList(
      if (length(row_1_elements) > 0) fluidRow(row_1_elements) else NULL,
      if (length(row_2_elements) > 0) fluidRow(row_2_elements) else NULL
    )
  })
  
  # --- 4. Advanced Multimode Plotting Helpers with Universal Trajectory Integration ---
  generate_trend_plot <- function(df, metric, mode = "trend") {
    req(nrow(df) > 0)
    
    meta <- vars$metadata_registry() %>% filter(Project_Key == input$global_project)
    max_w       <- if (nrow(meta) > 0 && !is.na(meta$Max_Weeks[1])) meta$Max_Weeks[1] else 12
    tx_week     <- if (nrow(meta) > 0 && "Tx_Start_Week" %in% colnames(meta) && !is.na(meta$Tx_Start_Week[1])) meta$Tx_Start_Week[1] else 1
    pool_active <- if (nrow(meta) > 0 && "Pool_Baseline" %in% colnames(meta) && !is.na(meta$Pool_Baseline[1])) meta$Pool_Baseline[1] else TRUE
    
    # Standardize metric column
    df$Target_Y <- df[[metric]]
    plot_df <- df %>% 
      filter(!is.na(Target_Y)) %>%
      mutate(Value = Target_Y)
    
    # 1. Normalize timeline discrete milestones into standardized W-keys
    plot_df <- plot_df %>%
      mutate(
        Timeline_Normalized = case_when(
          grepl("(?i)base|arrival", Timeline) ~ "W0",
          grepl("(?i)week", Timeline) ~ paste0("W", stringr::str_extract(Timeline, "\\d+")),
          grepl("(?i)pre-?op", Timeline) ~ paste0("W", max_w + 1),
          grepl("(?i)post-?op", Timeline) ~ paste0("W", max_w + 2),
          grepl("(?i)terminal|harvest", Timeline) ~ paste0("W", max_w + 3),
          TRUE ~ Timeline
        )
      ) %>%
      mutate(Timeline = Timeline_Normalized)
    
    # 2. Call the universal helper function
    traj <- prep_longitudinal_trajectory(
      df = plot_df,
      tx_week = tx_week,
      pool_baseline = pool_active,
      baseline_label = "Baseline (Pooled)",
      baseline_color = "#34495e"
    )
    
    summary_df <- traj$stats
    palette_vector <- traj$palette
    
    # High-visibility rendering constants
    base_text_sz <- 14
    line_weight  <- 1.6
    point_sz     <- 3.2
    
    # Dynamic Error Bar Width Calculation
    unique_times <- unique(summary_df$Time_Index)
    n_pts <- length(unique_times)
    cap_width <- if (n_pts <= 1) 0.08 else min(0.15, (max(unique_times) - min(unique_times)) * 0.05)
    
    # Safe X-axis labels
    format_label <- function(idx) {
      if (idx == 0) return("Baseline")
      if (idx > 0 && idx <= max_w) return(paste0("W", idx))
      if (idx == max_w + 1) return("Pre-Op")
      if (idx == max_w + 2) return("Post-Op")
      if (idx == max_w + 3) return("Terminal")
      return(paste0("W", idx))
    }
    
    sorted_breaks <- sort(unique_times)
    sorted_labels <- sapply(sorted_breaks, format_label)
    
    g <- ggplot()
    
    if (mode == "trend") {
      g <- g + 
        geom_line(data = summary_df, aes(x = Time_Index, y = mean_val, color = Group, group = Group), linewidth = line_weight) +
        geom_errorbar(data = summary_df, aes(x = Time_Index, ymin = mean_val - sd_val, ymax = mean_val + sd_val, color = Group), width = cap_width, linewidth = (line_weight * 0.7)) +
        geom_point(data = summary_df, aes(x = Time_Index, y = mean_val, color = Group), size = point_sz)
      
    } else if (mode == "spaghetti") {
      spag_df <- plot_df %>%
        mutate(
          Week_Num = as.numeric(stringr::str_extract(Timeline, "-?\\d+")),
          Week_Num = ifelse(is.na(Week_Num), 0, Week_Num),
          Sub_Num  = as.numeric(stringr::str_extract(SubRun, "\\d+")),
          Sub_Num  = ifelse(is.na(Sub_Num), 1, Sub_Num),
          Time_Index = Week_Num + ((Sub_Num - 1) * 0.2)
        )
      
      g <- g + 
        geom_line(data = spag_df, aes(x = Time_Index, y = Target_Y, color = Group, group = Animal_ID), linewidth = 0.5, alpha = 0.35) +
        geom_line(data = summary_df, aes(x = Time_Index, y = mean_val, color = Group, group = Group), linewidth = line_weight) +
        geom_point(data = summary_df, aes(x = Time_Index, y = mean_val, color = Group), size = point_sz)
      
    } else if (mode == "boxplot") {
      box_df <- plot_df %>%
        mutate(
          Week_Num = as.numeric(stringr::str_extract(Timeline, "-?\\d+")),
          Week_Num = ifelse(is.na(Week_Num), 0, Week_Num),
          Display_Time = factor(sapply(Week_Num, format_label), levels = sorted_labels)
        )
      
      g <- g + 
        geom_boxplot(data = box_df, aes(x = Display_Time, y = Target_Y, fill = Group), width = 0.5, alpha = 0.5, outlier.shape = NA) +
        geom_point(data = box_df, aes(x = Display_Time, y = Target_Y, color = Group), size = (point_sz * 0.6), position = position_jitterdodge(jitter.width = 0.05, dodge.width = 0.5), alpha = 0.6)
      
    } else if (mode == "dotmatrix") {
      dot_df <- plot_df %>%
        mutate(
          Week_Num = as.numeric(stringr::str_extract(Timeline, "-?\\d+")),
          Week_Num = ifelse(is.na(Week_Num), 0, Week_Num),
          Sub_Num  = as.numeric(stringr::str_extract(SubRun, "\\d+")),
          Sub_Num  = ifelse(is.na(Sub_Num), 1, Sub_Num),
          Time_Index = Week_Num + ((Sub_Num - 1) * 0.2)
        )
      
      g <- g + 
        geom_point(data = dot_df, aes(x = Time_Index, y = Target_Y, color = Group), size = point_sz, alpha = 0.7, position = position_jitter(width = 0.04, height = 0))
    }
    
    # Styling and Scales
    g <- g + 
      scale_color_manual(values = palette_vector) + 
      scale_fill_manual(values = palette_vector) + 
      theme_classic(base_size = base_text_sz) +
      theme(
        axis.text = element_text(color = "black", face = "bold"),
        axis.line = element_line(linewidth = 0.8, color = "black"),
        legend.title = element_blank()
      )
    
    # Continuous breaks for non-boxplot modes
    if (mode != "boxplot") {
      if (n_pts == 1) {
        g <- g + scale_x_continuous(
          breaks = sorted_breaks,
          labels = sorted_labels,
          limits = c(sorted_breaks[1] - 0.5, sorted_breaks[1] + 0.5)
        )
      } else {
        g <- g + scale_x_continuous(
          breaks = sorted_breaks,
          labels = sorted_labels
        )
      }
      
      # Add vertical dashed treatment intervention line
      if (traj$is_pooled && !is.null(traj$tx_week)) {
        g <- g + geom_vline(xintercept = traj$tx_week, linetype = "dashed", color = "gray50", linewidth = 0.8)
      }
    }
    
    return(g)
  }
  
  # --- 5. Compile Master Relational Database Preserving SubRuns ---
  compiled_cross_assay_data <- reactive({
    df_bp  <- bp_raw()
    df_pwv <- pwv_raw()
    df_wt  <- wt_raw()
    
    bp_tidy <- if (nrow(df_bp) > 0) {
      df_bp %>% select(Animal_ID, Timeline, SubRun, BP = Mean_Systolic, Group_BP = Group, Color_BP = Group_Color)
    } else {
      tibble(Animal_ID = character(), Timeline = character(), SubRun = character(), BP = numeric(), Group_BP = character(), Color_BP = character())
    }
    
    pwv_tidy <- if (nrow(df_pwv) > 0) {
      df_pwv %>% select(Animal_ID = Rat_ID, Timeline, SubRun, PWV = Velocity_ms, Group_PWV = Group, Color_PWV = Group_Color)
    } else {
      tibble(Animal_ID = character(), Timeline = character(), SubRun = character(), PWV = numeric(), Group_PWV = character(), Color_PWV = character())
    }
    
    wt_tidy <- if (nrow(df_wt) > 0) {
      df_wt %>% 
        mutate(Timeline_Clean = case_when(
          Timeline == "Baseline/Arrival" ~ "Baseline/Arrival",
          Timeline == "Pre-Op" ~ "Pre-Op",
          Timeline == "Post-Op" ~ "Post-Op",
          Timeline == "Terminal Harvest" ~ "Terminal Harvest",
          TRUE ~ str_replace(Timeline, "Week ", "W")
        )) %>% 
        transmute(Animal_ID, Timeline = Timeline_Clean, SubRun = "Run 1", Weight = Mass_g, Group_WT = Group, Color_WT = Group_Color)
    } else {
      tibble(Animal_ID = character(), Timeline = character(), SubRun = character(), Weight = numeric(), Group_WT = character(), Color_WT = character())
    }
    
    all_animals   <- unique(c(bp_tidy$Animal_ID, pwv_tidy$Animal_ID, wt_tidy$Animal_ID))
    all_timelines <- unique(c(bp_tidy$Timeline, pwv_tidy$Timeline, wt_tidy$Timeline))
    all_subruns   <- unique(c(bp_tidy$SubRun, pwv_tidy$SubRun, wt_tidy$SubRun))
    
    if (length(all_animals) == 0 || length(all_timelines) == 0) return(tibble())
    
    backbone <- expand.grid(Animal_ID = all_animals, Timeline = all_timelines, SubRun = all_subruns, stringsAsFactors = FALSE) %>% as_tibble()
    
    master_join <- backbone %>%
      left_join(bp_tidy, by = c("Animal_ID", "Timeline", "SubRun")) %>%
      left_join(pwv_tidy, by = c("Animal_ID", "Timeline", "SubRun")) %>%
      left_join(wt_tidy, by = c("Animal_ID", "Timeline", "SubRun")) %>%
      mutate(
        Group = case_when(!is.na(Group_BP) ~ Group_BP, !is.na(Group_PWV) ~ Group_PWV, !is.na(Group_WT) ~ Group_WT, TRUE ~ "Unassigned"),
        Group_Color = case_when(!is.na(Color_BP) ~ Color_BP, !is.na(Color_PWV) ~ Color_PWV, !is.na(Color_WT) ~ Color_WT, TRUE ~ "#7f8c8d")
      )
    
    if (!is.null(input$dash_cohort_filter)) {
      master_join <- master_join %>% filter(Group %in% input$dash_cohort_filter)
    }
    return(master_join)
  })
  
  # --- 6. Generate Pearson Correlation Matrix ---
  pearson_correlation_df <- reactive({
    df <- compiled_cross_assay_data()
    can <- can_raw()
    
    if (nrow(df) == 0) return(tibble())
    
    long_means <- df %>% 
      group_by(Animal_ID) %>% 
      summarise(
        `Systolic BP` = mean(BP, na.rm = TRUE),
        `Pulse Wave Velocity` = mean(PWV, na.rm = TRUE),
        `Body Weight` = mean(Weight, na.rm = TRUE),
        .groups = "drop"
      )
    
    can_means <- if (nrow(can) > 0) {
      can %>% 
        group_by(Animal_ID = as.character(Animal_ID)) %>% 
        summarise(`Terminal MAP` = mean(Value, na.rm = TRUE), .groups = "drop")
    } else {
      tibble(Animal_ID = character(), `Terminal MAP` = numeric())
    }
    
    joined_metrics <- long_means %>% 
      left_join(can_means, by = "Animal_ID") %>% 
      select(-Animal_ID) %>% 
      select(where(~ any(!is.na(.))))
    
    if (ncol(joined_metrics) < 2) return(tibble())
    
    corr_matrix <- cor(joined_metrics, use = "pairwise.complete.obs", method = "pearson")
    
    corr_matrix %>% 
      as.data.frame() %>% 
      rownames_to_column(var = "Metric_A") %>% 
      pivot_longer(cols = -Metric_A, names_to = "Metric_B", values_to = "Coefficient")
  })
  
  # --- MAIN DASHBOARD SCREEN MICRO PLOT RENDERERS ---
  output$grid_bp_micro   <- renderPlot({ generate_trend_plot(compiled_cross_assay_data(), "BP", mode = "trend") + theme(axis.title = element_blank()) })
  output$grid_pwv_micro  <- renderPlot({ generate_trend_plot(compiled_cross_assay_data(), "PWV", mode = "trend") + theme(axis.title = element_blank()) })
  output$grid_wt_micro   <- renderPlot({ generate_trend_plot(compiled_cross_assay_data(), "Weight", mode = "trend") + theme(axis.title = element_blank()) })
  
  output$grid_corr_micro <- renderPlot({
    df <- pearson_correlation_df()
    if (nrow(df) == 0) {
      return(ggplot() + annotate("text", x=1, y=1, label="Insufficient data overlaps to map correlation space.", fontface="italic") + theme_void())
    }
    
    ggplot(df, aes(x = Metric_A, y = Metric_B, fill = Coefficient)) + 
      geom_tile(color = "white", linewidth = 1.5) +
      geom_text(aes(label = sprintf("%.2f", Coefficient)), color = "black", fontface = "bold", size = 4.5) +
      scale_fill_gradient2(low = "#4e73df", mid = "#f8f9fa", high = "#e74a3b", 
                           midpoint = 0, limit = c(-1, 1), name = "Pearson (r)") +
      theme_minimal(base_size = 11) +
      theme(
        axis.title = element_blank(),
        axis.text = element_text(color = "black", face = "bold"),
        panel.grid = element_blank(),
        legend.position = "none"
      )
  })
  
  output$grid_can_micro  <- renderPlot({
    df <- can_raw(); if (nrow(df) == 0) return(NULL)
    if (!is.null(input$dash_cohort_filter)) df <- df %>% filter(Group %in% input$dash_cohort_filter)
    meta_colors <- vars$metadata_registry() %>% filter(Project_Key == input$global_project) %>% distinct(Group_Key, Group_Color)
    summary_stats <- df %>% group_by(Group) %>% summarise(mean_val = mean(Value), sd_val = ifelse(n() > 1, sd(Value), 0), .groups = "drop") %>% left_join(meta_colors, by = c("Group" = "Group_Key"))
    palette_vector <- setNames(summary_stats$Group_Color, summary_stats$Group)
    
    ggplot(summary_stats, aes(x = Group, y = mean_val, fill = Group)) + 
      geom_bar(stat = "identity", width = 0.45, color = "black", alpha = 0.4) +
      geom_errorbar(aes(ymin = mean_val - sd_val, ymax = mean_val + sd_val), width = 0.12, linewidth = 1.0, color = "black") +
      scale_fill_manual(values = palette_vector) + 
      theme_classic(base_size = 14) + 
      theme(legend.position = "none", axis.title = element_blank(), axis.text = element_text(color="black", face="bold"))
  })
  
  # =========================================================================
  # HUB MODAL INTERROGATION DECK CONTROLLERS
  # =========================================================================
  observeEvent(input$expand_panel_bp_link, { trigger_longitudinal_modal("BP", "🩺 Blood Pressure Longitudinal Workstation") })
  observeEvent(input$expand_panel_pwv_link, { trigger_longitudinal_modal("PWV", "🏎️ Pulse Wave Velocity Compliance Workstation") })
  observeEvent(input$expand_panel_wt_link, { trigger_longitudinal_modal("Weight", "⚖️ Body Weight Kinetic Workstation") })
  
  trigger_longitudinal_modal <- function(target_metric, modal_title) {
    showModal(modalDialog(
      title = modal_title, size = "l", easyClose = TRUE,
      fluidRow(
        column(7, 
               div(class="dash-card", style="padding:10px;", 
                   div(class="left-aligned-select", 
                       selectizeInput("modal_view_mode", "Select Longitudinal Mapping Style:", 
                                      choices = c("Cohort Trends (Mean ± SD)" = "trend", 
                                                  "Individual Tracks (Spaghetti Plot)" = "spaghetti", 
                                                  "Distribution Spread (Boxplots)" = "boxplot", 
                                                  "Specimen Dot Matrix Grid" = "dotmatrix"), 
                                      options = list(dropdownParent = "body")))),
               hr(),
               plotOutput("modal_track_render", height="420px", click="modal_track_click")),
        column(5,
               div(class="stat-box", style="border-left-color:#4e73df;",
                   tags$h4(tags$strong("Interactive Node Explorer")),
                   hr(), uiOutput("modal_track_click_readout")),
               div(class="stat-box", style="border-left-color:#1cc88a;",
                   tags$h4(tags$strong("Longitudinal Variance Model")),
                   tags$p(tags$small(tags$em("Two-Way ANOVA Global interaction analysis evaluations."))),
                   hr(), div(class="compact-dt", DTOutput("modal_stats_dt_view")))
        )
      ),
      footer = modalButton("Dismiss Workstation")
    ))
    
    output$modal_track_render <- renderPlot({ 
      req(input$modal_view_mode)
      generate_trend_plot(compiled_cross_assay_data(), target_metric, mode = input$modal_view_mode) + 
        labs(x = "Study Progression Timeline", y = target_metric) +
        theme(axis.text = element_text(color="black", face="bold"), base_size = 15)
    })
    
    output$modal_track_click_readout <- renderUI({
      df <- compiled_cross_assay_data(); req(nrow(df) > 0, input$modal_track_click)
      meta <- vars$metadata_registry() %>% filter(Project_Key == input$global_project)
      max_w <- if (nrow(meta) > 0) meta$Max_Weeks[1] else 12
      df$Target_Y <- df[[target_metric]]
      
      s_df <- df %>% filter(!is.na(Target_Y)) %>%
        mutate(Week_Num = as.numeric(str_extract(Timeline, "\\d+")), Sub_Num = as.numeric(str_extract(SubRun, "\\d+")),
               Time_Index = case_when(Timeline == "Baseline/Arrival" ~ 0.0, Timeline == "Pre-Op" ~ as.numeric(max_w) + 1.0, Timeline == "Post-Op" ~ as.numeric(max_w) + 2.0, Timeline == "Terminal Harvest" ~ as.numeric(max_w) + 3.0, TRUE ~ Week_Num + ((Sub_Num - 1) * 0.2)))
      
      clicked <- nearPoints(s_df, input$modal_track_click, yvar="Target_Y", maxpoints=1, threshold=20)
      if (nrow(clicked) == 0) return(tags$p(style="color:gray; font-style:italic;", "Click squarely on an individual analyte dot..."))
      
      tagList(
        tags$h5(tags$strong(paste("Specimen ID Reference:", clicked$Animal_ID))),
        tags$p(tags$strong("Cohort: "), clicked$Group), tags$p(tags$strong("Timeline Point: "), clicked$Timeline), tags$p(tags$strong("Run Track: "), clicked$SubRun), hr(),
        tags$p(tags$strong("Absolute Reading: "), round(clicked$Target_Y, 2))
      )
    })
    
    output$modal_stats_dt_view <- renderDT({
      df <- compiled_cross_assay_data(); if (nrow(df)==0) return(NULL)
      df$Target <- df[[target_metric]]
      filtered_df <- df %>% filter(!is.na(Target))
      
      if (length(unique(filtered_df$Group)) < 2 || length(unique(filtered_df$Timeline)) < 2) {
        return(datatable(tibble(`Notice` = "Requires longitudinal cohort dataset tracks."), options = list(dom='t'), rownames=FALSE))
      }
      
      res <- summary(aov(Target ~ Group * Timeline, data = filtered_df))[[1]]
      out_df <- tibble(`Source of Variation` = c("Group Factor", "Timeline Track", "Interaction Matrix"), `p-value` = res$`Pr(>F)`[1:3])
      
      datatable(out_df, options = list(dom='t', autoWidth=TRUE, columnDefs = list(list(width = '65%', targets = 0), list(width = '35%', targets = 1))), rownames=FALSE) %>% 
        formatStyle('p-value', backgroundColor = styleInterval(c(0.05), c('#d4edda', 'transparent')), fontWeight = styleInterval(c(0.05), c('bold', 'normal'))) %>% 
        formatSignif('p-value', digits=4)
    })
  }
  
  # --- MODAL B: SYSTEM-WIDE HEATMAP EXPANSION WORKSTATION ---
  observeEvent(input$expand_panel_corr_link, {
    showModal(modalDialog(
      title = "🎯 System-Wide Co-Expression & Correlation Heat Matrix", size = "l", easyClose = TRUE,
      fluidRow(
        column(7, 
               div(class = "dash-card", style = "padding: 10px;",
                   plotOutput("modal_corr_render", height = "460px")
               )
        ),
        column(5, 
               div(class = "stat-box", style = "border-left-color: #e74a3b;", 
                   tags$h4(tags$strong("Statistical Correlation Dossier")), 
                   tags$p(tags$small(tags$em("Calculated Pearson (r) coefficients across pairwise project nodes."))), 
                   hr(), 
                   div(class = "compact-dt", DTOutput("modal_corr_stats_dt"))
               )
        )
      ),
      footer = modalButton("Dismiss Heat Matrix")
    ))
    
    output$modal_corr_render <- renderPlot({
      df <- pearson_correlation_df()
      req(nrow(df) > 0)
      
      ggplot(df, aes(x = Metric_A, y = Metric_B, fill = Coefficient)) + 
        geom_tile(color = "white", linewidth = 2.0) +
        geom_text(aes(label = sprintf("%.3f", Coefficient)), color = "black", fontface = "bold", size = 5.5) +
        scale_fill_gradient2(low = "#4e73df", mid = "#f8f9fa", high = "#e74a3b", 
                             midpoint = 0, limit = c(-1, 1), name = "Pearson (r)") +
        theme_classic(base_size = 14) + 
        theme(
          axis.title = element_blank(),
          axis.text = element_text(color = "black", face = "bold"),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          legend.position = "right"
        )
    })
    
    output$modal_corr_stats_dt <- renderDT({
      df <- pearson_correlation_df()
      req(nrow(df) > 0)
      
      unique_pairs <- df %>% 
        filter(Metric_A < Metric_B) %>% 
        arrange(desc(abs(Coefficient))) %>% 
        transmute(
          `Comparison Node` = paste(Metric_A, "vs", Metric_B),
          `Pearson (r)` = Coefficient,
          `Direction` = ifelse(Coefficient > 0, "Positive (+)", "Negative (-)")
        )
      
      datatable(unique_pairs, 
                options = list(dom = 't', pageLength = 10, autoWidth = TRUE,
                               columnDefs = list(list(className = 'dt-center', targets = 1:2))), 
                rownames = FALSE) %>% 
        formatSignif('Pearson (r)', digits = 4) %>% 
        formatStyle('Pearson (r)', 
                    backgroundColor = styleInterval(c(-0.5, 0.5), c('rgba(78, 115, 223, 0.1)', 'transparent', 'rgba(231, 74, 59, 0.1)')), 
                    fontWeight = "bold")
    })
  })
  
  # --- MODAL C: SURGICAL MAP PROFILER ---
  observeEvent(input$expand_panel_can_link, {
    showModal(modalDialog(
      title = "💉 High-Resolution Surgical Endpoint Pressure Workstation", size = "l", easyClose = TRUE,
      fluidRow(
        column(7, div(class="dash-card", style="padding:10px;", plotOutput("modal_can_render", height="460px", click="modal_can_click"))),
        column(5, 
               div(class="stat-box", style="border-left-color:#1cc88a;", tags$h4(tags$strong("Surgical Node Profile")), hr(), uiOutput("modal_can_click_info")),
               div(class="stat-box", style="border-left-color:#34495e;", tags$h4(tags$strong("Surgical Variance Matrix")), hr(), div(class="compact-dt", DTOutput("modal_can_stats_dt")))
        )
      ),
      footer = modalButton("Dismiss Surgical Workstation")
    ))
    
    output$modal_can_render <- renderPlot({
      df <- can_raw(); req(nrow(df) > 0)
      if (!is.null(input$dash_cohort_filter)) df <- df %>% filter(Group %in% input$dash_cohort_filter)
      meta_colors <- vars$metadata_registry() %>% filter(Project_Key == input$global_project) %>% distinct(Group_Key, Group_Color)
      summary_stats <- df %>% group_by(Group) %>% summarise(mean_val = mean(Value), sd_val = ifelse(n() > 1, sd(Value), 0), .groups = "drop") %>% left_join(meta_colors, by = c("Group" = "Group_Key"))
      palette_vector <- setNames(summary_stats$Group_Color, summary_stats$Group)
      
      ggplot() + 
        geom_bar(data = summary_stats, aes(x = Group, y = mean_val, fill = Group), stat = "identity", width = 0.45, color = "black", alpha = 0.4) +
        geom_errorbar(data = summary_stats, aes(x = Group, ymin = mean_val - sd_val, ymax = mean_val + sd_val), width = 0.12, linewidth = 1.0, color = "black") +
        geom_point(data = df, aes(x = Group, y = Value, color = Group), size = 5.0, position = position_jitter(width = 0.06, height = 0), alpha = 0.8) +
        scale_fill_manual(values = palette_vector) + scale_color_manual(values = palette_vector) + 
        scale_y_continuous(expand = expansion(mult = c(0, 0.12))) + labs(x = "Cohort Group Matrix", y = "MAP (mmHg)", title = "Terminal Mean Arterial Pressure Spread") + 
        theme_classic(base_size = 15) + theme(plot.title = element_text(hjust = 0.5, face = "bold"), legend.position = "none")
    })
    
    output$modal_can_click_info <- renderUI({
      df <- can_raw(); req(nrow(df) > 0, input$modal_can_click)
      if (!is.null(input$dash_cohort_filter)) df <- df %>% filter(Group %in% input$dash_cohort_filter)
      clicked <- nearPoints(df, input$modal_can_click, xvar="Group", yvar="Value", maxpoints=1, threshold=20)
      if (nrow(clicked) == 0) return("Click on a subject dot coordinate...")
      tagList(tags$h5(tags$strong(clicked$Animal_ID)), tags$p("Cohort: ", clicked$Group), hr(), tags$p("MAP: ", round(clicked$Value, 2), " mmHg"))
    })
    
    output$modal_can_stats_dt <- renderDT({
      df <- can_raw(); req(nrow(df) >= 3)
      if (!is.null(input$dash_cohort_filter)) df <- df %>% filter(Group %in% input$dash_cohort_filter)
      
      if (length(unique(df$Group)) == 2) {
        g_vec <- unique(df$Group)
        t_res <- t.test(df$Value[df$Group == g_vec[1]], df$Value[df$Group == g_vec[2]])
        datatable(tibble(`Contrast` = paste(g_vec[1], "vs", g_vec[2]), `p-value` = t_res$p.value), 
                  options=list(dom='t', autoWidth=TRUE, columnDefs = list(list(width = '65%', targets = 0), list(width = '35%', targets = 1))), rownames=FALSE) %>% 
          formatSignif('p-value', digits=4)
      } else {
        tuk_res <- TukeyHSD(aov(Value ~ Group, data = df), "Group")$Group
        clean_tukey <- as_tibble(tuk_res, rownames="Comparison Matrix") %>% 
          select(`Comparison Matrix`, `p adj`)
        
        datatable(clean_tukey, 
                  options=list(dom='t', autoWidth=TRUE, columnDefs = list(list(width = '65%', targets = 0), list(width = '35%', targets = 1))), rownames=FALSE) %>% 
          formatSignif('p adj', digits=4)
      }
    })
  })
}