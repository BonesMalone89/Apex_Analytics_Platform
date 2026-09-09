# =========================================================================
# APEX PLATFORM: PUBLICATION FIGURE STUDIO MODULE (WITH LONGITUDINAL STATS)
# =========================================================================

library(shiny)
library(dplyr)
library(ggplot2)
library(patchwork)
library(ragg)
library(stringr)

# --- UI Component ---
ui_figure_studio_layout <- function() {
  sidebarLayout(
    sidebarPanel(
      tags$h4("🎨 Studio Canvas Composition"),
      selectInput("studio_layout_mode", "Figure Composition Grid:",
                  choices = c("1 Panel (Single Assay)"      = "1",
                              "2 Panels Horizontal (1 x 2)" = "1x2",
                              "2 Panels Vertical (2 x 1)"   = "2x1",
                              "4 Panels Grid (2 x 2)"       = "2x2",
                              "6 Panels Grid (2 x 3)"       = "2x3")),
      hr(),
      
      tags$h4("📐 Physical Journal Standards"),
      selectInput("studio_journal_target", "Target Journal Width:",
                  choices = c("Single Column (85 mm)" = "85",
                              "1.5 Column (120 mm)"   = "120",
                              "Double Column (175 mm)" = "175",
                              "Custom Dimensions"     = "custom")),
      fluidRow(
        column(6, numericInput("studio_width_in", "Width (in):", value = 6.89, min = 2, max = 15, step = 0.1)),
        column(6, numericInput("studio_height_in", "Height (in):", value = 5.5, min = 2, max = 15, step = 0.1))
      ),
      hr(),
      
      tags$h4("🔬 Typography & Error Dispersion"),
      sliderInput("studio_base_font_size", "Base Typography (pt):", min = 7, max = 14, value = 9, step = 0.5),
      radioButtons("studio_dispersion", "Error Dispersion:",
                   choices = c("Mean ± SD (Biological Variance)" = "SD",
                               "Mean ± SEM (Inferential Precision)" = "SEM"),
                   selected = "SEM", inline = TRUE),
      checkboxInput("studio_show_pvals", "Compute Integrated Significance & Asterisks", value = TRUE),
      hr(),
      
      tags$h4("📊 Assay Panel Mapping"),
      uiOutput("studio_panel_selectors_ui"),
      hr(),
      
      tags$h4("💾 Publication Vector Export"),
      selectInput("studio_export_format", "Format:", choices = c("PDF (Vector Graphic)" = "pdf", "TIFF (600 DPI LZW)" = "tiff", "PNG (300 DPI)" = "png")),
      downloadButton("studio_download_figure_btn", "Export Publication Figure", class = "btn-success btn-block")
    ),
    
    mainPanel(
      tags$h4("📑 Interactive Proof Canvas"),
      tags$p(tags$small(tags$em("Live multi-panel vector proof. Trajectory tracks include 2-Way ANOVA Interaction p-values and per-timepoint post-hoc asterisks (*p<0.05, **p<0.01, ***p<0.001)."))),
      br(),
      div(class = "dash-card", style = "background-color: #ffffff; padding: 15px; border-radius: 8px; box-shadow: 0 4px 6px rgba(0,0,0,0.08);",
          plotOutput("studio_live_canvas_plot", height = "580px")
      )
    )
  )
}

# --- Server Component ---
server_figure_studio_logic <- function(input, output, session, vars, ...) {
  
  # Auto-update physical dimensions when journal target changes
  observeEvent(input$studio_journal_target, {
    req(input$studio_journal_target != "custom")
    if (input$studio_journal_target == "85") {
      updateNumericInput(session, "studio_width_in", value = 3.35)
      updateNumericInput(session, "studio_height_in", value = 3.20)
    } else if (input$studio_journal_target == "120") {
      updateNumericInput(session, "studio_width_in", value = 4.72)
      updateNumericInput(session, "studio_height_in", value = 4.00)
    } else if (input$studio_journal_target == "175") {
      updateNumericInput(session, "studio_width_in", value = 6.89)
      updateNumericInput(session, "studio_height_in", value = 5.50)
    }
  })
  
  # Assay Catalog
  assay_choices <- list(
    "Longitudinal Physiological Trajectories" = c(
      "Aortic Pulse Wave Velocity (PWV)"  = "PWV_TRAJ",
      "Tail-Cuff Systolic BP (CODA 6)"    = "BP_TRAJ",
      "Longitudinal Body Weight"          = "WEIGHT_TRAJ"
    ),
    "Terminal Surgical Cannulation (Direct Pressures)" = c(
      "Direct Systolic BP (SBP)"          = "CAN_SBP",
      "Direct Diastolic BP (DBP)"         = "CAN_DBP",
      "Mean Arterial Pressure (MAP)"      = "CAN_MAP",
      "Pulse Pressure (PP)"               = "CAN_PP",
      "Heart Rate (HR)"                   = "CAN_HR",
      "Inotropy (Max dP/dt)"              = "CAN_Max_dPdt",
      "Arterial Runoff (Min dP/dt)"       = "CAN_Min_dPdt",
      "Myocardial Workload (PTI)"         = "CAN_PTI"
    )
  )
  
  # Dynamic panel assignment dropdowns
  output$studio_panel_selectors_ui <- renderUI({
    mode <- input$studio_layout_mode
    num_panels <- switch(mode, "1" = 1, "1x2" = 2, "2x1" = 2, "2x2" = 4, "2x3" = 6, 1)
    panel_letters <- LETTERS[1:num_panels]
    
    defaults <- c("BP_TRAJ", "PWV_TRAJ", "CAN_MAP", "CAN_Min_dPdt", "WEIGHT_TRAJ", "CAN_PP")
    
    lapply(seq_along(panel_letters), function(i) {
      fluidRow(
        column(12, selectInput(paste0("studio_metric_panel_", panel_letters[i]),
                               paste0("Panel ", panel_letters[i], " Assay Target:"),
                               choices = assay_choices,
                               selected = defaults[min(i, length(defaults))]))
      )
    })
  })
  
  # --- Robust Longitudinal Plot Engine with Two-Way ANOVA & Post-Hoc Asterisks ---
  render_longitudinal_panel <- function(raw_df, target_metric_type, y_axis_title, meta, tx_week, pool_baseline, base_sz, disp, show_stats) {
    if (is.null(raw_df) || nrow(raw_df) == 0) {
      return(ggplot() + annotate("text", x = 1, y = 1, label = "No Longitudinal Records Available", fontface = "bold") + theme_void())
    }
    
    # Universal column resolver
    candidate_cols <- c("Value", "Weight_g", "Weight", "Body_Weight_g", "Mean_Systolic", "Velocity_ms", "Distance_mm")
    matched_col <- intersect(candidate_cols, colnames(raw_df))[1]
    
    if (is.na(matched_col)) {
      num_cols <- names(raw_df)[sapply(raw_df, is.numeric)]
      num_cols <- setdiff(num_cols, c("Distance_mm", "Time_Index", "Week_Num", "Sub_Num"))
      matched_col <- num_cols[1]
    }
    
    if (is.na(matched_col)) {
      return(ggplot() + annotate("text", x = 1, y = 1, label = "Measurement Column Unresolved", fontface = "bold") + theme_void())
    }
    
    clean_df <- raw_df %>% 
      filter(!is.na(.data[[matched_col]]), !is.na(Timeline)) %>% 
      mutate(Value = as.numeric(.data[[matched_col]])) %>% 
      select(-any_of("Group_Color")) %>% 
      left_join(meta %>% select(Group_Key, Group_Color) %>% distinct(), by = c("Group" = "Group_Key")) %>% 
      mutate(Group_Color = if_else(is.na(Group_Color), "#333333", Group_Color))
    
    if (nrow(clean_df) == 0) {
      return(ggplot() + annotate("text", x = 1, y = 1, label = "Insufficient Longitudinal Records", fontface = "bold") + theme_void())
    }
    
    # Trajectory Data Structuring
    traj <- prep_longitudinal_trajectory(clean_df, tx_week = tx_week, pool_baseline = pool_baseline)
    stats_df <- traj$stats
    
    # Dispersion calculation (SEM vs SD)
    clean_df_indexed <- clean_df %>% 
      mutate(
        Week_Num = as.numeric(str_extract(as.character(Timeline), "-?\\d+")),
        Week_Num = ifelse(is.na(Week_Num), 0, Week_Num),
        Sub_Num  = as.numeric(str_extract(as.character(if("SubRun" %in% colnames(.)) SubRun else "1"), "\\d+")),
        Sub_Num  = ifelse(is.na(Sub_Num), 1, Sub_Num),
        Time_Index = Week_Num + ((Sub_Num - 1) * 0.2)
      )
    
    if (disp == "SEM") {
      if (isTRUE(pool_baseline)) {
        n_base <- clean_df_indexed %>% filter(Week_Num < tx_week) %>% group_by(Time_Index) %>% summarise(N = n(), .groups = "drop") %>% mutate(Group = "Baseline (Pooled)")
        n_tx   <- clean_df_indexed %>% filter(Week_Num >= tx_week) %>% group_by(Group, Time_Index) %>% summarise(N = n(), .groups = "drop")
        n_all  <- bind_rows(n_base, n_tx)
      } else {
        n_all  <- clean_df_indexed %>% group_by(Group, Time_Index) %>% summarise(N = n(), .groups = "drop")
      }
      
      stats_df <- stats_df %>% 
        left_join(n_all, by = c("Group", "Time_Index")) %>% 
        mutate(
          N = if_else(is.na(N) | N < 1, 1L, N),
          disp_val = sd_val / sqrt(N)
        )
    } else {
      stats_df <- stats_df %>% mutate(disp_val = sd_val)
    }
    
    # --- Statistical Analysis Layer ---
    anova_label <- ""
    sig_annotations <- tibble(Time_Index = numeric(), y_pos = numeric(), label = character())
    
    groups_present <- unique(clean_df_indexed$Group)
    
    if (show_stats && length(groups_present) == 2) {
      # 1. Two-Way ANOVA Interaction (Group x Timeline)
      anova_data <- clean_df_indexed %>% filter(Week_Num >= tx_week)
      if (nrow(anova_data) >= 4 && length(unique(anova_data$Timeline)) > 1) {
        aov_fit <- tryCatch(aov(Value ~ Group * factor(Timeline), data = anova_data), error = function(e) NULL)
        if (!is.null(aov_fit)) {
          aov_tab <- summary(aov_fit)[[1]]
          int_row_idx <- grep("Group:factor\\(Timeline\\)|Group:Timeline", rownames(aov_tab), ignore.case = TRUE)
          if (length(int_row_idx) > 0) {
            p_int <- aov_tab$`Pr(>F)`[int_row_idx[1]]
            if (!is.na(p_int)) {
              stars <- if (p_int < 0.001) "***" else if (p_int < 0.01) "**" else if (p_int < 0.05) "*" else "ns"
              anova_label <- if (p_int < 0.001) "2-Way ANOVA Int. p < 0.001 ***" else paste0("2-Way ANOVA Int. p = ", sprintf("%.3f", p_int), " ", stars)
            }
          }
        }
      }
      
      # 2. Per-Timepoint Post-Hoc Welch's T-Tests during Treatment Phase
      treatment_weeks <- unique(clean_df_indexed$Timeline[clean_df_indexed$Week_Num >= tx_week])
      sig_list <- list()
      
      for (wk in treatment_weeks) {
        sub_df <- clean_df_indexed %>% filter(Timeline == wk)
        g1_v <- sub_df %>% filter(Group == groups_present[1]) %>% pull(Value)
        g2_v <- sub_df %>% filter(Group == groups_present[2]) %>% pull(Value)
        
        if (length(g1_v) >= 2 && length(g2_v) >= 2) {
          t_res <- tryCatch(t.test(g1_v, g2_v, var.equal = FALSE), error = function(e) NULL)
          if (!is.null(t_res) && !is.na(t_res$p.value) && t_res$p.value < 0.05) {
            pval <- t_res$p.value
            star_txt <- if (pval < 0.001) "***" else if (pval < 0.01) "**" else "*"
            
            # Find the exact Time_Index and the highest point/errorbar for this week
            t_idx <- sub_df$Time_Index[1]
            max_y_at_t <- max(stats_df$mean_val[stats_df$Timeline == wk] + stats_df$disp_val[stats_df$Timeline == wk], na.rm = TRUE)
            
            sig_list[[length(sig_list) + 1]] <- tibble(
              Time_Index = t_idx,
              y_pos = max_y_at_t,
              label = star_txt
            )
          }
        }
      }
      if (length(sig_list) > 0) sig_annotations <- bind_rows(sig_list)
    }
    
    # Calculate explicit y-limits to give top margin room for asterisks
    global_max_val <- max(stats_df$mean_val + stats_df$disp_val, na.rm = TRUE)
    global_min_val <- min(stats_df$mean_val - stats_df$disp_val, na.rm = TRUE)
    data_span <- max(global_max_val - global_min_val, 1)
    
    y_upper <- if (nrow(sig_annotations) > 0) (global_max_val + 0.18 * data_span) else (global_max_val + 0.08 * data_span)
    y_lower <- max(0, global_min_val - 0.08 * data_span)
    
    # Adjust asterisk position to sit in the dedicated headspace
    if (nrow(sig_annotations) > 0) {
      sig_annotations <- sig_annotations %>% mutate(y_pos = y_pos + (0.06 * data_span))
    }
    
    # Build clean line plot without duplicate transition points
    p <- ggplot() +
      geom_line(data = stats_df, 
                aes(x = Time_Index, y = mean_val, color = Group, group = Group), 
                linewidth = 1.5) +
      geom_errorbar(data = stats_df %>% filter(!is_anchor), 
                    aes(x = Time_Index, ymin = mean_val - disp_val, ymax = mean_val + disp_val, color = Group), 
                    width = 0.14, linewidth = 1.0) +
      geom_point(data = stats_df %>% filter(!is_anchor), 
                 aes(x = Time_Index, y = mean_val, color = Group), 
                 size = 2.8) +
      scale_color_manual(values = traj$palette, name = "Cohort") +
      labs(
        x = "Study Timeline (Weeks)", 
        y = y_axis_title,
        subtitle = if (anova_label != "") anova_label else NULL
      ) +
      coord_cartesian(ylim = c(y_lower, y_upper)) +
      theme_apex_publication(base_size = base_sz)
      theme(
        plot.subtitle = element_text(face = "bold.italic", size = rel(0.85), color = "#2c3e50", hjust = 0.5, margin = margin(b = 4))
      )
    
    # Add Treatment Intervention Line
    if (tx_week > 0) {
      p <- p + geom_vline(xintercept = tx_week, linetype = "dashed", color = "gray50", linewidth = 0.5)
    }
    
    # Overlay Post-Hoc Asterisks
    if (nrow(sig_annotations) > 0) {
      p <- p + geom_text(data = sig_annotations, aes(x = Time_Index, y = y_pos, label = label),
                         color = "black", fontface = "bold", size = (base_sz / 2.2), inherit.aes = FALSE)
    }
    
    return(p)
  }
  
  # Universal Dispatcher Plot Generator
  build_panel_plot <- function(target_key, panel_letter) {
    req(input$global_project)
    
    meta <- vars$metadata_registry() %>% filter(Project_Key == input$global_project)
    req(nrow(meta) > 0)
    
    tx_week       <- if ("Tx_Start_Week" %in% colnames(meta)) meta$Tx_Start_Week[1] else 1
    pool_baseline <- if ("Pool_Baseline" %in% colnames(meta)) isTRUE(meta$Pool_Baseline[1]) else TRUE
    
    palette_vec <- setNames(meta$Group_Color, meta$Group_Key)
    base_sz     <- input$studio_base_font_size
    disp        <- input$studio_dispersion
    show_p      <- input$studio_show_pvals
    
    # --- ROUTING A: Terminal Cannulation (Direct Carotid) ---
    if (grepl("^CAN_", target_key)) {
      df <- vars$cannulation_historical_data() %>% 
        filter(Project == input$global_project) %>% 
        mutate(across(any_of(c("SBP", "DBP", "MAP", "PP", "HR", "Max_dPdt", "Min_dPdt", "PTI")), as.numeric))
      
      metric <- gsub("^CAN_", "", target_key)
      if (nrow(df) == 0 || !(metric %in% colnames(df))) {
        return(ggplot() + annotate("text", x = 1, y = 1, label = "No Cannulation Data Available", fontface = "bold") + theme_void())
      }
      
      y_labels <- c(
        "SBP" = "Systolic BP (mmHg)", "DBP" = "Diastolic BP (mmHg)",
        "MAP" = "Mean Arterial Pressure (mmHg)", "PP" = "Pulse Pressure (mmHg)",
        "HR" = "Heart Rate (BPM)", "Max_dPdt" = "Max dP/dt (mmHg/s)",
        "Min_dPdt" = "Min dP/dt (mmHg/s)", "PTI" = "PTI (mmHg·s)"
      )
      
      titles <- c(
        "SBP" = "Terminal SBP", "DBP" = "Terminal DBP", "MAP" = "Terminal MAP",
        "PP" = "Pulse Pressure", "HR" = "Heart Rate", "Max_dPdt" = "Inotropy (Max dP/dt)",
        "Min_dPdt" = "Runoff (Min dP/dt)", "PTI" = "Workload (PTI)"
      )
      
      return(apex_plot_endpoint(
        df = df, x_col = "Group", y_col = metric, palette = palette_vec,
        y_title = y_labels[[metric]], plot_title = titles[[metric]],
        dispersion = disp, show_p_val = show_p, base_size = base_sz
      ))
    }
    
    # --- ROUTING B: Pulse Wave Velocity (Trajectory) ---
    if (target_key == "PWV_TRAJ") {
      raw_df <- vars$pwv_historical_data() %>% filter(Project == input$global_project)
      return(render_longitudinal_panel(raw_df, "Velocity_ms", "PWV (m/s)", meta, tx_week, pool_baseline, base_sz, disp, show_p))
    }
    
    # --- ROUTING C: Longitudinal Tail-Cuff BP (Trajectory) ---
    if (target_key == "BP_TRAJ") {
      raw_df <- vars$bp_historical_data() %>% filter(Project == input$global_project)
      return(render_longitudinal_panel(raw_df, "Mean_Systolic", "Systolic BP (mmHg)", meta, tx_week, pool_baseline, base_sz, disp, show_p))
    }
    
    # --- ROUTING D: Longitudinal Body Weight (Trajectory) ---
    if (target_key == "WEIGHT_TRAJ") {
      raw_df <- if (!is.null(vars$weight_historical_data)) vars$weight_historical_data() %>% filter(Project == input$global_project) else tibble()
      return(render_longitudinal_panel(raw_df, "Weight", "Body Mass (g)", meta, tx_week, pool_baseline, base_sz, disp, show_p))
    }
  }
  
  # Composite Multi-Panel Assembly via Patchwork
  assembled_figure <- reactive({
    req(input$studio_layout_mode)
    mode <- input$studio_layout_mode
    
    if (mode == "1") {
      req(input$studio_metric_panel_A)
      pA <- build_panel_plot(input$studio_metric_panel_A, "A")
      return(pA + plot_annotation(tag_levels = 'A') & theme(plot.tag = element_text(face = "bold", size = 11)))
    }
    
    if (mode %in% c("1x2", "2x1")) {
      req(input$studio_metric_panel_A, input$studio_metric_panel_B)
      pA <- build_panel_plot(input$studio_metric_panel_A, "A")
      pB <- build_panel_plot(input$studio_metric_panel_B, "B")
      
      comp <- if (mode == "1x2") (pA | pB) else (pA / pB)
      return(comp + plot_layout(guides = "collect") + plot_annotation(tag_levels = 'A') & theme(plot.tag = element_text(face = "bold", size = 11)))
    }
    
    if (mode == "2x2") {
      req(input$studio_metric_panel_A, input$studio_metric_panel_B, input$studio_metric_panel_C, input$studio_metric_panel_D)
      pA <- build_panel_plot(input$studio_metric_panel_A, "A")
      pB <- build_panel_plot(input$studio_metric_panel_B, "B")
      pC <- build_panel_plot(input$studio_metric_panel_C, "C")
      pD <- build_panel_plot(input$studio_metric_panel_D, "D")
      
      return((pA | pB) / (pC | pD) + plot_layout(guides = "collect") + plot_annotation(tag_levels = 'A') & theme(plot.tag = element_text(face = "bold", size = 11)))
    }
    
    if (mode == "2x3") {
      req(input$studio_metric_panel_A, input$studio_metric_panel_B, input$studio_metric_panel_C, 
          input$studio_metric_panel_D, input$studio_metric_panel_E, input$studio_metric_panel_F)
      pA <- build_panel_plot(input$studio_metric_panel_A, "A")
      pB <- build_panel_plot(input$studio_metric_panel_B, "B")
      pC <- build_panel_plot(input$studio_metric_panel_C, "C")
      pD <- build_panel_plot(input$studio_metric_panel_D, "D")
      pE <- build_panel_plot(input$studio_metric_panel_E, "E")
      pF <- build_panel_plot(input$studio_metric_panel_F, "F")
      
      return((pA | pB | pC) / (pD | pE | pF) + plot_layout(guides = "collect") + plot_annotation(tag_levels = 'A') & theme(plot.tag = element_text(face = "bold", size = 11)))
    }
  })
  
  # Live Proof Rendering
  output$studio_live_canvas_plot <- renderPlot({
    assembled_figure()
  })
  
  # High-Resolution Publication Exporter
  output$studio_download_figure_btn <- downloadHandler(
    filename = function() {
      paste0("Figure_", input$global_project, "_", Sys.Date(), ".", input$studio_export_format)
    },
    content = function(file) {
      fig <- assembled_figure()
      w <- input$studio_width_in
      h <- input$studio_height_in
      fmt <- input$studio_export_format
      
      if (fmt == "pdf") {
        cairo_pdf(file, width = w, height = h)
        print(fig)
        dev.off()
      } else if (fmt == "tiff") {
        ragg::agg_tiff(file, width = w, height = h, units = "in", res = 600, compression = "lzw")
        print(fig)
        dev.off()
      } else if (fmt == "png") {
        ragg::agg_png(file, width = w, height = h, units = "in", res = 300)
        print(fig)
        dev.off()
      }
    }
  )
}