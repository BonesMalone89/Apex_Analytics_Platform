# =========================================================================
# APEX PLATFORM PHENOTYPIC DISCOVERY & ADVANCED MODELING MODULE (HYBRID)
# =========================================================================

# --- UI Layout Component ---
ui_discovery_layout <- function() {
  tagList(
    tags$head(
      tags$style(HTML("
        .discovery-sidebar { background: #E6E4DD; padding: 20px; border-radius: 6px; border: 1px solid #4A5343; }
        .stats-dossier-card { background: #FFFDF9; border-left: 4px solid #D9A05B; padding: 15px; border-radius: 4px; margin-top: 15px; }
        .equation-box { font-family: 'Courier New', Courier, monospace; font-weight: bold; background: #231F20; color: #D9A05B; padding: 10px; border-radius: 4px; text-align: center; font-size: 14px; }
        .engine-select-box { background: #dfdcd3; padding: 10px; border-radius: 4px; margin-bottom: 15px; border-left: 3px solid #4A5343; }
      "))
    ),
    
    fluidRow(
      # Left Sidebar: Model Configuration Controls
      column(4,
             div(class = "discovery-sidebar",
                 tags$h4(tags$strong("🎯 Model Configuration")),
                 hr(style = "border-top: 1px solid #4A5343; opacity: 0.3;"),
                 
                 # 🚀 THE SAFESHIELD SELECTOR: Choose individual tracking vs group-level timepoint summaries
                 div(class = "engine-select-box",
                     selectInput("disc_engine_mode", "Analysis Data Engine:",
                                 choices = c("Individual Specimen Tracks" = "INDIVIDUAL",
                                             "Cross-Sectional (Group-Timepoint Means)" = "CROSS_SECTIONAL"),
                                 selected = "INDIVIDUAL"),
                     tags$p(style = "font-size: 11px; color: #555; margin-bottom: 0; font-style: italic;",
                            "Use Cross-Sectional mode if animal IDs are unlinked or missing across weeks.")
                 ),
                 
                 selectInput("disc_x_var", "X-Axis Independent Variable:",
                             choices = c("Systolic BP" = "BP", 
                                         "Pulse Wave Velocity" = "PWV", 
                                         "Body Weight" = "Weight",
                                         "Terminal MAP" = "MAP"),
                             selected = "BP"),
                 
                 selectInput("disc_y_var", "Y-Axis Dependent Variable:",
                             choices = c("Pulse Wave Velocity" = "PWV", 
                                         "Systolic BP" = "BP", 
                                         "Body Weight" = "Weight",
                                         "Terminal MAP" = "MAP"),
                             selected = "PWV"),
                 
                 selectInput("disc_time_slice", "Timeline Scope Slice:",
                             choices = c("All Study Points" = "ALL",
                                         "Baseline/Arrival Only" = "Baseline/Arrival",
                                         "Pre-Op Phase" = "Pre-Op",
                                         "Post-Op Phase" = "Post-Op",
                                         "Terminal Harvest Only" = "Terminal Harvest")),
                 
                 hr(style = "border-top: 1px solid #4A5343; opacity: 0.3;"),
                 tags$h5(tags$strong("Stratification Settings")),
                 
                 uiOutput("conditional_stratify_ui"), # Dynamic rendering to match engine mode
                 checkboxInput("disc_show_se", "Display 95% Confidence Intervals", value = TRUE)
             )
      ),
      
      # Right Body Column: High-Resolution Plotting & Modeling Output
      column(8,
             div(class = "dash-card",
                 tags$h4(tags$strong("Global Phenotypic Mapping Space")),
                 plotOutput("discovery_regression_plot", height = "450px")
             ),
             
             div(class = "stats-dossier-card",
                 tags$h4(tags$strong("🔬 Statistical Modeling Readout")),
                 hr(style = "opacity: 0.15; margin: 8px 0;"),
                 uiOutput("discovery_stats_readout")
             )
      )
    )
  )
}

# --- Server Logic Component ---
server_discovery_logic <- function(input, output, session, vars) {
  
  # --- Dynamic UI Control for Stratification ---
  output$conditional_stratify_ui <- renderUI({
    if (req(input$disc_engine_mode) == "CROSS_SECTIONAL") {
      # In cross-sectional mode, data points *are* groups, so stratification is structural by default
      shinyjs::disabled(checkboxInput("disc_stratify_groups", "Stratify Regression by Cohort Group", value = TRUE))
    } else {
      checkboxInput("disc_stratify_groups", "Stratify Regression by Cohort Group", value = TRUE)
    }
  })
  
  # --- 1. Gather Raw Project Data Streams ---
  bp_raw  <- reactive({ vars$bp_historical_data() %>% filter(Project == input$global_project) })
  pwv_raw <- reactive({ vars$pwv_historical_data() %>% filter(Project == input$global_project) })
  wt_raw  <- reactive({ vars$weight_historical_data() %>% filter(Project == input$global_project) })
  can_raw <- reactive({ vars$cannulation_historical_data() %>% filter(Project == input$global_project) })
  
  # --- 2. Dual-Engine Hybrid Data Compiler ---
  discovery_dataset <- reactive({
    df_bp  <- bp_raw()
    df_pwv <- pwv_raw()
    df_wt  <- wt_raw()
    can    <- can_raw()
    
    req(input$disc_x_var, input$disc_y_var, input$disc_time_slice, input$disc_engine_mode)
    
    meta_colors <- vars$metadata_registry() %>% 
      filter(Project_Key == input$global_project) %>% 
      distinct(Group_Key, Group_Color)
    
    bp_tidy <- if(nrow(df_bp) > 0) {
      df_bp %>% select(Animal_ID, Timeline, BP = Mean_Systolic, Group_BP = Group, Color_BP = Group_Color)
    } else {
      tibble(Animal_ID = character(), Timeline = character(), BP = numeric(), Group_BP = character(), Color_BP = character())
    }
    
    pwv_tidy <- if(nrow(df_pwv) > 0) {
      df_pwv %>% select(Animal_ID = Rat_ID, Timeline, PWV = Velocity_ms, Group_PWV = Group, Color_PWV = Group_Color)
    } else {
      tibble(Animal_ID = character(), Timeline = character(), PWV = numeric(), Group_PWV = character(), Color_PWV = character())
    }
    
    wt_tidy <- if(nrow(df_wt) > 0) {
      df_wt %>% 
        mutate(Timeline_Clean = case_when(
          Timeline == "Baseline/Arrival" ~ "Baseline/Arrival",
          Timeline == "Pre-Op" ~ "Pre-Op",
          Timeline == "Post-Op" ~ "Post-Op",
          Timeline == "Terminal Harvest" ~ "Terminal Harvest",
          TRUE ~ str_replace(Timeline, "Week ", "W")
        )) %>%
        transmute(Animal_ID, Timeline = Timeline_Clean, Weight = Mass_g, Group_WT = Group, Color_WT = Group_Color)
    } else {
      tibble(Animal_ID = character(), Timeline = character(), Weight = numeric(), Group_WT = character(), Color_WT = character())
    }
    
    can_tidy <- if(nrow(can) > 0) {
      can %>% 
        left_join(meta_colors, by = c("Group" = "Group_Key")) %>%
        transmute(Animal_ID = as.character(Animal_ID), Timeline = "Terminal Harvest", MAP = Value, Group_CAN = Group, Color_CAN = Group_Color)
    } else {
      tibble(Animal_ID = character(), Timeline = character(), MAP = numeric(), Group_CAN = character(), Color_CAN = character())
    }
    
    all_animals   <- unique(c(bp_tidy$Animal_ID, pwv_tidy$Animal_ID, wt_tidy$Animal_ID, can_tidy$Animal_ID))
    all_timelines <- unique(c(bp_tidy$Timeline, pwv_tidy$Timeline, wt_tidy$Timeline, can_tidy$Timeline))
    
    if (length(all_animals) == 0 || length(all_timelines) == 0) return(tibble())
    
    backbone <- expand.grid(Animal_ID = all_animals, Timeline = all_timelines, stringsAsFactors = FALSE) %>% as_tibble()
    
    master_join <- backbone %>%
      left_join(bp_tidy, by = c("Animal_ID", "Timeline")) %>%
      left_join(pwv_tidy, by = c("Animal_ID", "Timeline")) %>%
      left_join(wt_tidy, by = c("Animal_ID", "Timeline")) %>%
      left_join(can_tidy, by = c("Animal_ID", "Timeline")) %>%
      mutate(
        Group = case_when(!is.na(Group_BP) ~ Group_BP, !is.na(Group_PWV) ~ Group_PWV, !is.na(Group_WT) ~ Group_WT, !is.na(Group_CAN) ~ Group_CAN, TRUE ~ "Unassigned"),
        Group_Color = case_when(!is.na(Color_BP) ~ Color_BP, !is.na(Color_PWV) ~ Color_PWV, !is.na(Color_WT) ~ Color_WT, !is.na(Color_CAN) ~ Color_CAN, TRUE ~ "#7f8c8d")
      )
    
    # 🎛️ FORK LINE: Execute data reduction based on Engine Type choice
    if (input$disc_engine_mode == "CROSS_SECTIONAL") {
      # ENGINE B: Group and collapse down to pure timepoint averages, neutralizing mismatched IDs
      cross_df <- master_join %>%
        group_by(Group, Group_Color, Timeline) %>%
        summarise(
          BP = mean(BP, na.rm = TRUE),
          PWV = mean(PWV, na.rm = TRUE),
          Weight = mean(Weight, na.rm = TRUE),
          MAP = mean(MAP, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        filter(!is.na(.data[[input$disc_x_var]]), !is.na(.data[[input$disc_y_var]]))
      
      if (input$disc_time_slice != "ALL") {
        cross_df <- cross_df %>% filter(Timeline == input$disc_time_slice)
      }
      return(cross_df)
      
    } else {
      # ENGINE A: Original within-subject matching pipeline
      slice <- input$disc_time_slice
      if (slice == "ALL") {
        slice_df <- master_join %>% 
          group_by(Animal_ID, Group, Group_Color) %>% 
          summarise(
            BP = mean(BP, na.rm = TRUE),
            PWV = mean(PWV, na.rm = TRUE),
            Weight = mean(Weight, na.rm = TRUE),
            MAP = mean(MAP, na.rm = TRUE),
            .groups = "drop"
          )
      } else {
        slice_df <- master_join %>% 
          filter(Timeline == slice) %>% 
          select(Animal_ID, Group, Group_Color, BP, PWV, Weight, MAP)
      }
      return(slice_df %>% filter(!is.na(.data[[input$disc_x_var]]), !is.na(.data[[input$disc_y_var]])))
    }
  })
  
  # --- 3. Render High-Resolution Regression Plot ---
  output$discovery_regression_plot <- renderPlot({
    df <- discovery_dataset()
    req(nrow(df) > 0, input$disc_x_var, input$disc_y_var)
    
    x_col <- input$disc_x_var
    y_col <- input$disc_y_var
    
    distinct_palette <- df %>% distinct(Group, Group_Color)
    palette_vector <- setNames(distinct_palette$Group_Color, distinct_palette$Group)
    
    g <- ggplot(df, aes(x = .data[[x_col]], y = .data[[y_col]]))
    
    # Check stratification logic safely across both modes
    stratify <- if(input$disc_engine_mode == "CROSS_SECTIONAL") TRUE else input$disc_stratify_groups
    
    if (stratify) {
      g <- g + 
        geom_point(aes(color = Group), size = 5.5, alpha = 0.9) +
        geom_smooth(aes(color = Group, fill = Group), method = "lm", 
                    se = input$disc_show_se, alpha = 0.15, linewidth = 1.5) +
        scale_color_manual(values = palette_vector) +
        scale_fill_manual(values = palette_vector)
    } else {
      g <- g + 
        geom_point(aes(color = Group), size = 4.5, alpha = 0.8) +
        geom_smooth(method = "lm", color = "black", linetype = "dashed",
                    se = input$disc_show_se, alpha = 0.1, linewidth = 1.5) +
        scale_color_manual(values = palette_vector)
    }
    
    g <- g + 
      theme_classic(base_size = 15) +
      theme(
        axis.text = element_text(color = "black", face = "bold"),
        axis.line = element_line(linewidth = 0.8, color = "black"),
        legend.position = "right"
      ) +
      labs(
        x = paste(input$disc_x_var, "(Independent Axis)"),
        y = paste(input$disc_y_var, "(Dependent Axis)"),
        title = paste("Kinetic Regression Space [", input$disc_engine_mode, "]:", input$disc_x_var, "vs", input$disc_y_var)
      )
    
    return(g)
  })
  
  # --- 4. Render Statistical Modeling Readout ---
  output$discovery_stats_readout <- renderUI({
    df <- discovery_dataset()
    req(nrow(df) > 0, input$disc_x_var, input$disc_y_var)
    
    x_col <- input$disc_x_var
    y_col <- input$disc_y_var
    
    stratify <- if(input$disc_engine_mode == "CROSS_SECTIONAL") TRUE else input$disc_stratify_groups
    
    if (!stratify) {
      # --- Pooled Linear Model ---
      fit <- lm(df[[y_col]] ~ df[[x_col]])
      sum_fit <- summary(fit)
      
      intercept <- round(coef(fit)[1], 3)
      slope <- round(coef(fit)[2], 3)
      r2 <- round(sum_fit$adj.r.squared, 4)
      p_val <- sum_fit$coefficients[2, 4]
      
      p_text <- if(p_val < 0.001) "p < 0.001 (Highly Significant)" else paste("p =", round(p_val, 4))
      
      tagList(
        fluidRow(
          column(6, 
                 tags$h5(tags$strong("Pooled Regression Formula:")),
                 div(class = "equation-box", paste0(y_col, " = (", slope, " * ", x_col, ") + ", intercept))
          ),
          column(6,
                 tags$p(tags$strong("Adjusted R²: "), r2),
                 tags$p(tags$strong("Model P-Value: "), p_text),
                 tags$p(style = "color: gray; font-style: italic;", 
                        "Note: This pooled model does not account for genetic groups. Watch out for Simpson's Paradox!")
          )
        )
      )
    } else {
      # --- Group Stratified Models ---
      groups <- unique(df$Group)
      
      readouts <- lapply(groups, function(grp) {
        sub_df <- df %>% filter(Group == grp)
        
        # Adjust minimum sizing boundary depending on structural mode selected
        min_n <- if(input$disc_engine_mode == "CROSS_SECTIONAL") 2 else 3
        if (nrow(sub_df) < min_n) {
          return(div(style="margin-bottom:10px;", tags$strong(grp), ": Insufficient historical coordinates for group modeling."))
        }
        
        fit_sub <- lm(sub_df[[y_col]] ~ sub_df[[x_col]])
        sum_sub <- summary(fit_sub)
        
        intercept_sub <- round(coef(fit_sub)[1], 3)
        slope_sub <- round(coef(fit_sub)[2], 3)
        r2_sub <- round(sum_sub$adj.r.squared, 4)
        p_val_sub <- sum_sub$coefficients[2, 4]
        
        p_text_sub <- if(p_val_sub < 0.001) "p < 0.001" else paste("p =", round(p_val_sub, 4))
        
        label_header <- if(input$disc_engine_mode == "CROSS_SECTIONAL") "Timepoint Trend:" else "Cohort Group:"
        
        div(style = "margin-bottom: 15px; padding: 10px; background: #fafafa; border-radius: 4px;",
            tags$h5(tags$strong(paste(label_header, grp))),
            fluidRow(
              column(6, div(class = "equation-box", style = "background:#4A5343; color:white;", paste0(y_col, " = (", slope_sub, " * ", x_col, ") + ", intercept_sub))),
              column(6, 
                     tags$span(tags$strong("Adj R²: "), r2_sub, " | "),
                     tags$span(tags$strong("P-Value: "), p_text_sub)
              )
            )
        )
      })
      
      tagList(readouts)
    }
  })
}