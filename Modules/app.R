# =========================================================================
# APEX ANALYTICS PLATFORM - MASTER LOGISTICAL ROUTER WITH THEME SWITCHER
# =========================================================================
library(shiny)
library(tidyverse)
library(janitor)
library(DT)
library(bslib)
library(shinyjs)
library(colourpicker)
library(patchwork)
library(ragg)

# --- 1. Define the Light and Dark Specifications ---
light_theme <- bs_theme(
  version = 5,
  bg = "#F4F3EF", fg = "#231F20",
  primary = "#4A5343", secondary = "#E6E4DD", success = "#D9A05B",
  base_font = font_google("Inter")
)

dark_theme <- bs_theme(
  version = 5,
  bg = "#17191E", fg = "#E2E8F0",
  primary = "#637365", secondary = "#2D323E", success = "#C9A66B",
  base_font = font_google("Inter")
)

# Injects external standalone functional tracking engines into session
source("global_metadata.R", local = TRUE)
source("engine_dashboard.R", local = TRUE)
source("engine_pwv.R", local = TRUE)
source("engine_bp.R", local = TRUE)
source("engine_weight.R", local = TRUE)
source("engine_recipes.R", local = TRUE)
source("engine_registry.R", local = TRUE)
source("engine_discovery.R", local = TRUE)
source("engine_cannulation.R", local = TRUE)
source("engine_WB.R", local = TRUE)
source("utils_apex_figures.R", local = TRUE)    # Central Publication Engine
source("engine_figure_studio.R", local = TRUE)  # Publication Figure Studio Module

# --- Master Front-End User Shell ---
ui <- fluidPage(
  useShinyjs(),
  theme = light_theme, # Binds the global design matrix to the browser window
  
  # Global CSS Overrides to fix component visibility
  tags$head(
    tags$style(HTML("
      /* Fix Secondary Buttons (Dismiss, Cancel, Export, Browse, Customize) */
      .btn-secondary, .btn-default, #browse, .fileinput-button, [id*='dismiss'], [id*='cancel'] {
        background-color: #E6E4DD !important;
        color: #231F20 !important;               /* Force text to Deep Chocolate */
        border: 1px solid #4A5343 !important;   /* Outline with Muted Olive */
        font-weight: bold !important;
      }
      .btn-secondary:hover, .btn-default:hover {
        background-color: #4A5343 !important;   /* Shift to Olive on hover */
        color: #F4F3EF !important;              /* Text turns light on hover */
      }
      
      /* Fix Animal Registry Profile Button Styling Error */
      [id*='profile'], .profile-btn {
        background-color: #4A5343 !important;   /* Force Profile button to Olive */
        color: #F4F3EF !important;
        border-color: #231F20 !important;
        opacity: 1 !important;
      }
      
      /* Fix Popup Modal Action Buttons (Add Target Row, Dismiss) */
      .modal-footer .btn, .modal-body .btn {
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      }
      
      /* ========================================== */
      /* GLOBAL CARD STYLING FOR ALL MODULES        */
      /* ========================================== */
      .dash-card {
        border: 1px solid #e3e6f0;
        border-radius: 8px !important;
        background: white;
        padding: 15px;
        margin-bottom: 20px;
        box-shadow: 0 4px 6px rgba(0,0,0,0.04), 0 1px 3px rgba(0,0,0,0.02); /* Clean, ultra-soft dual shadow */
        text-align: center;
        overflow: hidden !important; /* Cookie-cuts plot corners to stay rounded */
      }
    "))
  ),
  
  titlePanel("🔬 Apex Analytics Platform"),
  hr(),
  
  # Centralized global tracking context header
  ui_global_header(),
  
  tabsetPanel(
    id = "master_tabs",
    type = "pills", 
    
    tabPanel("📊 Dashboard", ui_dashboard_layout()),
    tabPanel("🎯 Phenotypic Discovery", ui_discovery_layout()),
    tabPanel("🏎️ Pulse Wave Velocity", ui_pwv_layout()),
    tabPanel("🩺 CODA 6 Blood Pressure", ui_bp_layout()),
    tabPanel("🩺 Cannulation Plots", ui_cannulation_layout()),
    tabPanel("🐀 Animal Profile Registry", ui_registry_layout()),
    tabPanel("⚖️ Weight Tracking", ui_weight_layout()),
    tabPanel("🧪 Western Blotting", ui_western_layout()),
    tabPanel("🎨 Figure Studio", ui_figure_studio_layout()),
    tabPanel("📑 Reagent Recipes", ui_recipes_layout())
  )
)

# --- Master Server Logic Shell ---
server <- function(input, output, session) {
  
  vars <- new.env(parent = emptyenv())
  
  # -- Dynamic Graphics Sync Bridge --
  is_dark_mode <- reactive({
    !is.null(input$theme_switch_btn) && input$theme_switch_btn == "dark"
  })
  
  # Global Plot Customization reactive overrides
  pwv_titles    <- reactiveValues(title = "Pulse Wave Velocity Stiffness Profile", xlab = "Timeline (Weeks)", ylab = "Velocity (m/s)")
  bp_titles     <- reactiveValues(title = "Systolic Blood Pressure Profile", xlab = "Timeline (Weeks)", ylab = "Systolic BP (mmHg)")
  can_titles    <- reactiveValues(title = "Terminal Carotid Cannulation Pressures", ylab = "Mean Arterial Pressure (mmHg)")
  weight_titles <- reactiveValues(title = "Longitudinal Animal Body Weight Trajectory", xlab = "Study Timeline / Milestones", ylab = "Body Mass (Grams)")
  
  cannulation_db_path <- "cannulation_terminal_database.csv"
  initial_can_db <- if (file.exists(cannulation_db_path)) {
    read_csv(cannulation_db_path, col_types = cols(Project = col_character(), Animal_ID = col_character(), Group = col_character(), Value = col_double()))
  } else {
    tibble(Project = character(), Animal_ID = character(), Group = character(), Value = numeric())
  }
  vars$cannulation_historical_data <- reactiveVal(initial_can_db)
  
  # Execute background runtime tracking loops for individual sub-modules
  server_global_metadata(input, output, session, vars)
  server_dashboard_logic(input, output, session, vars)
  server_pwv_logic(input, output, session, vars, pwv_titles)
  server_bp_logic(input, output, session, vars, bp_titles)
  server_cannulation_logic(input, output, session, vars, can_titles)
  server_registry_logic(input, output, session, vars)
  server_weight_logic(input, output, session, vars, weight_titles)
  server_western_logic(input, output, session, vars)
  server_discovery_logic(input, output, session, vars)
  server_figure_studio_logic(input, output, session, vars)
  
  # Independent Recipe Engine execution (not passing vars = fully decoupled from project ID)
  server_recipes_logic(input, output, session)
}

shinyApp(ui = ui, server = server)