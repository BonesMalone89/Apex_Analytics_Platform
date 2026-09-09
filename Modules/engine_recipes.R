library(shiny)
library(dplyr)
library(jsonlite)
library(purrr)

# =========================================================================
# APEX REAGENT RECIPE & PROTOCOL ENGINE (CARD GRID + DELETE FEATURE)
# =========================================================================

ui_recipes_layout <- function() {
  tagList(
    div(class = "well dash-card", style = "background-color: #ffffff; border-left: 5px solid #D9A05B; text-align: left;",
        fluidRow(
          column(7, 
                 h4(strong("🧪 APEX Reagent Recipe & Protocol Engine"), style = "color: #231F20; margin: 0 0 5px 0;"),
                 p(tags$small("Dynamic batch volume scaling and global bench solution registry."), style = "color: #7f8c8d; margin: 0;")
          ),
          column(5, align = "right", 
                 actionButton("rec_btn_add_recipe", "➕ Create New Recipe", class = "btn-secondary", style = "font-weight: bold;")
          )
        )
    ),
    
    # Active View: Visual Cards Grid + Dynamic Volume Scaler
    uiOutput("rec_recipes_display_container")
  )
}

server_recipes_logic <- function(input, output, session, db_path = "recipes_database.json") {
  
  # Dynamic Row Indices for Modal & Selected Recipe State
  row_indices <- reactiveVal(1:2)
  selected_recipe_id <- reactiveVal(NULL)
  recipe_search_query <- reactiveVal("")
  
  # Stored Recipes reactive (loaded from local JSON)
  recipes_db <- reactiveVal({
    if (file.exists(db_path)) {
      fromJSON(db_path, simplifyDataFrame = FALSE)
    } else {
      list()
    }
  })
  
  # Auto-select first recipe if selection is lost or upon initial load
  observe({
    db <- recipes_db()
    curr <- selected_recipe_id()
    if (length(db) > 0 && (is.null(curr) || !any(map_chr(db, ~ .x$recipe_id) == curr))) {
      selected_recipe_id(db[[1]]$recipe_id)
    }
  })
  
  # Update search query state
  observeEvent(input$rec_search_input, {
    recipe_search_query(tolower(trimws(input$rec_search_input)))
  })
  
  # --- RENDER MAIN UI CONTAINER ---
  output$rec_recipes_display_container <- renderUI({
    db <- recipes_db()
    
    if (length(db) == 0) {
      div(class = "well dash-card", style = "text-align: center; padding: 40px;",
          h4(strong("No Recipes in Database Yet"), style = "color: #7f8c8d;"),
          p("Click the button below to create your first reagent or media protocol."),
          actionButton("rec_btn_add_first_recipe", "➕ Add Your First Recipe", class = "btn-secondary", style = "margin-top: 10px;")
      )
    } else {
      query <- recipe_search_query()
      
      # Filter recipes by search query
      filtered_db <- if (nchar(query) > 0) {
        keep(db, function(r) {
          grepl(query, tolower(r$recipe_name)) || 
            grepl(query, tolower(r$category)) ||
            any(map_lgl(r$components, ~ grepl(query, tolower(.x$reagent_name))))
        })
      } else {
        db
      }
      
      fluidRow(
        # LEFT COLUMN: Recipe Browser Cards & Search
        column(5,
               div(class = "well dash-card", style = "text-align: left; background-color: #ffffff; padding: 15px;",
                   h5(strong("📚 Recipe Library"), style = "color: #231F20; margin-top: 0;"),
                   textInput("rec_search_input", NULL, value = recipe_search_query(), placeholder = "🔍 Search recipes or components..."),
                   hr(style = "margin: 10px 0;"),
                   
                   if (length(filtered_db) == 0) {
                     p("No matching recipes found.", style = "color: #7f8c8d; font-style: italic;")
                   } else {
                     div(style = "max-height: 550px; overflow-y: auto; padding-right: 5px;",
                         map(filtered_db, function(rec) {
                           is_selected <- !is.null(selected_recipe_id()) && selected_recipe_id() == rec$recipe_id
                           card_border <- if (is_selected) "2px solid #4A5343" else "1px solid #e3e6f0"
                           card_bg <- if (is_selected) "#F4F3EF" else "#ffffff"
                           
                           div(
                             style = sprintf("border: %s; background-color: %s; border-radius: 6px; padding: 12px; margin-bottom: 10px; cursor: pointer; transition: all 0.2s;", card_border, card_bg),
                             onclick = sprintf("Shiny.setInputValue('rec_select_card_id', '%s', {priority: 'event'})", rec$recipe_id),
                             fluidRow(
                               column(8, strong(rec$recipe_name), style = "color: #231F20; font-size: 14px;"),
                               column(4, align = "right", 
                                      span(rec$category, class = "badge", style = "background-color: #4A5343; color: #ffffff; font-size: 10px;")
                               )
                             ),
                             p(tags$small(paste("Solvent:", rec$base_solvent, "| Components:", length(rec$components))), 
                               style = "color: #7f8c8d; margin: 5px 0 0 0;")
                           )
                         })
                     )
                   }
               )
        ),
        
        # RIGHT COLUMN: Active Scaler Workbench & Instructions
        column(7,
               div(class = "well dash-card", style = "text-align: left; background-color: #ffffff; padding: 15px;",
                   uiOutput("rec_scaled_recipe_details")
               )
        )
      )
    }
  })
  
  # Select card on click
  observeEvent(input$rec_select_card_id, {
    selected_recipe_id(input$rec_select_card_id)
  })
  
  # Trigger Modal Event Listeners
  observeEvent(input$rec_btn_add_first_recipe, {
    shinyjs::click("rec_btn_add_recipe")
  })
  
  observeEvent(input$rec_btn_add_recipe, {
    row_indices(1:2)
    showModal(modalDialog(
      title = div(strong("➕ Add New Reagent Recipe"), style = "color: #231F20;"),
      size = "l",
      easyClose = FALSE,
      
      div(style = "padding: 5px;",
          fluidRow(
            column(6, textInput("rec_input_name", "Recipe Name:", placeholder = "e.g., Complete DMEM Growth Media")),
            column(6, selectInput("rec_input_cat", "Category:", 
                                  choices = c("Cell Culture", "Fixatives & Stains", "Buffers & Wash", "Enzymatic Solutions", "Custom Reagents")))
          ),
          
          textAreaInput("rec_input_notes", "Storage, Shelf Life & Additional Notes:", 
                        placeholder = "e.g., Store at 4°C protected from light. Warm to 37°C before use. pH to 7.4.",
                        rows = 2, width = "100%"),
          
          hr(style = "margin: 15px 0;"),
          h5(strong("Reagent Components")),
          p(tags$small(em("Enter stock and target concentrations for each component."))),
          
          uiOutput("rec_dynamic_component_rows"),
          
          actionButton("rec_btn_add_row", "➕ Add Another Reagent Row", class = "btn-secondary btn-xs", style = "margin-top: 5px;"),
          
          hr(style = "margin: 15px 0;"),
          
          textInput("rec_input_solvent", "Base Solvent / Top-Up Vehicle:", placeholder = "e.g., Basal DMEM Medium, Milli-Q H2O, 1x PBS")
      ),
      
      footer = tagList(
        modalButton("Cancel"),
        actionButton("rec_btn_save_recipe", "💾 Save Recipe to Engine", class = "btn-secondary", style = "font-weight: bold;")
      )
    ))
  })
  
  # Render Dynamic Component Input Rows in Modal
  output$rec_dynamic_component_rows <- renderUI({
    indices <- row_indices()
    map(indices, function(i) {
      div(key = paste0("rec_row_", i), style = "margin-bottom: 8px;",
          fluidRow(
            column(4, textInput(paste0("rec_comp_name_", i), NULL, placeholder = paste("Reagent", i, "Name"))),
            column(2, numericInput(paste0("rec_comp_stock_", i), NULL, value = 100, min = 0, step = 0.1)),
            column(2, numericInput(paste0("rec_comp_target_", i), NULL, value = 1, min = 0, step = 0.1)),
            column(3, selectInput(paste0("rec_comp_unit_", i), NULL, 
                                  choices = c("v/v %", "x fold", "w/v %", "mM", "µM", "mg/mL", "g/L"))),
            column(1, 
                   if (length(indices) > 1) {
                     actionButton(paste0("rec_btn_remove_", i), "❌", class = "btn-danger btn-xs", 
                                  onclick = sprintf("Shiny.setInputValue('rec_remove_row_id', %d, {priority: 'event'})", i))
                   } else NULL
            )
          )
      )
    })
  })
  
  # Row Management Listeners
  observeEvent(input$rec_btn_add_row, {
    current <- row_indices()
    next_id <- if(length(current) == 0) 1 else max(current) + 1
    row_indices(c(current, next_id))
  })
  
  observeEvent(input$rec_remove_row_id, {
    target_id <- input$rec_remove_row_id
    row_indices(setdiff(row_indices(), target_id))
  })
  
  # Save Recipe to JSON File
  observeEvent(input$rec_btn_save_recipe, {
    req(input$rec_input_name, input$rec_input_solvent)
    indices <- row_indices()
    
    components_list <- map(indices, function(i) {
      name <- input[[paste0("rec_comp_name_", i)]]
      stock <- input[[paste0("rec_comp_stock_", i)]]
      target <- input[[paste0("rec_comp_target_", i)]]
      unit <- input[[paste0("rec_comp_unit_", i)]]
      
      if (!is.null(name) && nchar(trimws(name)) > 0) {
        list(
          reagent_name = trimws(name),
          stock_conc   = as.numeric(stock),
          target_conc  = as.numeric(target),
          unit         = as.character(unit)
        )
      } else {
        NULL
      }
    }) %>% compact()
    
    new_recipe <- list(
      recipe_id    = paste0("rec_", format(Sys.time(), "%Y%m%d_%H%M%S")),
      recipe_name  = trimws(input$rec_input_name),
      category     = input$rec_input_cat,
      notes        = trimws(input$rec_input_notes),
      base_solvent = trimws(input$rec_input_solvent),
      components   = components_list
    )
    
    current_db <- recipes_db()
    updated_db <- c(current_db, list(new_recipe))
    
    write_json(updated_db, db_path, auto_unbox = TRUE, pretty = TRUE)
    recipes_db(updated_db)
    selected_recipe_id(new_recipe$recipe_id)
    
    removeModal()
    showNotification(paste("Recipe '", new_recipe$recipe_name, "' saved successfully!"), type = "message")
  })
  
  # --- DELETE RECIPE FEATURE ---
  observeEvent(input$rec_btn_delete_confirm, {
    req(selected_recipe_id())
    db <- recipes_db()
    rec_to_delete <- keep(db, ~ .x$recipe_id == selected_recipe_id())
    
    if (length(rec_to_delete) > 0) {
      rec_name <- rec_to_delete[[1]]$recipe_name
      
      showModal(modalDialog(
        title = div(strong("🗑️ Confirm Recipe Deletion"), style = "color: #721c24;"),
        p(sprintf("Are you sure you want to permanently delete '%s'?", rec_name)),
        p("This action will immediately update your recipes database file on disk."),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("rec_btn_delete_final", "Permanently Delete", class = "btn-danger", style = "font-weight: bold;")
        )
      ))
    }
  })
  
  observeEvent(input$rec_btn_delete_final, {
    req(selected_recipe_id())
    db <- recipes_db()
    
    updated_db <- keep(db, ~ .x$recipe_id != selected_recipe_id())
    
    write_json(updated_db, db_path, auto_unbox = TRUE, pretty = TRUE)
    recipes_db(updated_db)
    
    # Reset selection to next available recipe or NULL
    if (length(updated_db) > 0) {
      selected_recipe_id(updated_db[[1]]$recipe_id)
    } else {
      selected_recipe_id(NULL)
    }
    
    removeModal()
    showNotification("Recipe permanently deleted.", type = "warning")
  })
  
  # Scaled Recipe View Logic
  output$rec_scaled_recipe_details <- renderUI({
    req(selected_recipe_id())
    db <- recipes_db()
    rec <- keep(db, ~ .x$recipe_id == selected_recipe_id())
    
    if (length(rec) == 0) return(p("Select a recipe from the library card grid."))
    rec <- rec[[1]]
    
    target_vol <- if (!is.null(input$rec_target_prep_volume)) as.numeric(input$rec_target_prep_volume) else 100
    
    total_solute_vol <- 0
    walk(rec$components, function(cmp) {
      req_vol_mL <- (cmp$target_conc / cmp$stock_conc) * target_vol
      total_solute_vol <<- total_solute_vol + req_vol_mL
    })
    
    solvent_vol_mL <- max(0, target_vol - total_solute_vol)
    
    tagList(
      fluidRow(
        column(8, 
               h4(strong(rec$recipe_name), style = "color: #231F20; margin: 0 0 5px 0;"),
               span(rec$category, class = "badge", style = "background-color: #4A5343; color: #ffffff;")
        ),
        column(4, align = "right",
               actionButton("rec_btn_delete_confirm", "🗑️ Delete Recipe", class = "btn-danger btn-xs", style = "font-weight: bold;")
        )
      ),
      hr(style = "margin: 15px 0;"),
      
      numericInput("rec_target_prep_volume", "Target Preparation Volume (mL):", 
                   value = target_vol, min = 1, max = 5000, step = 5),
      
      if (nchar(rec$notes) > 0) div(style = "margin-top: 10px;", p(tags$small(em(rec$notes)), style = "color: #D9A05B;")),
      
      tableOutput("rec_tbl_components"),
      
      div(class = "alert alert-info", style = "margin-top: 15px; background-color: #E6E4DD; border-color: #4A5343; color: #231F20;",
          strong("📍 Bench Preparation Instructions:"), br(),
          sprintf("1. Add all listed reagent components to your preparation vessel."), br(),
          sprintf("2. Top up with base vehicle (%s) to reach the final batch volume of %.1f mL (approx. %.2f mL vehicle).", 
                  rec$base_solvent, target_vol, solvent_vol_mL)
      )
    )
  })
  
  output$rec_tbl_components <- renderTable({
    req(selected_recipe_id())
    db <- recipes_db()
    rec <- keep(db, ~ .x$recipe_id == selected_recipe_id())
    if (length(rec) == 0) return(NULL)
    rec <- rec[[1]]
    
    target_vol <- if (!is.null(input$rec_target_prep_volume)) as.numeric(input$rec_target_prep_volume) else 100
    
    map_df(rec$components, function(cmp) {
      req_vol_mL <- (cmp$target_conc / cmp$stock_conc) * target_vol
      display_amt <- if (req_vol_mL < 1.0) {
        sprintf("%.1f µL", req_vol_mL * 1000)
      } else {
        sprintf("%.2f mL", req_vol_mL)
      }
      
      data.frame(
        `Reagent Component` = cmp$reagent_name,
        `Stock Conc` = paste(cmp$stock_conc, cmp$unit),
        `Target Conc` = paste(cmp$target_conc, cmp$unit),
        `Amount Needed` = display_amt,
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    })
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
}