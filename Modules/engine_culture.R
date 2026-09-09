# =========================================================================
# APEX PLATFORM CELL CULTURE WORKSTATION & MICROSCOPY DECK (UPDATED)
# =========================================================================

# --- UI Layout Component ---
ui_culture_layout <- function() {
  tagList(
    tags$head(
      tags$style(HTML("
        .culture-sidebar { background: #E6E4DD; padding: 20px; border-radius: 6px; border: 1px solid #4A5343; }
        .gallery-card-btn { background: none; border: none; padding: 0; width: 100%; text-align: left; display: block; }
        .gallery-card { border: 1px solid #E6E4DD; border-radius: 6px; background: white; padding: 10px; margin-bottom: 15px; box-shadow: 0 2px 4px rgba(0,0,0,0.02); transition: transform 0.15s ease; }
        .gallery-card:hover { transform: scale(1.02); cursor: pointer; border-color: #4A5343; }
        .img-preview-frame { width: 100%; height: 160px; object-fit: cover; border-radius: 4px; border: 1px solid #E6E4DD; }
        .modal-img-frame { width: 100%; max-height: 500px; object-fit: contain; border-radius: 4px; border: 1px solid #2D323E; background: #17191E; transition: opacity 0.2s ease-in-out; }
        
        /* Navigation Swiper Deck Classes */
        .swiper-btn-deck { display: flex; justify-content: space-between; align-items: center; margin-bottom: 12px; }
        
        /* Deep Zoom Workstation Pan/Scan Viewport Controls */
        .zoom-scroll-box { width: 100%; max-height: 500px; overflow: auto; border: 1px solid #2D323E; border-radius: 4px; background: #17191E; }
        .img-zoom-focused { max-width: none !important; max-height: none !important; width: 180% !important; height: auto !important; cursor: zoom-out; }
        
        /* Adjust nested accordions to look distinct from outer panels */
        .nested-passage-accordion .accordion-button { background-color: #f8f9fa !important; color: #231F20 !important; font-size: 13px !important; font-weight: bold !important; padding: 8px 15px !important; }
        .nested-passage-accordion .accordion-button:not(.collapsed) { background-color: #E6E4DD !important; border-bottom: 1px solid #4A5343; }
        body { overflow-y: scroll !important; }
      "))
    ),
    
    fluidRow(
      # --- Left Sidebar: Culture Entry, Typology Control, & Ingestion ---
      column(4,
             div(class = "culture-sidebar",
                 tags$h4(tags$strong("🧫 Log Culture Node")),
                 hr(style = "border-top: 1px solid #4A5343; opacity: 0.3;"),
                 
                 selectInput("cult_cell_type", "Cell Line Typology:", choices = NULL),
                 
                 fluidRow(
                   column(6, actionButton("cult_add_type_modal_btn", "➕ Add Type", class = "btn-secondary btn-sm w-100")),
                   column(6, actionButton("cult_del_type_btn", "❌ Delete Type", class = "btn-secondary btn-sm w-100"))
                 ),
                 br(),
                 
                 # UPDATE 1: Set minimum passage parameter to 0 to allow Passage 0 baseline logging
                 numericInput("cult_passage", "Current Passage Number:", value = 1, min = 0, step = 1),
                 sliderInput("cult_confluency", "Estimated Confluency Bounds (%)", min = 0, max = 100, value = 50, step = 5),
                 textInput("cult_treatment", "Treatment / Stimulation Matrix:", value = "Control Baseline"),
                 
                 hr(style = "border-top: 1px solid #4A5343; opacity: 0.3;"),
                 tags$h5(tags$strong("🔬 Microscopic Image Ingestion")),
                 fileInput("cult_image_upload", "Choose Microscope Image File:",
                           accept = c('image/png', 'image/jpeg', 'image/jpg')),
                 
                 tags$p(tags$small(tags$em("Generated ID Key: ")), 
                        uiOutput("cult_generated_id_hint", inline = TRUE)),
                 br(),
                 actionButton("cult_save_btn", "Commit Entry to Database", class = "btn-primary w-100")
             )
      ),
      
      # --- Right Body Panels: Double-Nested Trees & Historical Ledger ---
      column(8,
             tabsetPanel(
               id = "culture_subtabs",
               type = "tabs",
               
               tabPanel("🌳 Collapsible Passage Trees",
                        br(),
                        uiOutput("culture_accordion_tree")
               ),
               
               tabPanel("📋 Relational Culture Ledger",
                        br(),
                        div(class = "compact-dt", DTOutput("culture_ledger_table"))
               )
             )
      )
    )
  )
}

# --- Server Logic Component ---
server_culture_logic <- function(input, output, session, vars) {
  
  img_dir <- "www/microscopy_store"
  if(!dir.exists("www")) dir.create("www")
  if(!dir.exists(img_dir)) dir.create(img_dir)
  
  shiny::addResourcePath(prefix = "micro_assets", directoryPath = normalizePath(img_dir, winslash = "/", mustWork = FALSE))
  
  db_path    <- "cell_culture_database.csv"
  types_path <- "culture_types_metadata.csv"
  
  # Tracks active internal state of your swiper index deck
  current_selected_uid <- reactiveVal(NULL)
  
  # --- 1. Manage Dynamic Typology Files ---
  cell_types_df <- reactiveVal({
    if (file.exists(types_path)) {
      read_csv(types_path, col_types = cols(Code = col_character(), Label = col_character()))
    } else {
      initial_types <- tibble(
        Code = c("SMC", "FIB", "EC"),
        Label = c("Smooth Muscle Cells (SMC)", "Primary Fibroblasts", "Endothelial Cells (EC)")
      )
      write_csv(initial_types, types_path)
      initial_types
    }
  })
  
  observe({
    req(cell_types_df())
    types <- cell_types_df()
    choices_vec <- setNames(types$Code, types$Label)
    updateSelectInput(session, "cult_cell_type", choices = choices_vec)
  })
  
  observeEvent(input$cult_add_type_modal_btn, {
    showModal(modalDialog(
      title = "🧬 Add New Cell Line Typology", size = "s", easyClose = TRUE,
      textInput("new_type_code", "Short Code (e.g., CAR):", placeholder = "MAX 4 Chars"),
      textInput("new_type_label", "Full Descriptive Label:", placeholder = "e.g., Cardiomyocytes"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("cult_save_type_btn", "Save Typology", class = "btn-primary")
      )
    ))
  })
  
  observeEvent(input$cult_save_type_btn, {
    req(input$new_type_code, input$new_type_label)
    code_clean <- toupper(str_trim(input$new_type_code))
    label_clean <- str_trim(input$new_type_label)
    current <- cell_types_df()
    if(code_clean %in% current$Code) {
      showNotification("Error: Typology short code already exists.", type = "error")
      return()
    }
    updated <- bind_rows(current, tibble(Code = code_clean, Label = label_clean))
    cell_types_df(updated)
    write_csv(updated, types_path)
    removeModal()
    showNotification("New cell typology appended successfully.", type = "message")
  })
  
  observeEvent(input$cult_del_type_btn, {
    req(input$cult_cell_type)
    current <- cell_types_df()
    if(nrow(current) <= 1) {
      showNotification("Aborted: Must retain at least one baseline cell archetype.", type = "warning")
      return()
    }
    updated <- current %>% filter(Code != input$cult_cell_type)
    cell_types_df(updated)
    write_csv(updated, types_path)
    showNotification("Typology record scrubbed from options.", type = "message")
  })
  
  # --- 2. Initialize Core Culture Records ---
  culture_db <- reactiveVal({
    if (file.exists(db_path)) {
      read_csv(db_path, col_types = cols(
        UID = col_character(), Cell_ID = col_character(), Date_Logged = col_character(), 
        Project = col_character(), Cell_Type = col_character(), Passage = col_double(), 
        Confluency = col_double(), Treatment = col_character(), Image_Path = col_character(),
        Observations = col_character()
      ))
    } else {
      tibble(
        UID = character(), Cell_ID = character(), Date_Logged = character(), 
        Project = character(), Cell_Type = character(), Passage = numeric(), 
        Confluency = numeric(), Treatment = character(), Image_Path = character(),
        Observations = character()
      )
    }
  })
  
  generated_id <- reactive({
    req(input$global_project, input$cult_cell_type, input$cult_passage)
    paste0(input$global_project, "_", input$cult_cell_type, "_P", input$cult_passage)
  })
  
  output$cult_generated_id_hint <- renderUI({
    tags$strong(style = "color: #4A5343;", generated_id())
  })
  
  # --- 3. Database Ingestion Routine ---
  observeEvent(input$cult_save_btn, {
    req(input$global_project, generated_id())
    final_img_path <- "No Image Appended"
    timestamp <- as.numeric(Sys.time())
    
    if(!is.null(input$cult_image_upload)) {
      ext <- tools::file_ext(input$cult_image_upload$name)
      unique_filename <- paste0(generated_id(), "_TS", timestamp, ".", ext)
      target_dest <- file.path(img_dir, unique_filename)
      file.copy(input$cult_image_upload$datapath, target_dest, overwrite = TRUE)
      final_img_path <- unique_filename
    }
    
    new_entry <- tibble(
      UID = paste0("NODE_", timestamp),
      Cell_ID = generated_id(),
      Date_Logged = format(Sys.Date(), "%Y-%m-%d"),
      Project = input$global_project,
      Cell_Type = input$cult_cell_type,
      Passage = as.numeric(input$cult_passage),
      Confluency = as.numeric(input$cult_confluency),
      Treatment = input$cult_treatment,
      Image_Path = final_img_path,
      Observations = "No entry notes compiled yet."
    )
    
    updated_df <- bind_rows(culture_db(), new_entry)
    culture_db(updated_df)
    write_csv(updated_df, db_path)
    
    showNotification(paste("Success: Node logged as", generated_id()), type = "message")
    shinyjs::reset("cult_image_upload")
  })
  
  # --- 4. Render Ledger Table with Database Row Deletion ---
  output$culture_ledger_table <- renderDT({
    req(culture_db())
    filtered_df <- culture_db() %>% 
      filter(Project == input$global_project) %>%
      select(Date_Logged, Cell_ID, Cell_Type, Passage, Confluency, Treatment, UID)
    
    if(nrow(filtered_df) > 0) {
      filtered_df$Actions <- paste0(
        '<button class="btn btn-danger btn-sm delete-btn" id="del_', filtered_df$UID, 
        '" onclick="Shiny.setInputValue(\'culture_row_to_delete\', this.id, {priority: \'event\'})">Scrub</button>'
      )
    } else {
      filtered_df$Actions <- character()
    }
    
    datatable(filtered_df %>% select(-UID), escape = FALSE, options = list(pageLength = 10, dom = 'rtip'), rownames = FALSE)
  })
  
  observeEvent(input$culture_row_to_delete, {
    target_uid <- str_replace(input$culture_row_to_delete, "del_", "")
    db <- culture_db()
    row_match <- db %>% filter(UID == target_uid)
    if(nrow(row_match) > 0 && row_match$Image_Path[1] != "No Image Appended") {
      file_to_remove <- file.path(img_dir, row_match$Image_Path[1]) 
      if(file.exists(file_to_remove)) file.remove(file_to_remove)
    }
    updated_db <- db %>% filter(UID != target_uid)
    culture_db(updated_db)
    write_csv(updated_db, db_path)
    showNotification("Record and asset permanently deleted.", type = "warning")
  })
  
  # --- 5. Double-Nested Collapsible Passage Tree Accordion Structure ---
  output$culture_accordion_tree <- renderUI({
    req(culture_db())
    proj_df <- culture_db() %>% filter(Project == input$global_project)
    
    if(nrow(proj_df) == 0) {
      return(tags$p(style = "color: gray; font-style: italic; padding: 20px;", 
                    "No microscopy or passage nodes compiled for this active study frame."))
    }
    
    distinct_types <- unique(proj_df$Cell_Type)
    
    outer_panels <- lapply(distinct_types, function(type) {
      type_subset <- proj_df %>% filter(Cell_Type == type)
      passages_present <- sort(unique(type_subset$Passage))
      
      inner_panels <- lapply(passages_present, function(p) {
        p_subset <- type_subset %>% filter(Passage == p) %>% arrange(Date_Logged)
        
        cards_layout <- lapply(1:nrow(p_subset), function(i) {
          row <- p_subset[i, ]
          img_src <- ifelse(row$Image_Path == "No Image Appended", "placeholder.png", paste0("micro_assets/", row$Image_Path))
          
          column(4,
                 tags$button(
                   class = "gallery-card-btn",
                   onclick = sprintf("Shiny.setInputValue('selected_culture_uid', '%s', {priority: 'event'})", row$UID),
                   div(class = "gallery-card",
                       tags$img(src = img_src, class = "img-preview-frame"),
                       br(), br(),
                       tags$h6(tags$strong(row$Cell_ID), style = "margin:0; color:#231F20;"),
                       tags$p(style = "font-size: 11px; color: #4A5343; margin: 4px 0 0 0;",
                              paste0("Confluency: ", row$Confluency, "% | ", row$Treatment))
                   )
                 )
          )
        })
        
        accordion_panel(
          title = paste("📦 Passage Round", p, "(", nrow(p_subset), "Nodes )"),
          fluidRow(cards_layout)
        )
      })
      
      nested_accordion_ui <- div(
        class = "nested-passage-accordion",
        do.call(accordion, c(id = paste0("nested_p_acc_", type), inner_panels))
      )
      
      accordion_panel(
        title = paste("🧬", type, "Culture Lineage Tree (Total Nodes:", nrow(type_subset), ")"),
        nested_accordion_ui
      )
    })
    
    do.call(accordion, c(id = "culture_main_tree_accordion", outer_panels))
  })
  
  # --- 6. Robust Microscope Workstation Modal Intercept ---
  # UPDATE 2: Refactored to fire a STATIC modal shell, letting reactivity handle content changes smoothly.
  observeEvent(input$selected_culture_uid, {
    req(input$selected_culture_uid)
    current_selected_uid(input$selected_culture_uid) 
    
    # Fire the modal skeleton once when a card is clicked.
    showModal(modalDialog(
      title = uiOutput("modal_header_title", inline = TRUE),
      size = "l", easyClose = TRUE,
      
      # Swiper Action Control Strip
      div(class = "swiper-btn-deck",
          actionButton("swiper_prev_btn", "◀️ Previous Entry", class = "btn-outline-dark btn-sm"),
          uiOutput("modal_tracking_label"),
          actionButton("swiper_next_btn", "Next Entry ▶️", class = "btn-outline-dark btn-sm")
      ),
      hr(style = "margin-top: 4px; margin-bottom: 12px;"),
      
      fluidRow(
        # Left Column: Clickable Presentation Frame
        column(7,
               tags$p(tags$small(tags$em("💡 Click directly on the image panel below to blow it up into a full-screen inspector look."))),
               tags$button(
                 style = "background: none; border: none; padding: 0; width: 100%;",
                 onclick = "Shiny.setInputValue('trigger_deep_zoom_click', true, {priority: 'event'})",
                 uiOutput("modal_image_viewport")
               )
        ),
        # Right Column: Specs Dossier & Editable Forms
        column(5,
               div(class = "stat-box", style = "border-left: 4px solid #4A5343; background: #F4F3EF; padding: 12px; border-radius: 4px;",
                   tags$h5(tags$strong("Editable Node Specifications"), style="color:#231F20; margin-top:0;"),
                   hr(style="opacity: 0.2; margin: 8px 0;"),
                   
                   # Inputs are rendered dynamically so they instantly adjust values during a swipe without modal flashes
                   uiOutput("modal_editable_specs_fields")
               ),
               br(),
               div(class = "stat-box", style = "border-left: 4px solid #D9A05B; background: #FFFDF9; padding: 12px; border-radius: 4px;",
                   tags$h5(tags$strong("Interactive Analysis Notebook")),
                   uiOutput("modal_observations_notebook_field"),
                   br(),
                   actionButton("modal_save_all_changes_btn", "💾 Sync & Save All Changes", class = "btn-success btn-sm w-100")
               )
        )
      ),
      footer = modalButton("Dismiss Workstation View")
    ))
  })
  
  # --- Reactive Sub-components for Smooth, Flicker-Free Swiping ---
  # Retrieves active record based on dynamic swiper state
  active_node_data <- reactive({
    req(current_selected_uid(), culture_db())
    node <- culture_db() %>% filter(UID == current_selected_uid())
    req(nrow(node) > 0)
    node
  })
  
  output$modal_header_title <- renderUI({
    node <- active_node_data()
    paste("🔬 Microscope Interrogation Workstation —", node$Cell_ID[1])
  })
  
  output$modal_tracking_label <- renderUI({
    node <- active_node_data()
    tags$span(tags$strong(paste("Active Node Layer:", node$Cell_ID[1])), style = "color: #4A5343; font-size: 13px;")
  })
  
  output$modal_image_viewport <- renderUI({
    node <- active_node_data()
    img_src <- ifelse(node$Image_Path[1] == "No Image Appended", "placeholder.png", paste0("micro_assets/", node$Image_Path[1]))
    tags$img(src = img_src, class = "modal-img-frame", style = "cursor: zoom-in;")
  })
  
  output$modal_editable_specs_fields <- renderUI({
    node <- active_node_data()
    tagList(
      # UPDATE 1: Set min = 0 here too to allow Passage 0 editing in the modal
      numericInput("modal_edit_passage", "Passage Round:", value = node$Passage[1], min = 0, step = 1),
      sliderInput("modal_edit_confluency", "Observed Confluency (%):", min = 0, max = 100, value = node$Confluency[1], step = 5),
      textInput("modal_edit_treatment", "Experimental Treatment Matrix:", value = node$Treatment[1])
    )
  })
  
  output$modal_observations_notebook_field <- renderUI({
    node <- active_node_data()
    textAreaInput("modal_notes_field", NULL, value = node$Observations[1], rows = 4, width = "100%")
  })
  
  # --- Swiper Navigation Control Logic (Updates UID State Only) ---
  observeEvent(input$swiper_next_btn, {
    req(current_selected_uid(), culture_db())
    db <- culture_db() %>% filter(Project == input$global_project)
    req(nrow(db) > 1)
    
    current_idx <- which(db$UID == current_selected_uid())
    if(length(current_idx) == 0) return()
    
    next_idx <- if(current_idx < nrow(db)) current_idx + 1 else 1
    current_selected_uid(db$UID[next_idx]) # Swaps State. Shiny reactively updates the UI components instantly and smoothly.
  })
  
  observeEvent(input$swiper_prev_btn, {
    req(current_selected_uid(), culture_db())
    db <- culture_db() %>% filter(Project == input$global_project)
    req(nrow(db) > 1)
    
    current_idx <- which(db$UID == current_selected_uid())
    if(length(current_idx) == 0) return()
    
    prev_idx <- if(current_idx > 1) current_idx - 1 else nrow(db)
    current_selected_uid(db$UID[prev_idx]) # Swaps State.
  })
  
  # --- Unified Live In-Line Database Editor Sync ---
  observeEvent(input$modal_save_all_changes_btn, {
    req(current_selected_uid(), culture_db())
    
    master_db <- culture_db()
    target_uid <- current_selected_uid()
    row_idx <- which(master_db$UID == target_uid)
    
    req(length(row_idx) > 0)
    
    # Read modified inline UI fields
    new_passage <- as.numeric(input$modal_edit_passage)
    new_type <- master_db$Cell_Type[row_idx]
    
    # Generate updated tracking ID
    updated_cell_id <- paste0(input$global_project, "_", new_type, "_P", new_passage)
    
    # Atomic transaction across target row
    master_db$Passage[row_idx]      <- new_passage
    master_db$Cell_ID[row_idx]      <- updated_cell_id
    master_db$Confluency[row_idx]   <- as.numeric(input$modal_edit_confluency)
    master_db$Treatment[row_idx]     <- input$modal_edit_treatment
    master_db$Observations[row_idx] <- input$modal_notes_field
    
    # Update local memory and rewrite to files
    culture_db(master_db)
    write_csv(master_db, db_path)
    
    showNotification("Database record and structural tracking keys synchronized cleanly.", type = "message")
    
    # Forces a quick refresh to update tracking ID structures on-screen
    current_selected_uid(target_uid)
  })
  
  # --- 7. INDEPENDENT LAYER 2 ZOOM LISTENERS ---
  observeEvent(input$trigger_deep_zoom_click, {
    req(current_selected_uid())
    db <- culture_db()
    node <- db %>% filter(UID == current_selected_uid())
    req(nrow(node) > 0)
    
    img_src <- ifelse(node$Image_Path[1] == "No Image Appended", "placeholder.png", paste0("micro_assets/", node$Image_Path[1]))
    
    showModal(modalDialog(
      title = paste("🔍 High-Resolution Morphological Asset Display —", node$Cell_ID[1]),
      size = "xl", easyClose = TRUE,
      
      div(style = "background: #111418; padding: 20px; border-radius: 6px; text-align: center; overflow: auto; max-height: 75vh;",
          tags$img(src = img_src, style = "max-width: 100%; height: auto; transform: scale(1.3); transform-origin: top center; margin-bottom: 20px;")
      ),
      
      footer = tagList(
        actionButton("return_to_workstation_btn", "↩️ Return to Workstation Notes", class = "btn-secondary")
      )
    ))
  })
  
  observeEvent(input$return_to_workstation_btn, {
    req(current_selected_uid())
    # Triggers a simple state refresh to re-render rather than reconstructing modal
    current_selected_uid(current_selected_uid())
  })
}