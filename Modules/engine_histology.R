# =========================================================================
# APEX PLATFORM UNIFIED IMAGING & QUANTIFICATION ENGINE (UPLOAD TYPE FIX)
# =========================================================================

# --- UI Layout Component ---
ui_histology_layout <- function() {
  tagList(
    tags$head(
      tags$style(HTML("
        .vault-card { border: 1px solid #E6E4DD; border-radius: 6px; background: white; padding: 12px; margin-bottom: 15px; box-shadow: 0 2px 4px rgba(0,0,0,0.03); }
        .gallery-card-btn { background: none; border: none; padding: 0; width: 100%; text-align: left; display: block; }
        .histo-card { border: 1px solid #E6E4DD; border-radius: 6px; background: white; padding: 10px; margin-bottom: 15px; box-shadow: 0 2px 4px rgba(0,0,0,0.02); transition: transform 0.15s ease; position: relative; }
        .histo-card:hover { transform: scale(1.02); cursor: pointer; border-color: #4A5343; }
        .vault-img-preview { width: 100%; height: 200px; object-fit: contain; background: #0F1115; border-radius: 4px; border: 1px solid #2D323E; }
        .protocol-box { background: #F4F3EF; border-left: 4px solid #4A5343; padding: 12px; border-radius: 4px; margin-bottom: 15px; }
        .modal-histo-img-frame { width: 100%; max-height: 70vh; object-fit: contain; border-radius: 4px; border: 1px solid #2D323E; background: #0A0C0E; }
      "))
    ),
    
    tabsetPanel(
      id = "imaging_master_tabs",
      type = "tabs",
      
      # ===================================================================
      # SUB-TAB 1: FULL-WIDTH IMAGE & PROTOCOL VAULT
      # ===================================================================
      tabPanel("🖼️ Image & Protocol Vault",
               br(),
               # Top Management Control Strip
               div(class = "vault-card",
                   fluidRow(
                     column(4,
                            selectInput("vault_active_deck", "Select Target Image Deck / Assay:", choices = NULL)
                     ),
                     column(3,
                            actionButton("vault_create_deck_btn", "➕ Create New Deck", class = "btn-secondary w-100", style = "margin-top: 25px;")
                     ),
                     column(5,
                            fileInput("vault_image_upload", "Upload Micrographs / Gel Scans to Deck:", 
                                      multiple = TRUE, accept = c('image/png', 'image/jpeg', 'image/jpg', 'image/tiff'), width = "100%")
                     )
                   )
               ),
               
               # Collapsible Decks Accordion UI
               uiOutput("vault_accordion_decks_ui")
      ),
      
      # ===================================================================
      # SUB-TAB 2: QUPATH & QUANTIFICATION ENGINE
      # ===================================================================
      tabPanel("📊 Quantification & Statistics",
               br(),
               fluidRow(
                 column(4,
                        div(class = "culture-sidebar", style = "background: #E6E4DD; padding: 20px; border-radius: 6px; border: 1px solid #4A5343;",
                            tags$h4(tags$strong("📈 Import Quantification")),
                            hr(style = "border-top: 1px solid #4A5343; opacity: 0.3;"),
                            
                            fileInput("quant_file_upload", "Choose QuPath / ImageJ Export (CSV/TSV):",
                                      accept = c('.csv', '.tsv', '.txt')),
                            
                            textInput("quant_metric_name", "Measurement Metric Label:", value = "Area % Staining / Optical Density"),
                            selectInput("quant_chart_type", "Plot Archetype:", choices = c("Boxplot + Jitter Points", "Bar Plot (Mean ± SEM)")),
                            
                            hr(style = "border-top: 1px solid #4A5343; opacity: 0.3;"),
                            tags$h5(tags$strong("🔗 Cross-Modal Correlation")),
                            checkboxInput("quant_enable_corr", "Correlate with BP / PWV Data", value = FALSE)
                        )
                 ),
                 column(8,
                        div(class = "vault-card",
                            tags$h4(tags$strong("Publication Statistical Graphics")),
                            hr(),
                            plotOutput("quant_main_plot", height = "420px"),
                            br(),
                            div(class = "compact-dt", DTOutput("quant_summary_stats_table"))
                        )
                 )
               )
      )
    )
  )
}

# --- Server Logic Component ---
server_histology_logic <- function(input, output, session, vars) {
  
  img_dir <- "www/histology_store"
  if(!dir.exists("www")) dir.create("www")
  if(!dir.exists(img_dir)) dir.create(img_dir)
  
  shiny::addResourcePath(prefix = "histo_assets", directoryPath = normalizePath(img_dir, winslash = "/", mustWork = FALSE))
  
  decks_path <- "imaging_decks_metadata.csv"
  db_path    <- "histology_master_database.csv"
  
  current_selected_asset_uid <- reactiveVal(NULL)
  
  # --- 1. Manage Global-Synced Dynamic Image Decks ---
  decks_df <- reactiveVal({
    if (file.exists(decks_path)) {
      df <- read_csv(decks_path, col_types = cols(.default = col_character()), show_col_types = FALSE)
      if (!("Project" %in% names(df))) {
        df$Project <- "P01"
        write_csv(df, decks_path)
      }
      df
    } else {
      initial_decks <- tibble(
        Deck_ID = c("DECK_1", "DECK_2"),
        Project = c("P01", "P01"),
        Deck_Name = c("2026_Trichrome_Aortic_Arch", "Western_Blot_eNOS_Aorta"),
        Protocol_Notes = c("Masson's Trichrome Staining Kit; 10 min incubation in Bouin's Fluid at 56°C.", "12% SDS-PAGE, 30µg protein/lane. Primary: α-eNOS 1:1000 in 5% BSA 4°C overnight.")
      )
      write_csv(initial_decks, decks_path)
      initial_decks
    }
  })
  
  observe({
    req(decks_df(), input$global_project)
    proj_decks <- decks_df() %>% filter(Project == input$global_project)
    
    if(nrow(proj_decks) > 0) {
      choices_vec <- setNames(proj_decks$Deck_ID, proj_decks$Deck_Name)
      updateSelectInput(session, "vault_active_deck", choices = choices_vec)
    } else {
      updateSelectInput(session, "vault_active_deck", choices = c("-- No Decks Created for Project --" = ""))
    }
  })
  
  observeEvent(input$vault_create_deck_btn, {
    req(input$global_project)
    showModal(modalDialog(
      title = paste("📁 Create New Deck for Project:", input$global_project), size = "s", easyClose = TRUE,
      textInput("new_deck_name", "Deck / Assay Name:", placeholder = "e.g., IHC_CD31_Kidney_2026"),
      textAreaInput("new_deck_notes", "Protocol Details & Staining Specs:", placeholder = "Antibodies, dilutions, gel %, exposure times...", rows = 4),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("save_new_deck_btn", "Save Deck", class = "btn-primary")
      )
    ))
  })
  
  observeEvent(input$save_new_deck_btn, {
    req(input$new_deck_name, input$global_project)
    current <- decks_df()
    new_id <- paste0("DECK_", round(as.numeric(Sys.time())))
    
    updated <- bind_rows(current, tibble(
      Deck_ID = as.character(new_id), 
      Project = as.character(input$global_project), 
      Deck_Name = as.character(input$new_deck_name), 
      Protocol_Notes = as.character(input$new_deck_notes)
    ))
    
    decks_df(updated)
    write_csv(updated, decks_path)
    removeModal()
    showNotification(paste("New Deck created under project", input$global_project), type = "message")
  })
  
  # --- 2. Database & File Asset Handling (Type-Enforced) ---
  histology_db <- reactiveVal({
    if (file.exists(db_path)) {
      df <- read_csv(db_path, col_types = cols(.default = col_character()), show_col_types = FALSE)
      if (!("Project" %in% names(df))) {
        df$Project <- "P01"
        write_csv(df, db_path)
      }
      df
    } else {
      tibble(
        UID = character(), Deck_ID = character(), Date_Logged = character(), 
        Project = character(), Image_Name = character(), Image_Path = character(), 
        Sample_Label = character()
      )
    }
  })
  
  observeEvent(input$vault_image_upload, {
    req(input$vault_active_deck, input$global_project, input$vault_image_upload)
    
    if(input$vault_active_deck == "") {
      showNotification("Error: Please create or select an active Deck before uploading.", type = "error")
      return()
    }
    
    uploads <- input$vault_image_upload
    timestamp <- as.numeric(Sys.time())
    
    new_rows <- tibble(
      UID = paste0("ASSET_", round(timestamp), "_", seq_len(nrow(uploads))),
      Deck_ID = as.character(input$vault_active_deck),
      Date_Logged = as.character(format(Sys.Date(), "%Y-%m-%d")),
      Project = as.character(input$global_project),
      Image_Name = as.character(uploads$name),
      Image_Path = character(nrow(uploads)),
      Sample_Label = as.character(str_remove(uploads$name, "\\.[^.]+$"))
    )
    
    for(i in seq_len(nrow(uploads))) {
      orig_name <- uploads$name[i]
      ext <- tools::file_ext(orig_name)
      unique_filename <- paste0("IMG_", round(timestamp), "_", i, ".", ext)
      target_dest <- file.path(img_dir, unique_filename)
      file.copy(uploads$datapath[i], target_dest, overwrite = TRUE)
      new_rows$Image_Path[i] <- unique_filename
    }
    
    current_db <- histology_db()
    
    # Ensure current_db has strictly character columns before bind_rows
    current_db <- current_db %>% mutate(across(everything(), as.character))
    
    updated_db <- bind_rows(current_db, new_rows)
    histology_db(updated_db)
    write_csv(updated_db, db_path)
    showNotification(paste(nrow(uploads), "Image asset(s) appended cleanly to active deck!"), type = "message")
  })
  
  # --- 3. Collapsible Accordion Deck Layout ---
  output$vault_accordion_decks_ui <- renderUI({
    req(input$global_project, decks_df(), histology_db())
    
    proj_decks <- decks_df() %>% filter(Project == input$global_project)
    
    if(nrow(proj_decks) == 0) {
      return(tags$p(style = "color: gray; font-style: italic; padding: 30px; text-align: center;", 
                    paste("No imaging decks created for project", input$global_project, "yet. Use the control bar above to create your first deck.")))
    }
    
    db <- histology_db()
    
    deck_panels <- lapply(1:nrow(proj_decks), function(i) {
      deck <- proj_decks[i, ]
      deck_assets <- db %>% filter(Deck_ID == deck$Deck_ID)
      
      protocol_ui <- div(class = "protocol-box",
                         fluidRow(
                           column(9,
                                  tags$h6(tags$strong("📝 Protocol & Assay Notes")),
                                  textAreaInput(paste0("protocol_notes_edit_", deck$Deck_ID), NULL, value = deck$Protocol_Notes, rows = 2, width = "100%")
                           ),
                           column(3,
                                  br(),
                                  actionButton(paste0("save_protocol_btn_", deck$Deck_ID), "💾 Save Protocol Notes", class = "btn-success btn-sm w-100",
                                               onclick = sprintf("Shiny.setInputValue('vault_save_protocol_deck_id', '%s', {priority: 'event'})", deck$Deck_ID))
                           )
                         )
      )
      
      if(nrow(deck_assets) > 0) {
        cards <- lapply(1:nrow(deck_assets), function(j) {
          asset <- deck_assets[j, ]
          img_src <- paste0("histo_assets/", asset$Image_Path)
          
          column(4,
                 div(class = "histo-card",
                     tags$button(
                       class = "gallery-card-btn",
                       onclick = sprintf("Shiny.setInputValue('selected_vault_asset_uid', '%s', {priority: 'event'})", asset$UID),
                       tags$img(src = img_src, class = "vault-img-preview"),
                       br(), br(),
                       tags$strong(style = "font-size: 13px; color: #231F20;", asset$Sample_Label)
                     ),
                     hr(style = "margin: 6px 0;"),
                     actionButton(paste0("del_asset_", asset$UID), "Scrub Asset", class = "btn-danger btn-xs w-100",
                                  onclick = sprintf("Shiny.setInputValue('vault_asset_to_delete', '%s', {priority: 'event'})", asset$UID))
                 )
          )
        })
        gallery_ui <- fluidRow(cards)
      } else {
        gallery_ui <- tags$p(style = "color: gray; font-style: italic; padding: 15px;", "No images uploaded to this deck yet.")
      }
      
      accordion_panel(
        title = paste("📁 Deck:", deck$Deck_Name, "(", nrow(deck_assets), "Assets )"),
        protocol_ui,
        gallery_ui
      )
    })
    
    do.call(accordion, c(id = "vault_master_accordion", deck_panels))
  })
  
  observeEvent(input$vault_save_protocol_deck_id, {
    deck_id <- input$vault_save_protocol_deck_id
    note_val <- input[[paste0("protocol_notes_edit_", deck_id)]]
    
    df <- decks_df()
    idx <- which(df$Deck_ID == deck_id)
    if(length(idx) > 0) {
      df$Protocol_Notes[idx] <- note_val
      decks_df(df)
      write_csv(df, decks_path)
      showNotification("Protocol specifications saved cleanly.", type = "message")
    }
  })
  
  observeEvent(input$vault_asset_to_delete, {
    target_uid <- input$vault_asset_to_delete
    db <- histology_db()
    row_match <- db %>% filter(UID == target_uid)
    if(nrow(row_match) > 0) {
      file_to_remove <- file.path(img_dir, row_match$Image_Path[1])
      if(file.exists(file_to_remove)) file.remove(file_to_remove)
    }
    updated_db <- db %>% filter(UID != target_uid)
    histology_db(updated_db)
    write_csv(updated_db, db_path)
    showNotification("Image asset scrubbed cleanly.", type = "warning")
  })
  
  # --- 4. Deep Zoom Workstation Modal Intercept ---
  observeEvent(input$selected_vault_asset_uid, {
    req(input$selected_vault_asset_uid)
    current_selected_asset_uid(input$selected_vault_asset_uid)
    
    db <- histology_db()
    asset <- db %>% filter(UID == input$selected_vault_asset_uid)
    req(nrow(asset) > 0)
    
    img_src <- paste0("histo_assets/", asset$Image_Path[1])
    
    showModal(modalDialog(
      title = paste("🔍 High-Resolution Inspection —", asset$Sample_Label[1]),
      size = "xl", easyClose = TRUE,
      
      div(style = "background: #0A0C0E; padding: 15px; border-radius: 6px; text-align: center; overflow: auto;",
          tags$img(src = img_src, class = "modal-histo-img-frame")
      ),
      
      footer = modalButton("Close Inspection View")
    ))
  })
}