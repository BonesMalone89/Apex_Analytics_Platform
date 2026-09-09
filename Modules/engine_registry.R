# =========================================================================
# APEX PLATFORM CENTRALIZED ANIMAL REGISTRY & COLONY MANAGER
# =========================================================================

library(shiny)
library(dplyr)
library(stringr)
library(readr)
library(tibble)
library(DT)

# --- UI Layout Component ---
ui_registry_layout <- function() {
  sidebarLayout(
    sidebarPanel(
      tags$h4("➕ Register New Research Subject"),
      hr(),
      
      textInput("reg_animal_id", "Unique Subject ID:", placeholder = "e.g., WKY-01, SHR-01"),
      
      fluidRow(
        column(6, selectizeInput("reg_species", "Species:", choices = c("Rat", "Mouse", "Other"))),
        column(6, selectizeInput("reg_sex", "Biological Sex:", choices = c("Male", "Female")))
      ),
      
      fluidRow(
        column(6, textInput("reg_strain", "Strain / Breed:", placeholder = "e.g., SHR, WKY, C57BL/6")),
        column(6, textInput("reg_genotype", "Genotype:", value = "Wild-Type"))
      ),
      
      hr(),
      tags$h5(tags$strong("📆 Group Assignment and Housing Information")),
      
      fluidRow(
        column(6, dateInput("reg_dob", "Date of Birth (DOB):", value = Sys.Date() - 56)),
        column(6, dateInput("reg_arrival", "Arrival Date:", value = Sys.Date()))
      ),
      
      fluidRow(
        column(6, textInput("reg_cage", "Cage ID:", placeholder = "e.g., Cage 302")),
        column(6, textInput("reg_vendor", "Vendor:", placeholder = "e.g., Charles River"))
      ),
      
      selectizeInput("reg_group", "Assigned Experimental Cohort:", choices = NULL),
      actionLink("open_cohort_manager_btn", "⚙️ Add / Edit Custom Cohort Groups", style = "font-size: 12px; font-weight: bold; color: #4e73df;"),
      br(), br(),
      
      textInput("reg_iacuc", "Protocol:", placeholder = "e.g., 24-XYZ"),
      
      br(),
      actionButton("save_animal_btn", "🧬 Save Subject Profile to Registry", class = "btn-primary btn-block")
    ),
    
    mainPanel(
      tabsetPanel(
        type = "pills",
        tabPanel("📋 Active Animal Database",
                 br(),
                 tags$h4("Animal Subject Information Database"),
                 DTOutput("registry_master_dt_table"),
                 br(),
                 wellPanel(
                   style = "background-color: #f8f9fa; border: 1px solid #e3e6f0; padding: 12px;",
                   fluidRow(
                     column(6,
                            tags$h5(tags$strong("🔄 Reassign Selected Animal's Cohort")),
                            tags$small(tags$em("Select a row above, choose the randomized group target, and apply. All historical BP, PWV, Weight, and MAP data will automatically migrate.")),
                            br(), br(),
                            fluidRow(
                              column(7, selectizeInput("reassign_target_group", "New Cohort Group:", choices = NULL)),
                              column(5, actionButton("execute_reassign_btn", "Apply Reassignment", class = "btn-warning btn-block", style = "margin-top: 25px;"))
                            )
                     ),
                     column(6,
                            tags$h5(tags$strong("🧬 Subject Lifecycle & Data Actions")),
                            tags$br(),
                            actionButton("mark_terminal_btn", "Transition Subject to Terminal Archive", class = "btn-sm btn-default btn-block"),
                            br(),
                            actionButton("delete_animal_btn", "❌ Excise Record Permanently", class = "btn-sm btn-danger btn-block")
                     )
                   )
                 )
        ),
        tabPanel("🗃️ Euthanized Animal Database",
                 br(),
                 tags$h4("Terminal Archive and Tissue Collection Database"),
                 DTOutput("registry_archive_dt_table"),
                 br(),
                 actionButton("reactivate_animal_btn", "🔄 Revive / Move Selected Animal Back to Active Colony Ledger", class = "btn-sm btn-success")
        )
      )
    )
  )
}

# --- Server Logic Component ---
server_registry_logic <- function(input, output, session, vars) {
  
  current_inspected_id <- reactiveVal(NULL)
  
  config_file <- "biobank_config.txt"
  default_tissues <- c("Heart Tissue", "Kidney Tissue", "Serum", "Skeletal Muscle Tissue", "Aorta Ab", "Aorta Arch")
  
  if (!file.exists(config_file)) {
    writeLines(default_tissues, config_file)
  }
  
  master_tissue_list <- reactiveVal(readLines(config_file))
  
  project_registry <- reactive({
    df <- vars$animal_master_registry()
    if (nrow(df) == 0) return(df)
    df %>% filter(Project == input$global_project)
  })
  
  # --- Synchronize Cohort Choices Dynamically from Metadata Registry ---
  observe({
    meta <- vars$metadata_registry() %>% filter(Project_Key == input$global_project)
    
    # Always provide Unassigned/Pre-Randomization as the top neutral option
    group_choices <- if (nrow(meta) > 0) {
      c("Unassigned / Pre-Randomization", unique(meta$Group_Key))
    } else {
      c("Unassigned / Pre-Randomization", "Control", "Treatment")
    }
    
    updateSelectizeInput(session, "reg_group", choices = unique(group_choices), selected = "Unassigned / Pre-Randomization")
    updateSelectizeInput(session, "reassign_target_group", choices = unique(group_choices))
  })
  
  # --- Dynamic Cohort Customizer Modal ---
  observeEvent(input$open_cohort_manager_btn, {
    showModal(modalDialog(
      title = "🎨 Manage Study Cohorts & Display Colors", size = "m", easyClose = TRUE,
      tags$p(tags$small(tags$em("Add custom experimental cohorts for this study. These groups will propagate across BP, PWV, Weight, and Analytics modules."))),
      hr(),
      
      fluidRow(
        column(6, textInput("new_cohort_name", "New Cohort Name:", placeholder = "e.g., High-Salt Vehicle, Tamoxifen+")),
        column(6, colourpicker::colourInput("new_cohort_color", "Cohort Track Color:", value = "#e74a3b"))
      ),
      actionButton("btn_add_cohort_meta", "➕ Add Cohort to Study", class = "btn-primary btn-block"),
      hr(),
      
      tags$h5(tags$strong("Current Active Cohorts in Metadata:")),
      DTOutput("modal_current_cohorts_dt"),
      br(),
      actionButton("btn_remove_cohort_meta", "🗑️ Remove Selected Cohort", class = "btn-danger btn-sm"),
      
      footer = modalButton("Close Manager")
    ))
  })
  
  output$modal_current_cohorts_dt <- renderDT({
    meta <- vars$metadata_registry() %>% filter(Project_Key == input$global_project)
    if (nrow(meta) == 0) return(datatable(tibble(`Notice` = "No specific custom cohorts defined."), options = list(dom = 't'), rownames = FALSE))
    
    display_df <- meta %>% select(`Cohort Name` = Group_Key, `Assigned Color` = Group_Color)
    datatable(display_df, selection = 'single', options = list(dom = 't', pageLength = 10), rownames = FALSE) %>%
      formatStyle('Assigned Color', backgroundColor = styleEqual(display_df$`Assigned Color`, display_df$`Assigned Color`), color = '#ffffff')
  })
  
  # --- Add Cohort to Metadata Registry ---
  observeEvent(input$btn_add_cohort_meta, {
    req(input$new_cohort_name)
    grp_name <- trimws(input$new_cohort_name)
    grp_col  <- input$new_cohort_color
    
    meta_all <- vars$metadata_registry()
    study_meta <- meta_all %>% filter(Project_Key == input$global_project)
    
    if (grp_name %in% study_meta$Group_Key) {
      showNotification("Cohort with this name already exists in active study.", type = "warning")
      return()
    }
    
    # Inherit max_weeks, tx_start, pool_baseline from project template
    max_w <- if (nrow(study_meta) > 0) study_meta$Max_Weeks[1] else 12
    tx_w  <- if (nrow(study_meta) > 0 && "Tx_Start_Week" %in% colnames(study_meta)) study_meta$Tx_Start_Week[1] else 1
    pool  <- if (nrow(study_meta) > 0 && "Pool_Baseline" %in% colnames(study_meta)) study_meta$Pool_Baseline[1] else TRUE
    
    new_meta_row <- tibble(
      Project_Key   = input$global_project,
      Group_Key     = grp_name,
      Group_Color   = grp_col,
      Max_Weeks     = max_w,
      Tx_Start_Week = tx_w,
      Pool_Baseline = pool
    )
    
    updated_meta <- bind_rows(meta_all, new_meta_row)
    vars$metadata_registry(updated_meta)
    write_csv(updated_meta, "global_metadata_registry.csv")
    
    updateTextInput(session, "new_cohort_name", value = "")
    showNotification(paste0("Cohort '", grp_name, "' registered successfully."), type = "message")
  })
  
  # --- Remove Cohort from Metadata Registry ---
  observeEvent(input$btn_remove_cohort_meta, {
    req(input$modal_current_cohorts_dt_rows_selected)
    study_meta <- vars$metadata_registry() %>% filter(Project_Key == input$global_project)
    target_grp <- study_meta$Group_Key[input$modal_current_cohorts_dt_rows_selected]
    
    updated_meta <- vars$metadata_registry() %>%
      filter(!(Project_Key == input$global_project & Group_Key == target_grp))
    
    vars$metadata_registry(updated_meta)
    write_csv(updated_meta, "global_metadata_registry.csv")
    showNotification(paste0("Cohort '", target_grp, "' removed from registry."), type = "warning")
  })
  
  # --- Register New Research Subject ---
  observeEvent(input$save_animal_btn, {
    # 1. Ensure required fields are filled
    if (is.null(input$reg_animal_id) || trimws(input$reg_animal_id) == "") {
      showNotification("Please enter a Unique Subject ID before saving.", type = "warning")
      return()
    }
    
    if (is.null(input$reg_group) || trimws(input$reg_group) == "") {
      showNotification("Please assign an experimental cohort.", type = "warning")
      return()
    }
    
    clean_id <- trimws(input$reg_animal_id)
    current_db <- vars$animal_master_registry()
    
    # 2. Check for duplicate ID in the current project
    if (nrow(current_db) > 0) {
      duplicate_check <- current_db %>% filter(Project == input$global_project & Animal_ID == clean_id)
      if (nrow(duplicate_check) > 0) {
        showNotification(paste("Error: Subject identifier", clean_id, "is already registered in this project."), type = "error")
        return()
      }
    }
    
    # 3. Create and append the new animal record
    new_animal <- tibble(
      Project = input$global_project,
      Animal_ID = clean_id,
      Species = input$reg_species,
      Sex = input$reg_sex,
      Strain = input$reg_strain,
      Genotype = input$reg_genotype,
      DOB = as.character(input$reg_dob),
      Arrival_Date = as.character(input$reg_arrival),
      Cage_ID = input$reg_cage,
      Vendor = input$reg_vendor,
      Cohort_Group = input$reg_group,
      IACUC_Protocol = input$reg_iacuc,
      Status = "Active",
      Biobank_Tissues = "",
      Custom_Notes = ""
    )
    
    updated_db <- bind_rows(current_db, new_animal)
    vars$animal_master_registry(updated_db)
    write_csv(updated_db, "animal_registry.csv")
    
    updateTextInput(session, "reg_animal_id", value = "")
    showNotification(paste("Subject profile securely logged for", clean_id), type = "message")
  })
  
  # --- ACTIVE COHORT REASSIGNMENT & FULL MULTI-DATABASE CASCADE ---
  observeEvent(input$execute_reassign_btn, {
    req(input$registry_master_dt_table_rows_selected, input$reassign_target_group)
    
    df_active <- project_registry() %>% filter(Status == "Active")
    target_row <- df_active[input$registry_master_dt_table_rows_selected, ]
    target_id  <- target_row$Animal_ID
    new_group  <- input$reassign_target_group
    
    if (target_row$Cohort_Group == new_group) {
      showNotification(paste("Subject", target_id, "is already assigned to", new_group), type = "warning")
      return()
    }
    
    # Neutral slate fallback for pre-randomized assignments
    new_color <- if (grepl("(?i)unassigned|pre-?random", new_group)) {
      "#7f8c8d"
    } else {
      meta <- vars$metadata_registry() %>% filter(Project_Key == input$global_project)
      c_val <- meta %>% filter(Group_Key == new_group) %>% pull(Group_Color) %>% first()
      if (is.null(c_val) || is.na(c_val)) "#333333" else c_val
    }
    
    # 1. Update Master Animal Registry
    updated_reg <- vars$animal_master_registry() %>%
      mutate(Cohort_Group = ifelse(Project == input$global_project & Animal_ID == target_id, new_group, Cohort_Group))
    vars$animal_master_registry(updated_reg)
    write_csv(updated_reg, "animal_registry.csv")
    
    # 2. Cascade to Blood Pressure Database
    if (nrow(vars$bp_historical_data()) > 0) {
      updated_bp <- vars$bp_historical_data() %>%
        mutate(
          Group       = ifelse(Project == input$global_project & Animal_ID == target_id, new_group, Group),
          Group_Color = ifelse(Project == input$global_project & Animal_ID == target_id, new_color, Group_Color)
        )
      vars$bp_historical_data(updated_bp)
      write_csv(updated_bp, "bp_master_database.csv")
    }
    
    # 3. Cascade to PWV Database
    if (nrow(vars$pwv_historical_data()) > 0) {
      updated_pwv <- vars$pwv_historical_data() %>%
        mutate(
          Group       = ifelse(Project == input$global_project & Rat_ID == target_id, new_group, Group),
          Group_Color = ifelse(Project == input$global_project & Rat_ID == target_id, new_color, Group_Color)
        )
      vars$pwv_historical_data(updated_pwv)
      write_csv(updated_pwv, "pwv_master_database.csv")
    }
    
    # 4. Cascade to Longitudinal Weight Database
    if (nrow(vars$weight_historical_data()) > 0) {
      updated_wt <- vars$weight_historical_data() %>%
        mutate(
          Group       = ifelse(Project == input$global_project & Animal_ID == target_id, new_group, Group),
          Group_Color = ifelse(Project == input$global_project & Animal_ID == target_id, new_color, Group_Color)
        )
      vars$weight_historical_data(updated_wt)
      write_csv(updated_wt, "animal_weight_database.csv")
    }
    
    # 5. Cascade to Surgical Cannulation Database
    if (nrow(vars$cannulation_historical_data()) > 0) {
      updated_can <- vars$cannulation_historical_data() %>%
        mutate(Group = ifelse(Project == input$global_project & Animal_ID == target_id, new_group, Group))
      vars$cannulation_historical_data(updated_can)
      write_csv(updated_can, "cannulation_terminal_database.csv")
    }
    
    showNotification(
      paste0("Subject ", target_id, " successfully moved to [", new_group, "]. All historical assay records synchronized."),
      type = "message",
      duration = 6
    )
  })
  
  # --- Active Table View ---
  output$registry_master_dt_table <- renderDT({
    df <- project_registry() %>% filter(Status == "Active")
    if (nrow(df) == 0) return(datatable(tibble(`Notice` = "No active subjects registered."), options = list(dom = 't'), rownames = FALSE))
    
    processed_df <- df %>%
      mutate(
        DOB_Parsed = as.Date(DOB), 
        `Live Age (Weeks)` = round(as.numeric(Sys.Date() - DOB_Parsed) / 7, 1),
        `Actions` = paste0('<button class="btn btn-default btn-xs" onclick="Shiny.setInputValue(\'inspect_animal_trigger\', \'', Animal_ID, '\', {priority: \'event\'})">📝 Profile</button>')
      ) %>%
      select(`Actions`, `Subject ID` = Animal_ID, Species, `Cohort Group` = Cohort_Group, Strain, Genotype, Sex, `Live Age (Weeks)`, `Cage Slot` = Cage_ID)
    
    datatable(processed_df, selection = 'single', escape = FALSE, filter = 'top',
              options = list(pageLength = 25, order = list(list(1, 'asc')), autoWidth = TRUE))
  })
  
  # --- Archive Table View ---
  output$registry_archive_dt_table <- renderDT({
    df <- project_registry() %>% filter(Status == "Terminal")
    if (nrow(df) == 0) return(datatable(tibble(`Notice` = "Archive inventory clear."), options = list(dom = 't'), rownames = FALSE))
    
    processed_df <- df %>%
      mutate(
        `Actions` = paste0('<button class="btn btn-default btn-xs" onclick="Shiny.setInputValue(\'inspect_animal_trigger\', \'', Animal_ID, '\', {priority: \'event\'})">📝 Profile</button>')
      ) %>%
      select(`Actions`, `Subject ID` = Animal_ID, Species, `Historical Cohort` = Cohort_Group, Strain, Genotype, Sex, `Orig DOB` = DOB, `Cage Slot` = Cage_ID)
    
    datatable(processed_df, selection = 'single', escape = FALSE, filter = 'top',
              options = list(pageLength = 25, order = list(list(1, 'asc')), autoWidth = TRUE))
  })
  
  # --- Deep Profile Inspector & Biobank Modal ---
  observeEvent(input$inspect_animal_trigger, {
    target_id <- input$inspect_animal_trigger
    req(target_id)
    current_inspected_id(target_id)
    
    animal_record <- vars$animal_master_registry() %>% filter(Project == input$global_project & Animal_ID == target_id) %>% slice(1)
    req(nrow(animal_record) > 0)
    
    saved_tissues <- if (!is.na(animal_record$Biobank_Tissues) && animal_record$Biobank_Tissues != "") {
      str_split(animal_record$Biobank_Tissues, ",")[[1]]
    } else {
      character(0)
    }
    
    current_notes <- ifelse(is.na(animal_record$Custom_Notes), "", animal_record$Custom_Notes)
    
    showModal(modalDialog(
      title = paste("🔬 Deep Profile Inspector & Biobank Tracker:", target_id), size = "l", easyClose = TRUE,
      fluidRow(
        column(5,
               wellPanel(style = "background-color: #f8f9fa;",
                         tags$h4(tags$strong("Biological Blueprint")), hr(),
                         tags$p(tags$strong("Species Profile: "), animal_record$Species),
                         tags$p(tags$strong("Strain Matrix: "), animal_record$Strain),
                         tags$p(tags$strong("Genotype: "), animal_record$Genotype),
                         tags$p(tags$strong("Sex Line: "), animal_record$Sex),
                         tags$p(tags$strong("Current Status Label: "), tags$span(class=ifelse(animal_record$Status=="Active","label label-success","label label-warning"), animal_record$Status))
               )
        ),
        column(7,
               tags$h4(tags$strong("🎯 Dynamic Post-Mortem Biobank Vault")),
               tags$p(tags$small(tags$em("Check or uncheck the boxes below to immediately register harvested tissues for this subject."))),
               br(),
               checkboxGroupInput(
                 "modal_tissue_check", 
                 label = "Harvested Target Inventory:",
                 choices = master_tissue_list(),
                 selected = saved_tissues,
                 inline = TRUE
               ),
               hr(),
               tags$h5(tags$strong("➕ Expand Master Tissue Registry Options")),
               fluidRow(
                 column(8, textInput("modal_new_tissue_text", NULL, placeholder = "e.g., Liver - Formalin, Brain Cortex", width = "100%")),
                 column(4, actionButton("modal_add_tissue_btn", "Add Target Row", class = "btn-default btn-block"))
               ),
               hr(),
               textAreaInput("modal_animal_notes", "Custom Experimental / Surgical Notes Log:", 
                             value = current_notes, rows = 4, width = "100%", placeholder = "Enter physiological abnormalities, surgical anomalies, or litter source info...")
        )
      ),
      footer = tagList(
        modalButton("Dismiss"),
        actionButton("save_modal_notes_btn", "💾 Save Updates to Profile Registry", class = "btn-success")
      )
    ))
  })
  
  observeEvent(input$modal_add_tissue_btn, {
    new_tag <- trimws(input$modal_new_tissue_text)
    req(new_tag != "")
    
    current_list <- master_tissue_list()
    
    if (!(new_tag %in% current_list)) {
      updated_list <- c(current_list, new_tag)
      writeLines(updated_list, config_file)
      master_tissue_list(updated_list)
      
      updateCheckboxGroupInput(session, "modal_tissue_check", 
                               choices = updated_list, 
                               selected = input$modal_tissue_check,
                               inline = TRUE)
      
      updateTextInput(session, "modal_new_tissue_text", value = "")
      showNotification(paste("New target matrix row created:", new_tag), type = "message")
    }
  })
  
  observeEvent(input$save_modal_notes_btn, {
    req(input$save_modal_notes_btn)
    target_id <- current_inspected_id()
    req(target_id)
    
    tissue_string <- paste(input$modal_tissue_check, collapse = ",")
    
    updated_db <- vars$animal_master_registry() %>%
      mutate(
        Biobank_Tissues = ifelse(Project == input$global_project & Animal_ID == target_id, tissue_string, Biobank_Tissues),
        Custom_Notes = ifelse(Project == input$global_project & Animal_ID == target_id, input$modal_animal_notes, Custom_Notes)
      )
    
    vars$animal_master_registry(updated_db)
    write_csv(updated_db, "animal_registry.csv")
    
    removeModal()
    showNotification(paste("Biobank storage matrix updated securely for", target_id), type = "message")
  })
  
  # --- Archive / Reactivate / Delete Handlers ---
  observeEvent(input$reactivate_animal_btn, {
    req(input$registry_archive_dt_table_rows_selected)
    df <- project_registry() %>% filter(Status == "Terminal")
    target_row <- df[input$registry_archive_dt_table_rows_selected, ]
    
    updated_db <- vars$animal_master_registry() %>%
      mutate(Status = ifelse(Project == target_row$Project & Animal_ID == target_row$Animal_ID, "Active", Status))
    
    vars$animal_master_registry(updated_db)
    write_csv(updated_db, "animal_registry.csv")
    showNotification("Record successfully recovered and returned to active colony layout.", type = "message")
  })
  
  observeEvent(input$mark_terminal_btn, {
    req(input$registry_master_dt_table_rows_selected)
    df <- project_registry() %>% filter(Status == "Active")
    target_row <- df[input$registry_master_dt_table_rows_selected, ]
    
    updated_db <- vars$animal_master_registry() %>%
      mutate(Status = ifelse(Project == target_row$Project & Animal_ID == target_row$Animal_ID, "Terminal", Status))
    
    vars$animal_master_registry(updated_db)
    write_csv(updated_db, "animal_registry.csv")
    showNotification("Subject transitioned to post-mortem database tracker.", type = "warning")
  })
  
  observeEvent(input$delete_animal_btn, {
    req(input$registry_master_dt_table_rows_selected)
    df <- project_registry() %>% filter(Status == "Active")
    target_row <- df[input$registry_master_dt_table_rows_selected, ]
    
    updated_db <- vars$animal_master_registry() %>%
      filter(!(Project == target_row$Project & Animal_ID == target_row$Animal_ID))
    
    vars$animal_master_registry(updated_db)
    write_csv(updated_db, "animal_registry.csv")
    showNotification("Animal record completely purged from laboratory database tracking.", type = "error")
  })
}