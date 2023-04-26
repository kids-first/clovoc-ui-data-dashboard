# Load packages=================================================================
library(shiny)
library(shinyWidgets)
library(shinydashboard)

# UI component==================================================================
picker_defaults <- c(
  `actions-box` = TRUE,
  `dropup-auto` = TRUE,
  `live-search` = TRUE,
  `window-padding` = "[40,0,40,0]"
)

ui <- dashboardPage(
  ## Dashboard Header===========================================================
  dashboardHeader(title = span(tagList(icon("fire"), "CLOVoc Data Dashboard"))),
  ## Dashboard Sidebar==========================================================
  dashboardSidebar(
    sidebarMenu(
      menuItem("Combined Data", tabName = "combined_tab"), #TODO Add icon
      menuItem("Patients", tabName = "patient_tab", icon = icon("user")),
      menuItem("Conditions", tabName = "condition_tab", icon = icon("tags")),
      menuItem("Specimens", tabName = "specimen_tab", icon = icon("vial")),
      menuItem("Document References", tabName = "docref_tab",
               icon = icon("file-medical"))
    )
  ),
  ## Dashboard Body=============================================================
  dashboardBody(
    tabItems(
      ### Combined Data tab====
      tabItem(tabName = "combined_tab",
              fluidRow(h2("Combined Data")),
              fluidRow(DT::dataTableOutput("combined_table"))),
      ### Patient tab======
      tabItem(tabName = "patient_tab",
              #### Patient tab title----
              fluidRow(h2("Patient Data")),
              #### Patient tab filters
              fluidRow(
                fluidRow(
                  box(title = "Research Study Identifier:",
                      width = 2,
                      pickerInput("research_study_identifier",
                                  label = NULL,
                                  choices = NULL,
                                  multiple = TRUE,
                                  options = picker_defaults)),
                  box(title = "Group Identifier:",
                      width = 2,
                      pickerInput("group_identifier",
                                  label = NULL,
                                  choices = NULL,
                                  multiple = TRUE,
                                  options = picker_defaults)),
                  box(title = "Patient Identifier:",
                      width = 2,
                      pickerInput("patient_id",
                                  label = NULL,
                                  choices = NULL,
                                  multiple = TRUE,
                                  options = picker_defaults)),
                  box(title = "Race:",
                      width = 2,
                      pickerInput("race",
                                  label = NULL,
                                  choices = NULL,
                                  multiple = TRUE,
                                  options = picker_defaults)),
                  box(title = "Ethnicity:",
                      width = 2,
                      pickerInput("ethnicity",
                                  label = NULL,
                                  choices = NULL,
                                  multiple = TRUE,
                                  options = picker_defaults)),
                  box(title = "Gender:",
                      width = 2,
                      pickerInput("gender",
                                  label = NULL,
                                  choices = NULL,
                                  multiple = TRUE,
                                  options = picker_defaults)))
              ),
              fluidRow(DT::dataTableOutput("patient_table"))),
      ### Condition tab======
      tabItem(tabName = "condition_tab",
              fluidRow(h2("Condition Data Tab")),
              fluidRow(
                column(2,
                       box(title = "Patient Identifier:",
                           width = NULL,
                           pickerInput("condition_patient_id",
                                       label = NULL,
                                       choices = NULL,
                                       multiple = TRUE,
                                       options = picker_defaults)),
                       box(title = "Clinical Status:",
                           width = NULL,
                           pickerInput("clinical_status",
                                       label = NULL,
                                       choices = NULL,
                                       multiple = TRUE,
                                       options = picker_defaults))
                ),
                column(2,
                       box(title = "Verification Status:",
                           width = NULL,
                           pickerInput("verification_status",
                                       label = NULL,
                                       choices = NULL,
                                       multiple = TRUE,
                                       options = picker_defaults)),
                       box(title = "Condition Name:",
                           width = NULL,
                           pickerInput("condition_name",
                                       label = NULL,
                                       choices = NULL,
                                       multiple = TRUE,
                                       options = picker_defaults))
                ),
                column(3,
                       box(title = "Condition Ontology URI:",
                           width = NULL,
                           pickerInput("condition_uri",
                                       label = NULL,
                                       choices = NULL,
                                       multiple = TRUE,
                                       options = picker_defaults)),
                       box(title = "Condition Code:",
                           width = NULL,
                           pickerInput("condition_code",
                                       label = NULL,
                                       choices = NULL,
                                       multiple = TRUE,
                                       options = picker_defaults))
                ),
                column(3,
                       box(title = "Body Site Name:",
                           width = NULL,
                           pickerInput("condition_body_site_name",
                                       label = NULL,
                                       choices = NULL,
                                       multiple = TRUE,
                                       options = picker_defaults)),
                       box(title = "Body Site Ontology URI:",
                           width = NULL,
                           pickerInput("condition_body_site_uri",
                                       label = NULL,
                                       choices = NULL,
                                       multiple = TRUE,
                                       options = picker_defaults))),
                column(2,
                       box(title = "Body Site Code:",
                           width = NULL,
                           pickerInput("condition_body_site_code",
                                       label = NULL,
                                       choices = NULL,
                                       multiple = TRUE,
                                       options = picker_defaults))
                )),
              fluidRow(DT::dataTableOutput("condition_table"))),
      ### Specimen tab======
      tabItem(tabName = "specimen_tab",
              fluidRow(h2("Specimen Data Tab")),
              fluidRow(
                column(2,
                       box(title = "Patient Identifier:",
                           width = NULL,
                           pickerInput("specimen_patient_id",
                                       label = NULL,
                                       choices = NULL,
                                       multiple = TRUE,
                                       options = picker_defaults)),
                       box(title = "Body Site Name:",
                           width = NULL,
                           pickerInput("collection_body_type",
                                       label = NULL,
                                       choices = NULL,
                                       multiple = TRUE,
                                       options = picker_defaults))),
                column(3,
                       box(title = "Body Site Ontology URI:",
                           width = NULL,
                           pickerInput("specimen_body_site_uri",
                                       label = NULL,
                                       choices = NULL,
                                       multiple = TRUE,
                                       options = picker_defaults)),
                       box(title = "Body Site Code:",
                           width = NULL,
                           pickerInput("collection_body_code",
                                       label = NULL,
                                       choices = NULL,
                                       multiple = TRUE,
                                       options = picker_defaults))),
                column(2,
                       box(title = "Specimen Identifier:",
                           width = NULL,
                           pickerInput("specimen_identifier",
                                       label = NULL,
                                       choices = NULL,
                                       multiple = TRUE,
                                       options = picker_defaults)),
                       box(title = "Specimen Status:",
                           width = NULL,
                           pickerInput("specimen_status",
                                       label = NULL,
                                       choices = NULL,
                                       multiple = TRUE,
                                       options = picker_defaults))),
                column(3,
                       box(title = "Specimen Type Name:",
                           width = NULL,
                           pickerInput("specimen_type_name",
                                       label = NULL,
                                       choices = NULL,
                                       multiple = TRUE,
                                       options = picker_defaults)),
                       box(title = "Specimen Type Ontology URI:",
                           width = NULL,
                           pickerInput("specimen_type_uri",
                                       label = NULL,
                                       choices = NULL,
                                       multiple = TRUE,
                                       options = picker_defaults))),
                column(2,
                       box(title = "Specimen Type Code:",
                           width = NULL,
                           pickerInput("specimen_type_code",
                                       label = NULL,
                                       choices = NULL,
                                       multiple = TRUE,
                                       options = picker_defaults)))
              ),
              fluidRow(DT::dataTableOutput("specimen_table"))),
      ### Document Reference tab======
      tabItem(tabName = "docref_tab",
              fluidRow(h2("Document Reference Tab")),
              fluidRow(
                column(3,
                       box(title = "Patient Identifier:",
                           width = NULL,
                           pickerInput("docref_patient_id",
                                       label = NULL,
                                       choices = NULL,
                                       multiple = TRUE,
                                       options = picker_defaults)),
                       box(title = "DocumentReference Status:",
                           width = NULL,
                           pickerInput("docref_status",
                                       label = NULL,
                                       choices = NULL,
                                       multiple = TRUE,
                                       options = picker_defaults))),
                column(3,
                       box(title = "Document Status:",
                           width = NULL,
                           pickerInput("doc_status",
                                       label = NULL,
                                       choices = NULL,
                                       multiple = TRUE,
                                       options = picker_defaults)),
                       box(title = "Document Type:",
                           width = NULL,
                           pickerInput("doc_type",
                                       label = NULL,
                                       choices = NULL,
                                       multiple = TRUE,
                                       options = picker_defaults))),
                column(3,
                       box(title = "Experiment Strategy:",
                           width = NULL,
                           pickerInput("experiment_strategy",
                                       label = NULL,
                                       choices = NULL,
                                       multiple = TRUE,
                                       options = picker_defaults)),
                       box(title = "Data Category:",
                           width = NULL,
                           pickerInput("data_category",
                                       label = NULL,
                                       choices = NULL,
                                       multiple = TRUE,
                                       options = picker_defaults))),
                column(3,
                       box(title = "URI:",
                           width = NULL,
                           pickerInput("docref_uri",
                                       label = NULL,
                                       choices = NULL,
                                       multiple = TRUE,
                                       options = picker_defaults))
                )
              ),
              fluidRow(DT::dataTableOutput("docref_table")))
    )
  )
)

# Server component==============================================================
server <- function(input, output, session) {
  ## Retrieve pinned data=======================================================
  board <- pins::board_rsconnect()
  dataset <- pins::pin_read(board, "nemarichc/clovoc-data-cookie")

  ## Separate and filter datasets===============================================
  ### Filter application functions==============================================
  apply_tab_filters <- function(data, tabfilter_list) {
    for (n in 1:NROW(tabfilter_list)) {
      column_name <- tabfilter_list[[n, 1]]
      filter_name <- tabfilter_list[[n, 2]]
      if (!is.null(input[[filter_name]])) {
        data <- data[data[[column_name]] %in% input[[filter_name]], ]
      }
    }
    return(data)
  }

  apply_cross_filters <- function(data, crossfilter_list) {
    apply <- !(is.null(cross_filters$patient) ||
                 is.null(cross_filters$condition) ||
                 is.null(cross_filters$specimen) ||
                 is.null(cross_filters$docref))
    if (apply) return(data)
    for (n in 1:NROW(crossfilter_list)) {
      filter_values <- cross_filters[[crossfilter_list[[n, 2]]]][["Patient Identifier"]]
      if (!is.null(filter_values)) {
        data <- data[data[["Patient Identifier"]] %in% filter_values, ]
      }
    }
    return(data)
  }
  ### Cross-tab filtering=======================================================
  cross_filters <- reactiveValues(patient = NULL,
                                  condition = NULL,
                                  specimen = NULL,
                                  docref = NULL)

  observeEvent(input$crossfilter_reset, {
    cross_filters$patient <- NULL
    cross_filters$condition <- NULL
    cross_filters$specimen <- NULL
    cross_filters$docref <- NULL
  })

  observeEvent(input$crossfilter_patient, {
    cross_filters$patient <- dataset[["Patient"]] |>
      apply_tab_filters(tibble::tribble(
        ~column_name,               ~filter_name,
        "ResearchStudy Identifier", "research_study_identifier",
        "Group Identifier",         "group_identifier",
        "Patient Identifier",       "patient_id",
        "Race",                     "race",
        "Ethnicity",                "ethnicity",
        "Gender",                   "gender"
      )) |>
      unique()
  })

  observeEvent(input$crossfilter_condition, {
    cross_filters$condition <- dataset[["Condition"]] |>
      apply_tab_filters(tibble::tribble(
        ~column_name,             ~filter_name,
        "Patient Identifier",     "condition_patient_id",
        "Clinical Status",        "clinical_status",
        "Verification Status",    "verification_status",
        "Condition Name",         "condition_name",
        "Body Site Name",         "condition_body_site_name"
      )) |>
      unique()
  })

  observeEvent(input$crossfilter_specimen, {
    cross_filters$specimen <- dataset[["Specimen"]]|>
      apply_tab_filters(tibble::tribble(
        ~column_name,                 ~filter_name,
        "Patient Identifier",         "specimen_patient_id",
        "Body Site Name",             "collection_body_name",
        "Specimen Status",            "specimen_status",
        "Specimen Type Name",         "specimen_type_name"
      )) |>
      unique()
  })

  observeEvent(input$crossfilter_docref, {
    cross_filters$docref <- dataset[["DocumentReference"]] |>
      apply_tab_filters(tibble::tribble(
        ~column_name,               ~filter_name,
        "Patient Identifier",       "docref_patient_id",
        "DocumentReference Status", "docref_status",
        "Document Status",          "doc_status",
        "Document Type",            "doc_type",
        "Experiment Strategy",      "experiment_strategy",
        "Data Category",            "data_category",
        "URL",                      "docref_uri"
      )) |>
      unique()
  })

  ### Create combined dataset===================================================
  combined_data <- reactive({
    data <- dataset[["Patient"]] |>
      dplyr::left_join(dataset[["Condition"]],
                       by = "Patient Identifier") |>
      dplyr::left_join(dataset[["Specimen"]],
                       by = "Patient Identifier") |>
      dplyr::left_join(dataset[["DocumentReference"]],
                       by = "Patient Identifier")
    return(data)
  })

  ### Filter patient dataset====================================================
  patient_data <- reactive({
    data <- dataset[["Patient"]] |>
      apply_tab_filters(tibble::tribble(
        ~column_name,               ~filter_name,
        "ResearchStudy Identifier", "research_study_identifier",
        "Group Identifier",         "group_identifier",
        "Patient Identifier",       "patient_id",
        "Race",                     "race",
        "Ethnicity",                "ethnicity",
        "Gender",                   "gender"
      )) |>
      apply_cross_filters(tibble::tribble(
        ~tab_name,        ~filter_name,
        "condition_data", "condition",
        "specimen_data",  "specimen",
        "docref_data",    "docref"
      ))
    return(data)
  })

  ### Filter condition dataset==================================================
  condition_data <- reactive({
    data <- dataset[["Condition"]] |>
      apply_tab_filters(tibble::tribble(
        ~column_name,             ~filter_name,
        "Patient Identifier",     "condition_patient_id",
        "Clinical Status",        "clinical_status",
        "Verification Status",    "verification_status",
        "Condition Name",         "condition_name",
        "Body Site Name",         "condition_body_site_name"
      )) |>
      apply_cross_filters(tibble::tribble(
        ~tab_name,       ~filter_name,
        "patient_data",  "patient",
        "specimen_data", "specimen",
        "docref_data",   "docref"
      ))
    return(data)
  })

  ### Filter specimen dataset===================================================
  specimen_data <- reactive({
    data <- dataset[["Specimen"]]|>
      apply_tab_filters(tibble::tribble(
        ~column_name,                 ~filter_name,
        "Patient Identifier",         "specimen_patient_id",
        "Body Site Name",             "collection_body_name",
        "Specimen Status",            "specimen_status",
        "Specimen Type Name",         "specimen_type_name"
      )) |>
      apply_cross_filters(tibble::tribble(
        ~tab_name,        ~filter_name,
        "patient_data",   "patient",
        "condition_data", "condition",
        "docref_data",    "docref"
      ))
    return(data)
  })

  ### Filter document reference dataset=========================================
  docref_data <- reactive({
    data <- dataset[["DocumentReference"]] |>
      apply_tab_filters(tibble::tribble(
        ~column_name,               ~filter_name,
        "Patient Identifier",       "docref_patient_id",
        "DocumentReference Status", "docref_status",
        "Document Status",          "doc_status",
        "Document Type",            "doc_type",
        "Experiment Strategy",      "experiment_strategy",
        "Data Category",            "data_category",
        "URL",                      "docref_uri"
      )) |>
      apply_cross_filters(tibble::tribble(
        ~tab_name,        ~filter_name,
        "patient_data",   "patient",
        "condition_data", "condition",
        "specimen_data",  "specimen"
      ))
    return(data)
  })

  ## Populate filter options====================================================
  observe({
    updatePickerInput(session,
                      "research_study_identifier",
                      selected = NULL,
                      choices = sort(unique(
                        dataset[["Patient"]]$`ResearchStudy Identifier`)))
  })
  observe({
    updatePickerInput(session,
                      "group_identifier",
                      selected = NULL,
                      choices = sort(unique(
                        dataset[["Patient"]]$`Group Identifier`)))
  })
  observe({
    updatePickerInput(session,
                      "patient_id",
                      selected = NULL,
                      choices = sort(unique(
                        dataset[["Patient"]]$`Patient Identifier`)))
  })
  observe({
    updatePickerInput(session,
                      "race",
                      selected = NULL,
                      choices = sort(unique(dataset[["Patient"]]$Race)))
  })
  observe({
    updatePickerInput(session,
                      "ethnicity",
                      selected = NULL,
                      choices = sort(unique(
                        dataset[["Patient"]]$Ethnicity)))
  })
  observe({
    updatePickerInput(session,
                      "gender",
                      selected = NULL,
                      choices = sort(unique(
                        dataset[["Patient"]]$Gender)))
  })
  observe({
    updatePickerInput(session,
                      "condition_patient_id",
                      selected = NULL,
                      choices = sort(unique(
                        dataset[["Condition"]]$`Patient Identifier`)))
  })
  observe({
    updatePickerInput(session,
                      "clinical_status",
                      selected = NULL,
                      choices = sort(unique(
                        dataset[["Condition"]]$`Clinical Status`)))
  })
  observe({
    updatePickerInput(session,
                      "verification_status",
                      selected = NULL,
                      choices = sort(unique(
                        dataset[["Condition"]]$`Verification Status`)))
  })
  observe({
    updatePickerInput(session,
                      "condition_name",
                      selected = NULL,
                      choices = sort(unique(
                        dataset[["Condition"]]$`Condition Name`)))
  })
  observe({
    updatePickerInput(session,
                      "condition_body_site_name",
                      selected = NULL,
                      choices = sort(unique(
                        dataset[["Condition"]]$`Body Site Name`)))
  })
  observe({
    updatePickerInput(session,
                      "specimen_patient_id",
                      selected = NULL,
                      choices = sort(unique(
                        dataset[["Specimen"]]$`Patient Identifier`)))
  })
  observe({
    updatePickerInput(session,
                      "collection_body_type",
                      selected = NULL,
                      choices = sort(unique(
                        dataset[["Specimen"]]$`Body Site Name`)))
  })
  observe({
    updatePickerInput(session,
                      "specimen_identifier",
                      selected = NULL,
                      choices = sort(unique(
                        dataset[["Specimen"]]$`Specimen Identifier`)))
  })
  observe({
    updatePickerInput(session,
                      "specimen_status",
                      selected = NULL,
                      choices = sort(unique(
                        dataset[["Specimen"]]$`Specimen Status`)))
  })
  observe({
    updatePickerInput(session,
                      "specimen_type_name",
                      selected = NULL,
                      choices = sort(unique(
                        dataset[["Specimen"]]$`Specimen Type Name`)))
  })
  observe({
    updatePickerInput(session,
                      "docref_patient_id",
                      selected = NULL,
                      choices = sort(unique(
                        dataset[["DocumentReference"]]$`Patient Identifier`)))
  })
  observe({
    updatePickerInput(
      session,
      "docref_status",
      selected = NULL,
      choices = sort(unique(
        dataset[["DocumentReference"]]$`DocumentReference Status`)))
  })
  observe({
    updatePickerInput(
      session,
      "doc_status",
      selected = NULL,
      choices = sort(unique(dataset[["DocumentReference"]]$`Document Status`)))
  })
  observe({
    updatePickerInput(session,
                      "doc_type",
                      selected = NULL,
                      choices = sort(unique(
                        dataset[["DocumentReference"]]$`Document Type`)))
  })
  observe({
    updatePickerInput(
      session,
      "experiment_strategy",
      selected = NULL,
      choices = sort(unique(
        dataset[["DocumentReference"]]$`Experiment Strategy`)))
  })
  observe({
    updatePickerInput(session,
                      "data_category",
                      selected = NULL,
                      choices = sort(unique(
                        dataset[["DocumentReference"]]$`Data Category`)))
  })
  observe({
    updatePickerInput(session,
                      "docref_uri",
                      selected = NULL,
                      choices = sort(unique(
                        dataset[["DocumentReference"]]$`URL`)))
  })

  ## Output datasets to UI======================================================
  ### Output combined dataset===================================================
  output$combined_table <- DT::renderDataTable({
    combined_data()
  },
  extensions = list("Buttons" = NULL),
  options = list(
    pageLength = 25,
    dom = "Bfrtip",
    buttons = list("copy",
                   "print",
                   list(extend = "collection",
                        buttons = c("csv", "excel", "pdf"),
                        text = "Download")),
    scrollX = TRUE,
    scrollY = 500)
  )

  ### Output patient dataset====================================================
  output$patient_table <- DT::renderDataTable({
    patient_data()
  },
  extensions = list("Buttons" = NULL),
  options = list(
    pageLength = 25,
    dom = "Bfrtip",
    buttons = list("copy",
                   "print",
                   list(extend = "collection",
                        buttons = c("csv", "excel", "pdf"),
                        text = "Download"),
                   list(extend = "collection",
                        text = "Apply these filters across tabs",
                        action = DT::JS("function( e, dt, node, config ) {
                                            Shiny.setInputValue('crossfilter_patient', true, {priority: 'event'});
                                            alert( 'Cross-tab filtering applied' );
                                        }")),
                   list(extend = "collection",
                        text = "Reset cross-tab filtering",
                        action = DT::JS("function( e, dt, node, config ) {
                                            Shiny.setInputValue('crossfilter_reset', true, {priority: 'event'});
                                            alert( 'Filtering across tabs reset' );
                                        }"))
    )
  ))

  ### Output condition dataset==================================================
  output$condition_table <- DT::renderDataTable({
    condition_data()
  },
  extensions = list("Buttons" = NULL),
  options = list(
    pageLength = 25,
    dom = "Bfrtip",
    buttons = list("copy", "print", list(extend = "collection",
                                         buttons = c("csv", "excel", "pdf"),
                                         text = "Download"),
                   list(extend = "collection",
                        text = "Apply these filters across tabs",
                        action = DT::JS("function( e, dt, node, config ) {
                                            Shiny.setInputValue('crossfilter_condition', true, {priority: 'event'});
                                            alert( 'Cross-tab filtering applied' );
                                        }")),
                   list(extend = "collection",
                        text = "Reset cross-tab filtering",
                        action = DT::JS("function( e, dt, node, config ) {
                                            Shiny.setInputValue('crossfilter_reset', true, {priority: 'event'});
                                            alert( 'Filtering across tabs reset' );
                                        }"))
    )
  ))

  ### Output specimen dataset===================================================
  output$specimen_table <- DT::renderDataTable({
    specimen_data()
  },
  extensions = list("Buttons" = NULL),
  options = list(
    pageLength = 25,
    dom = "Bfrtip",
    buttons = list("copy",
                   "print",
                   list(extend = "collection",
                        buttons = c("csv", "excel", "pdf"),
                        text = "Download"),
                   list(extend = "collection",
                        text = "Apply these filters across tabs",
                        action = DT::JS("function( e, dt, node, config ) {
                                            Shiny.setInputValue('crossfilter_specimen', true, {priority: 'event'});
                                            alert( 'Cross-tab filtering applied' );
                                        }")),
                   list(extend = "collection",
                        text = "Reset cross-tab filtering",
                        action = DT::JS("function( e, dt, node, config ) {
                                            Shiny.setInputValue('crossfilter_reset', true, {priority: 'event'});
                                            alert( 'Filtering across tabs reset' );
                                       }"))
    )
  ))

  ### Output document reference dataset=========================================
  output$docref_table <- DT::renderDataTable({
    docref_data()
  },
  extensions = list("Buttons" = NULL),
  options = list(
    pageLength = 25,
    dom = "Bfrtip",
    buttons = list("copy",
                   "print",
                   list(extend = "collection",
                        buttons = c("csv", "excel", "pdf"),
                        text = "Download"),
                   list(extend = "collection",
                        text = "Apply these filters across tabs",
                        action = DT::JS("function( e, dt, node, config ) {
                                            Shiny.setInputValue('crossfilter_docref', true, {priority: 'event'});
                                            alert( 'Cross-tab filtering applied' );
                                        }")),
                   list(extend = "collection",
                        text = "Reset cross-tab filtering",
                        action = DT::JS("function( e, dt, node, config ) {
                                            Shiny.setInputValue('crossfilter_reset', true, {priority: 'event'});
                                            alert( 'Filtering across tabs reset' );
                                       }"))
    ))
  )
}

# Bind UI and server to an application==========================================
shinyApp(ui = ui, server = server)
