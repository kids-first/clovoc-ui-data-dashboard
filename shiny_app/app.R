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
    data <- dataset[["Patient"]]
    if (!is.null(input$research_study_identifier)) {
      data <-
        data[
          data$`ResearchStudy Identifier` %in%
            input$research_study_identifier,
        ]
    }
    if (!is.null(input$group_identifier)) {
      data <- data[data$`Group Identifier` %in% input$group_identifier, ]
    }
    if (!is.null(input$patient_id)) {
      data <- data[data$`Patient Identifier` %in% input$patient_id, ]
    }
    if (!is.null(input$race)) {
      data <- data[data$Race %in% input$race, ]
    }
    if (!is.null(input$ethnicity)) {
      data <- data[data$Ethnicity %in% input$ethnicity, ]
    }
    if (!is.null(input$gender)) {
      data <- data[data$Gender %in% input$gender, ]
    }
    return(data)
  })

  ### Filter condition dataset==================================================
  condition_data <- reactive({
    data <- dataset[["Condition"]]
    if (!is.null(input$condition_patient_id)) {
      data <- data[data$`Patient Identifier` %in% input$condition_patient_id, ]
    }
    if (!is.null(input$clinical_status)) {
      data <- data[data$`Clinical Status` %in% input$clinical_status, ]
    }
    if (!is.null(input$verification_status)) {
      data <- data[data$`Verification Status` %in% input$verification_status, ]
    }
    if (!is.null(input$condition_name)) {
      data <- data[data$`Condition Name` %in% input$condition_name, ]
    }
    if (!is.null(input$condition_uri)) {
      data <- data[data$`Condition Ontology URI` %in% input$condition_uri, ]
    }
    if (!is.null(input$condition_code)) {
      data <- data[data$`Condition Code` %in% input$condition_code, ]
    }
    if (!is.null(input$condition_body_site_name)) {
      data <- data[data$`Body Site Name` %in% input$condition_body_site_name, ]
    }
    if (!is.null(input$condition_body_site_uri)) {
      data <- data[data$`Body Site Ontology URI` %in%
                     input$condition_body_site_uri, ]
    }
    if (!is.null(input$condition_body_site_code)) {
      data <- data[data$`Body Site Code` %in% input$condition_body_site_code, ]
    }
    return(data)
  })

  ### Filter specimen dataset===================================================
  specimen_data <- reactive({
    data <- dataset[["Specimen"]]
    if (!is.null(input$specimen_patient_id)) {
      data <- data[data$`Patient Identifier` %in% input$specimen_patient_id, ]
    }
    if (!is.null(input$collection_body_name)) {
      data <- data[data$`Body Site Name` %in% input$collection_body_name, ]
    }
    if (!is.null(input$collection_body_site_uri)) {
      data <- data[data$`Body Site Ontology URI` %in%
                     input$collection_body_site_uri, ]
    }
    if (!is.null(input$collection_body_code)) {
      data <- data[data$`Body Site Code` %in% input$collection_body_code, ]
    }
    if (!is.null(input$specimen_status)) {
      data <- data[data$`Specimen Status` %in% input$specimen_status, ]
    }
    if (!is.null(input$specimen_type_name)) {
      data <- data[data$`Specimen Type Name` %in% input$specimen_type_name, ]
    }
    if (!is.null(input$specimen_type_uri)) {
      data <- data[data$`Specimen Type Ontology URI` %in%
                     input$specimen_type_uri, ]
    }
    if (!is.null(input$specimen_type_code)) {
      data <- data[data$`Specimen Type Code` %in% input$specimen_type_code, ]
    }
    return(data)
  })

  ### Filter document reference dataset=========================================
  docref_data <- reactive({
    data <- dataset[["DocumentReference"]]
    if (!is.null(input$docref_patient_id)) {
      data <- data[data$`Patient Identifier` %in% input$docref_patient_id]
    }
    if (!is.null(input$docref_status)) {
      data <- data[data$`DocumentReference Status` %in% input$docref_status, ]
    }
    if (!is.null(input$doc_status)) {
      data <- data[data$`Document Status` %in% input$doc_status, ]
    }
    if (!is.null(input$doc_type)) {
      data <- data[data$`Document Type` %in% input$doc_type, ]
    }
    if (!is.null(input$experiment_strategy)) {
      data <- data[data$`Experiment Strategy` %in% input$experiment_strategy, ]
    }
    if (!is.null(input$data_category)) {
      data <- data[data$`Data Category` %in% input$data_category, ]
    }
    if (!is.null(input$docref_uri)) {
      data <- data[data$`URL` %in% input$docref_uri, ]
    }
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
                      "condition_uri",
                      selected = NULL,
                      choices = sort(unique(
                        dataset[["Condition"]]$`Condition Ontology URI`)))
  })
  observe({
    updatePickerInput(session,
                      "condition_code",
                      selected = NULL,
                      choices = sort(unique(
                        dataset[["Condition"]]$`Condition Code`)))
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
                      "condition_body_site_uri",
                      selected = NULL,
                      choices = sort(unique(
                        dataset[["Condition"]]$`Body Site Ontology URI`)))
  })
  observe({
    updatePickerInput(session,
                      "condition_body_site_code",
                      selected = NULL,
                      choices = sort(unique(
                        dataset[["Condition"]]$`Body Site Code`)))
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
                      "specimen_body_site_uri",
                      selected = NULL,
                      choices = sort(unique(
                        dataset[["Specimen"]]$`Body Site Ontology URI`)))
  })
  observe({
    updatePickerInput(session,
                      "collection_body_code",
                      selected = NULL,
                      choices = sort(unique(
                        dataset[["Specimen"]]$`Body Site Code`)))
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
                      "specimen_type_uri",
                      selected = NULL,
                      choices = sort(unique(
                        dataset[["Specimen"]]$`Specimen Type Ontology URI`)))
  })
  observe({
    updatePickerInput(session,
                      "specimen_type_code",
                      selected = NULL,
                      choices = sort(unique(
                        dataset[["Specimen"]]$`Specimen Type Code`)))
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
