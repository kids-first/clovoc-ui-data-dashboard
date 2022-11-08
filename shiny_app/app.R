library(shiny)
library(shinyWidgets)
library(shinydashboard)

# UI component
ui <- dashboardPage(
  dashboardHeader(title = span(tagList(icon("fire"), "CLOVoc Data Dashboard"))),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Patients", tabName = "patient_tab", icon = icon("user")),
      menuItem("Conditions", tabName = "condition_tab", icon = icon("tags")),
      menuItem("Specimens", tabName = "specimen_tab", icon = icon("vial")),
      menuItem("Document References", tabName = "docref_tab",
               icon = icon("file-medical"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "patient_tab",
              fluidRow(h2("Patient Data")),
              fluidRow(
                box(title = "Search a Research Study Identifier", width = 3,
                    textInput("research_study_identifier",
                              label = NULL)),
                # checkboxGroupInput("group_identifier",
                #                    "Group Identifier",
                #                    choices = ""),
                box(title = "Filter by Race:", width = 3,
                    selectInput("race",
                                label = NULL,
                                choices = "",
                                multiple = TRUE)),
                box(title = "Filter by Ethnicity:", width = 3,
                    checkboxGroupInput("ethnicity",
                                       label = NULL,
                                       choices = "")),
                box(title = "Filter by Gender:", width = 3,
                    checkboxGroupInput("gender",
                                       label = NULL,
                                       choices = ""))
              ),
              fluidRow(DT::dataTableOutput("patient_table"))),
      tabItem(tabName = "condition_tab",
              fluidRow(h2("Condition Data Tab")),
              fluidRow(
                box(title = "Filter by Clinical Status:", width = 4,
                    checkboxGroupInput("clinical_status",
                                       label = NULL,
                                       choices = "")),
                box(title = "Filter by Verification Status:", width = 4,
                    checkboxGroupInput("verification_status",
                                       label = NULL,
                                       choices = "")),
                box(title = "Search a Condition Name:", width = 4,
                    textInput("condition_name",
                              label = NULL))),
              fluidRow(
                box(title = "Filter by Condition Code:", width = 4,
                    pickerInput("condition_code",
                                label = NULL,
                                choices = "",
                                multiple = TRUE,
                                options = c(`actions-box` = TRUE,
                                            `dropup-auto` = TRUE,
                                            `window-padding` = "[40,0,40,0]"))),
                box(title = "Search a Body Name:", width = 4,
                    textInput("body_site_name",
                              label = NULL)),
                box(title = "Filter by Body Site Code:", width = 4,
                    checkboxGroupInput("body_site_code",
                                       label = NULL,
                                       choices = ""))),
              fluidRow(DT::dataTableOutput("condition_table"))),
      tabItem(tabName = "specimen_tab",
              fluidRow(h2("Specimen Data Tab")),
              fluidRow(
                box(title = "Filter by Specimen Status:", width = 4,
                    checkboxGroupInput("specimen_status",
                                       label = NULL,
                                       choices = "")),
                box(title = "Search a Specimen Type Name:", width = 4,
                    textInput("specimen_type_name",
                              label = NULL)),
                box(title = "Filter by Specimen Type Code:", width = 4,
                    pickerInput("specimen_type_code",
                                label = NULL,
                                choices = "",
                                multiple = TRUE,
                                options = c(`actions-box` = TRUE,
                                            `dropup-auto` = TRUE,
                                            `window-padding` = "[40,0,40,0]")))),
              fluidRow(
                box(title = "Search a Body Site Name:", width = 4,
                    textInput("collection_body_type",
                              label = NULL)),
                box(title = "Filter by Body Site Code:", width = 4,
                    checkboxGroupInput("collection_body_code",
                                       label = NULL,
                                       choices = ""))),
              fluidRow(DT::dataTableOutput("specimen_table"))),
      tabItem(tabName = "docref_tab",
              fluidRow(h2("Document Reference Tab")),
              fluidRow(
                box(title = "Filter by DocumentReference Status:", width = 4,
                    checkboxGroupInput("docref_status",
                                       label = NULL,
                                       choices = "")),
                box(title = "Filter by Document Status:", width = 4,
                    checkboxGroupInput("doc_status",
                                       label = NULL,
                                       choices = "")),
                box(title = "Filter by Document Type:", width = 4,
                    pickerInput("doc_type",
                                label = NULL,
                                choices = "",
                                multiple = TRUE,
                                options = c(`actions-box` = TRUE,
                                            `dropup-auto` = TRUE,
                                            `window-padding` = "[40,0,40,0]")))),
              fluidRow(
                box(title = "Filter by Experiment Strategy:", width = 4,
                    checkboxGroupInput("experiment_strategy",
                                       label = NULL,
                                       choices = "")),
                box(title = "Filter by Data Category:", width = 4,
                    checkboxGroupInput("data_category",
                                       label = NULL,
                                       choices = ""))),
              fluidRow(DT::dataTableOutput("docref_table")))
    )
  )
)

# server component
server <- function(input, output, session) {
  board <- pins::board_rsconnect()
  dataset <- pins::pin_read(board, "nemarichc/clovoc-data-cookie")

  patient_data <- reactive({
    data <- dataset[["Patient"]]
    # if (!is.null(input$research_study_identifier)) {
    #     data <- data[
    #         data$`ResearchStudy Identifier` %in% input$research_study_identifier,
    #     ]
    # }
    # if (!is.null(input$group_identifier)) {
    #   data <- data[data$`Group Identifier` %in% input$group_identifier, ]
    # }
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

  condition_data <- reactive({
    data <- dataset[["Condition"]]
    if (!is.null(input$clinical_status)) {
      data <- data[data$`Clinical Status` %in% input$clinical_status, ]
    }
    if (!is.null(input$verification_status)) {
      data <- data[data$`Verification Status` %in% input$verification_status, ]
    }
    if (!is.null(input$condition_code)) {
      data <- data[data$`Condition Code` %in% input$condition_code, ]
    }
    if (!is.null(input$body_site_code)) {
      data <- data[data$`Body Site Code` %in% input$body_site_code, ]
    }
    return(data)
  })

  specimen_data <- reactive({
    data <- dataset[["Specimen"]]
    if (!is.null(input$specimen_status)) {
      data <- data[data$`Specimen Status` %in% input$specimen_status, ]
    }
    if (!is.null(input$specimen_type_code)) {
      data <- data[data$`Specimen Type Code` %in% input$specimen_type_code, ]
    }
    if (!is.null(input$collection_body_code)) {
      data <- data[data$`Body Site Code` %in% input$collection_body_code, ]
    }
    return(data)
  })

  docref_data <- reactive({
    data <- dataset[["DocumentReference"]]
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
    return(data)
  })

  ## Create Filters
  observe({
    updateCheckboxGroupInput(session,
                             "group_identifier",
                             selected = NULL,
                             choices = sort(unique(dataset[["Patient"]]$Group)))
  })
  observe({
    updateSelectInput(session,
                             "race",
                             selected = NULL,
                             choices = sort(unique(dataset[["Patient"]]$Race)))
  })
  observe({
    updateCheckboxGroupInput(session,
                             "ethnicity",
                             selected = NULL,
                             choices = sort(unique(dataset[["Patient"]]$Ethnicity)))
  })
  observe({
    updateCheckboxGroupInput(session,
                             "gender",
                             selected = NULL,
                             choices = sort(unique(dataset[["Patient"]]$Gender)))
  })
  observe({
    updateCheckboxGroupInput(session,
                             "clinical_status",
                             selected = NULL,
                             choices = sort(unique(dataset[["Condition"]]$`Clinical Status`)))
  })
  observe({
    updateCheckboxGroupInput(session,
                             "verification_status",
                             selected = NULL,
                             choices = sort(unique(dataset[["Condition"]]$`Verification Status`)))
  })
  observe({
    updatePickerInput(session,
                      "condition_code",
                      selected = NULL,
                      choices = sort(unique(dataset[["Condition"]]$`Condition Code`)))
  })
  observe({
    updateCheckboxGroupInput(session,
                             "body_site_code",
                             selected = NULL,
                             choices = sort(unique(dataset[["Condition"]]$`Body Site Count`)))
  })
  observe({
    updateCheckboxGroupInput(session,
                             "specimen_status",
                             selected = NULL,
                             choices = sort(unique(dataset[["Specimen"]]$`Specimen Status`)))
  })
  observe({
    updatePickerInput(session,
                      "specimen_type_code",
                      selected = NULL,
                      choices = sort(unique(dataset[["Specimen"]]$`Specimen Type Code`)))
  })
  observe({
    updateCheckboxGroupInput(session,
                             "collection_body_code",
                             selected = NULL,
                             choices = sort(unique(dataset[["Specimen"]]$`Body Site Code`)))
  })
  observe({
    updateCheckboxGroupInput(session,
                             "docref_status",
                             selected = NULL,
                             choices = sort(unique(dataset[["DocumentReference"]]$`DocumentReference Status`)))
  })
  observe({
    updateCheckboxGroupInput(session,
                             "doc_status",
                             selected = NULL,
                             choices = sort(unique(dataset[["DocumentReference"]]$`Document Status`)))
  })
  observe({
    updatePickerInput(session,
                      "doc_type",
                      selected = NULL,
                      choices = sort(unique(dataset[["DocumentReference"]]$`Document Type`)))
  })
  observe({
    updateCheckboxGroupInput(session,
                             "experiment_strategy",
                             selected = NULL,
                             choices = sort(unique(dataset[["DocumentReference"]]$`Experiment Strategy`)))
  })
  observe({
    updateCheckboxGroupInput(session,
                             "data_category",
                             selected = NULL,
                             choices = sort(unique(dataset[["DocumentReference"]]$`Data Category`)))
  })

  output$download <- downloadHandler(
    file_name <- function() {
      paste(input$table, "tsv", sep = ".")
    },
    content <- function(file) {
      write.table(data_input(), file, sep = "\t", row.names = FALSE)
    }
  )

  output$patient_table <- DT::renderDataTable({
    patient_data()
  }, options = list(pageLength = 25))

  output$condition_table <- DT::renderDataTable({
    condition_data()
  }, options = list(pageLength = 25))

  output$specimen_table <- DT::renderDataTable({
    specimen_data()
  }, options = list(pageLength = 25))

  output$docref_table <- DT::renderDataTable({
    docref_data()
  }, options = list(pageLength = 25))
}

# Bind UI and server to an application
shinyApp(ui = ui, server = server)
