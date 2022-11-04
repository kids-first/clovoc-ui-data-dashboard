library(shiny)
library(shinyWidgets)
library(shinydashboard)

# UI component
ui <- dashboardPage(
  dashboardHeader(title = span(tagList(icon("fire"), "CLOVoc Data Dashboard"))),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Patients",
               tabName = "patient_tab",
               icon = icon("user")),
      menuItem("Conditions",
               tabName = "condition_tab",
               icon = icon("tags")),
      menuItem("Specimens",
               tabName = "specimen_tab",
               icon = icon("vial")),
      menuItem("Document References",
               tabName = "docref_tab",
               icon = icon("file-medical"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "patient_tab",
              fluidRow(h2("Patient Data")),
              fluidRow(
                box(title = "Box1", width = 3,
                    textInput("research_study_identifier",
                              "Search a Research Study Identifier")),
                # checkboxGroupInput("group_identifier",
                #                    "Group Identifier",
                #                    choices = ""),
                box(title = "Box 2", width = 3,
                    checkboxGroupInput("race",
                                       "Race",
                                       choices = "")),
                box(title = "Box 3", width = 3,
                    checkboxGroupInput("ethnicity",
                                       "Ethnicity",
                                       choices = "")),
                box(title = "Box 4", width = 3,
                    checkboxGroupInput("gender",
                                       "Gender",
                                       choices = ""))
              ),
              fluidRow(DT::dataTableOutput("patient_table"))),
      tabItem(tabName = "condition_tab",
              fluidRow(h2("Condition Data Tab")),
              fluidRow(
                box(title = "Box1", width = 4,
                    checkboxGroupInput("clinical_status",
                                   "Clinical Status",
                                   choices = "")),
                box(title = "Box2", width = 4,
                    checkboxGroupInput("verification_status",
                                   "Verification Status",
                                   choices = "")),
                box(title = "Box3", width = 4,
                    textInput("condition_name",
                          "Search a Condition Name"))),
              fluidRow(
                box(title = "Box4", width = 4,
                    pickerInput("condition_code",
                            "Condition Code",
                            choices = "")),
                box(title = "Box5", width = 4,
                    textInput("body_site_name",
                          "Search a Body Name")),
                box(title = "Box6", width = 4,
                    checkboxGroupInput("body_site_code",
                                   "Body Site Code",
                                   choices = ""))),
              fluidRow(DT::dataTableOutput("condition_table"))),
      tabItem(tabName = "specimen_tab",
              fluidRow(h2("Specimen Data Tab")),
              fluidRow(
                box(title = "Box1", width = 4,
                    checkboxGroupInput("specimen_status",
                                   "Specimen Status",
                                   choices = "")),
                box(title = "Box2", width = 4,
                    textInput("specimen_type_name",
                          "Search a Specimen Type Name")),
                box(title = "Box3", width = 4,
                    checkboxGroupInput("specimen_type_code",
                                   "Specimen Type Code",
                                   choices = ""))),
              fluidRow(
                box(title = "Box4", width = 4,
                    textInput("collection_body_type",
                          "Search a Body Site Name")),
                box(title = "Box5", width = 4,
                    checkboxGroupInput("collection_body_code",
                                   "Body Site Code",
                                   choices = ""))),
              fluidRow(DT::dataTableOutput("specimen_table"))),
      tabItem(tabName = "docref_tab",
              fluidRow(h2("Document Reference Tab")),
              fluidRow(
                box(title = "Box1", width = 4,
                    checkboxGroupInput("docref_status",
                                   "DocumentReference Status",
                                   choices = "")),
                box(title = "Box2", width = 4,
                    checkboxGroupInput("doc_status",
                                   "Document Status",
                                   choices = "")),
                box(title = "Box3", width = 4,
                    checkboxGroupInput("doc_type",
                                   "Document Type",
                                   choices = ""))),
              fluidRow(
                box(title = "Box4", width = 4,
                    checkboxGroupInput("experiment_strategy",
                                   "Experiment Strategy",
                                   choices = "")),
                box(title = "Box5", width = 4,
                    checkboxGroupInput("data_category",
                                   "Data Category",
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
    #         data$"ResearchStudy Identifier" %in% input$research_study_identifier,
    #     ]
    # }
    # if (!is.null(input$group_identifier)) {
    #   data <- data[
    #     data$"Group Identifier" %in% input$group_identifier,
    #   ]
    # }
    # if (!is.null(input$race)) {
    #   data <- data[data$"Race" %in% input$race, ]
    # }
    # if (!is.null(input$ethnicity)) {
    #   data <- data[data$"Ethnicity" %in% input$ethnicity, ]
    # }
    # if (!is.null(input$gender)) {
    #   data <- data[data$"Gender" %in% input$gender, ]
    # }
    return(data)
  })

  condition_data <- reactive({
    data <- dataset[["Condition"]]
    # if (!is.null(input$clinical_status)) {
    #   data <- data[
    #     data$"Clinical Status" %in% input$clinical_status,
    #   ]
    # }
    # if (!is.null(input$verification_status)) {
    #   data <- data[
    #     data$"Verification Status" %in% input$verification_status,
    #   ]
    # }
    # if (!is.null(input$condition_code)) {
    #   data <- data[
    #     data$"Condition Code" %in% input$condition_code,
    #   ]
    # }
    # if (!is.null(input$body_site_code)) {
    #   data <- data[
    #     data$"Body Site Code" %in% input$body_site_code,
    #   ]
    # }
    return(data)
  })

  specimen_data <- reactive({
    data <- dataset[["Specimen"]]
    # if (!is.null(input$specimen_status)) {
    #   data <- data[
    #     data$"Specimen Status" %in% input$specimen_status,
    #   ]
    # }
    # if (!is.null(input$specimen_type_code)) {
    #   data <- data[
    #     data$" Specimen Type Code" %in% input$specimen_type_code,
    #   ]
    # }
    # if (!is.null(input$collection_body_code)) {
    #   data <- data[
    #     data$`Body Site Code` %in% input$collection_body_code,
    #   ]
    # }
    return(data)
  })

  docref_data <- reactive({
    data <- dataset[["DocumentReference"]]
    # if (!is.null(input$docref_status)) {
    #   data <- data[
    #     data$"DocumentReference Status" %in% input$docref_status,
    #   ]
    # }
    # if (!is.null(input$doc_status)) {
    #   data <- data[
    #     data$"Document Status" %in% input$doc_status,
    #   ]
    # }
    # if (!is.null(input$doc_type)) {
    #   data <- data[
    #     data$"Document Type" %in% input$doc_type,
    #   ]
    # }
    # if (!is.null(input$experiment_strategy)) {
    #   data <- data[
    #     data$"Experiment Strategy" %in% input$experiment_strategy,
    #   ]
    # }
    # if (!is.null(input$data_category)) {
    #   data <- data[
    #     data$"Data Category" %in% input$data_category,
    #   ]
    # }
    return(data)
  })

  ## Create Filters
  observe({
    updateCheckboxGroupInput(session,
                             "group_identifier",
                             selected = "",
                             choices = sort(unique(patient_data()$Group)))
  })
  observe({
    updateCheckboxGroupInput(session,
                             "race",
                             selected = "",
                             choices = sort(unique(patient_data()$Race)))
  })
  observe({
    updateCheckboxGroupInput(session,
                             "ethnicity",
                             selected = "",
                             choices = sort(unique(patient_data()$Ethnicity)))
  })
  observe({
    updateCheckboxGroupInput(session,
                             "gender",
                             selected = "",
                             choices = sort(unique(patient_data()$Gender)))
  })
  observe({
    updateCheckboxGroupInput(session,
                             "clinical_status",
                             selected = "",
                             choices = sort(unique(condition_data()$`Clinical Status`)))
  })
  observe({
    updateCheckboxGroupInput(session,
                             "verification_status",
                             selected = "",
                             choices = sort(unique(condition_data()$`Verification Status`)))
  })
  observe({
    updatePickerInput(session,
                      "condition_code",
                      selected = "",
                      choices = sort(unique(condition_data()$`Condition Code`)))
  })
  observe({
    updateCheckboxGroupInput(session,
                             "body_site_code",
                             selected = "",
                             choices = sort(unique(condition_data()$`Body Site Count`)))
  })
  observe({
    updateCheckboxGroupInput(session,
                             "specimen_status",
                             selected = "",
                             choices = sort(unique(specimen_data()$`Specimen Status`)))
  })
  observe({
    updateCheckboxGroupInput(session,
                             "specimen_type_code",
                             selected = "",
                             choices = sort(unique(specimen_data()$`Specimen Type Code`)))
  })
  observe({
    updateCheckboxGroupInput(session,
                             "collection_body_code",
                             selected = "",
                             choices = sort(unique(specimen_data()$`Body Site Code`)))
  })
  observe({
    updateCheckboxGroupInput(session,
                             "docref_status",
                             selected = "",
                             choices = sort(unique(docref_data()$`DocumentReference Status`)))
  })
  observe({
    updateCheckboxGroupInput(session,
                             "doc_status",
                             selected = "",
                             choices = sort(unique(docref_data()$`Document Status`)))
  })
  observe({
    updateCheckboxGroupInput(session,
                             "doc_type",
                             selected = "",
                             choices = sort(unique(docref_data()$`Document Type`)))
  })
  observe({
    updateCheckboxGroupInput(session,
                             "experiment_strategy",
                             selected = "",
                             choices = sort(unique(docref_data()$`Experiment Strategy`)))
  })
  observe({
    updateCheckboxGroupInput(session,
                             "data_category",
                             selected = "",
                             choices = sort(unique(docref_data()$`Data Category`)))
  })

  output$download <- downloadHandler(
    file_name <- function() {
      paste(input$table, "tsv", sep = ".")
    },
    content <- function(file) {
      write.table(
        data_input(), file, sep = "\t", row.names = FALSE
      )
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
