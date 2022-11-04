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
               icon = icon("user"),
               uiOutput("patient_filters")),
      menuItem("Conditions",
               tabName = "condition_tab",
               icon = icon("tags"),
               uiOutput("condition_filters")),
      menuItem("Specimens",
               tabName = "specimen_tab",
               # icon = icon("search"),
               uiOutput("specimen_filters")),
      menuItem("Document References",
               tabName = "docref_tab",
               icon = icon("file"),
               uiOutput("docref_filters"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "patient_tab",
              mainPanel(DT::dataTableOutput("patient_table"), width = 10)),
      tabItem(tabName = "condition_tab",
              mainPanel(DT::dataTableOutput("condition_table"), width = 10)),
      tabItem(tabName = "specimen_tab",
              mainPanel(DT::dataTableOutput("specimen_table"), width = 10)),
      tabItem(tabName = "docref_tab",
              mainPanel(DT::dataTableOutput("docref_table"), width = 10))
    )
  )
)

# server component
server <- function(input, output) {
    board <- pins::board_rsconnect()
    dataset <- pins::pin_read(board, "nemarichc/clovoc-data-cookie")

    patient_data <- reactive({
      data <- dataset[["Patient"]]
      # if (!is.null(input$research_study_identifier)) {
      #     data <- data[
      #         data$"ResearchStudy Identifier" %in% input$research_study_identifier,
      #     ]
      # }
      if (!is.null(input$group_identifier)) {
        data <- data[
          data$"Group Identifier" %in% input$group_identifier,
        ]
      }
      if (!is.null(input$race)) {
        data <- data[data$"Race" %in% input$race, ]
      }
      if (!is.null(input$ethnicity)) {
        data <- data[data$"Ethnicity" %in% input$ethnicity, ]
      }
      if (!is.null(input$gender)) {
        data <- data[data$"Gender" %in% input$gender, ]
      }
      return(data)
    })

    condition_data <- reactive({
      data <- dataset[["Condition"]]
      if (!is.null(input$clinical_status)) {
        data <- data[
          data$"Clinical Status" %in% input$clinical_status,
        ]
      }
      if (!is.null(input$verification_status)) {
        data <- data[
          data$"Verification Status" %in% input$verification_status,
        ]
      }
      if (!is.null(input$condition_code)) {
        data <- data[
          data$"Condition Code" %in% input$condition_code,
        ]
      }
      if (!is.null(input$body_site_code)) {
        data <- data[
          data$"Body Site Code" %in% input$body_site_code,
        ]
      }
      return(data)
    })

    specimen_data <- reactive({
      data <- dataset[["Specimen"]]
      if (!is.null(input$specimen_status)) {
        data <- data[
          data$"Specimen Status" %in% input$specimen_status,
        ]
      }
      if (!is.null(input$specimen_type_code)) {
        data <- data[
          data$" Specimen Type Code" %in% input$specimen_type_code,
        ]
      }
      if (!is.null(input$collection_body_code)) {
        data <- data[
          data$`Body Site Code` %in% input$collection_body_code,
        ]
      }
      return(data)
    })

    docref_data <- reactive({
      data <- dataset[["DocumentReference"]]
      if (!is.null(input$docref_status)) {
        data <- data[
          data$"DocumentReference Status" %in% input$docref_status,
        ]
      }
      if (!is.null(input$doc_status)) {
        data <- data[
          data$"Document Status" %in% input$doc_status,
        ]
      }
      if (!is.null(input$doc_type)) {
        data <- data[
          data$"Document Type" %in% input$doc_type,
        ]
      }
      if (!is.null(input$experiment_strategy)) {
        data <- data[
          data$"Experiment Strategy" %in% input$experiment_strategy,
        ]
      }
      if (!is.null(input$data_category)) {
        data <- data[
          data$"Data Category" %in% input$data_category,
        ]
      }
      return(data)
    })

    ## Create Filters
    output$patient_filters <- renderUI({
      tagList(
        textInput("research_study_identifier",
                  "Search a Research Study Identifier"),
        # checkboxGroupInput("group_identifier",
        #                    "Group Identifier",
        #                    choices = sort(unique(patient_data()$Group))),
        checkboxGroupInput("race",
                           "Race",
                           choices = sort(unique(patient_data()$Race))),
        checkboxGroupInput("ethnicity",
                           "Ethnicity",
                           choices = sort(unique(patient_data()$Ethnicity))),
        checkboxGroupInput("gender",
                           "Gender",
                           choices = sort(unique(patient_data()$Gender)))
      )
    })

    output$condition_filters <- renderUI({
      tagList(
        checkboxGroupInput("clinical_status",
                           "Clinical Status",
                           choices = sort(unique(condition_data()$`Clinical Status`))),
        checkboxGroupInput("verification_status",
                           "Verification Status",
                           choices = sort(unique(condition_data()$`Verification Status`))),
        textInput("condition_name",
                  "Search a Condition Name",),
        pickerInput("condition_code",
                    "Condition Code",
                    choices = sort(unique(condition_data()$`Condition Code`))),
        textInput("body_site_name",
                  "Search a Body Name"),
        checkboxGroupInput("body_site_code",
                           "Body Site Code",
                           choices = sort(unique(condition_data()$`Body Site Code`)))
      )
    })

    output$specimen_filters <- renderUI({
      tagList(
        checkboxGroupInput("specimen_status",
                           "Specimen Status",
                           choices = sort(unique(specimen_data()$`Specimen Status`))),
        textInput("specimen_type_name",
                  "Search a Specimen Type Name"),
        checkboxGroupInput("specimen_type_code",
                           "Specimen Type Code",
                           choices = sort(unique(specimen_data()$`Specimen Type Code`))),
        textInput("collection_body_type",
                  "Search a Body Site Name"),
        checkboxGroupInput("collection_body_code",
                           "Body Site Code",
                           choices = sort(unique(specimen_data()$`Body Site Code`)))
      )
    })

    output$docref_filters <- renderUI({
      tagList(
        checkboxGroupInput("docref_status",
                           "DocumentReference Status",
                           choices = sort(unique(docref_data()$`DocumentReference Status`))),
        checkboxGroupInput("doc_status",
                           "Document Status",
                           choices = sort(unique(docref_data()$`Document Status`))),
        checkboxGroupInput("doc_type",
                           "Document Type",
                           choices = sort(unique(docref_data()$`Document Type`))),
        checkboxGroupInput("experiment_strategy",
                           "Experiment Strategy",
                           choices = sort(unique(docref_data()$`Experiment Strategy`))),
        checkboxGroupInput("data_category",
                           "Data Category",
                           choices = sort(unique(docref_data()$`Data Category`)))
      )
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
