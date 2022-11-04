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
      return(data)
    })

    condition_data <- reactive({
      data <- dataset[["Condition"]]
      return(data)
    })

    specimen_data <- reactive({
      data <- dataset[["Specimen"]]
      return(data)
    })

    docref_data <- reactive({
      data <- dataset[["DocumentReference"]]
      return(data)
    })

    ## Create Filters
    output$patient_filters <- renderUI({
    })

    output$condition_filters <- renderUI({
    })

    output$specimen_filters <- renderUI({
    })

    output$docref_filters <- renderUI({
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
