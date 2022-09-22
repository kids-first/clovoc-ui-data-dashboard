source("global.R", local = TRUE)
source("common.R", local = TRUE)

# Install or load dependencies
required_packages <- c(
    "shiny",
    "shinyWidgets",
    "DT"
)
LoadRequiredPackages(required_packages)


# UI component
ui <- navbarPage(
    "CLOVoc Data Dashboard",
    tabPanel(
        "Explore Data",
        sidebarLayout(
            sidebarPanel(
                uiOutput("select"),
                downloadButton("download", "Download as TSV"),
                width = 2
            ),
            mainPanel(DT::dataTableOutput("records"), width = 10)
        )
    )
)

# server component
server <- function(input, output) {
    output$select <- renderUI({
        selectInput(
            "table",
            "Select a Table:",
            c("Patient", "Condition", "Specimen", "DocumentReference"),
            selected = "Patient"
        )
    })

    data_input <- reactive({
        data <- dataset[[input$table]]
        return(data)
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

    output$records <- DT::renderDataTable({
        data_input()
    }, options = list(pageLength = 25))
}

# Bind UI and server to an application
shinyApp(ui = ui, server = server)
