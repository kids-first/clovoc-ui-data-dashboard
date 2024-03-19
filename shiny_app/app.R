# TEDDY VERSION OF THE SHINY APP
# Load packages=================================================================
library(shiny)
library(shinydashboard)

# Load modules==================================================================
source("and_composition_module.R")
source("or_composition_module.R")
source("filter_module.R")

# Retrieve pinned data==========================================================
board <- pins::board_connect()
dataset <- pins::pin_read(board, "nemarichc/clovoc-teddy-dataset")

ui <- dashboardPage(
  ## Dashboard Header===========================================================
  dashboardHeader(title = span(tagList(icon("fire"), "CLOVoc Data Dashboard"))),
  ## Dashboard Sidebar==========================================================
  dashboardSidebar(
    sidebarMenu(
      menuItem("Cohort Generation", tabName = "cohort_tab",
               icon = icon("file-medical")),
      menuItem("Download", tabName = "download_tab",
               icon = icon("download"))
    )
  ),
  ## Dashboard Body=============================================================
  dashboardBody(
    tabItems(
      ### Combined Data tab====
      tabItem(tabName = "cohort_tab",
              fluidPage(
                br(),
                actionButton("addCohortButton",
                             "Add Cohort",
                             icon = icon("plus")),
                actionButton("removeCohortButton",
                             "Remove Cohort",
                             icon = icon("minus")),
                br(),
                br(),
                tabsetPanel(id = "cohortTabs",
                            tabPanel("Cohort #01",
                                     orCompositionUI("cohort_filter01",
                                                     dataset)))
              )),
      tabItem(tabName = "download_tab",
              fluidRow(h2("Download Cohort Data")),
              downloadButton("downloadData", "Download Dataset"))
    )
  )
)


# Server component==============================================================
server <- function(input, output, session) {

  cohort_data <- reactiveValues()

  cohort_data[["cohort_filter01"]] <-
    orCompositionServer("cohort_filter01", dataset)

  observeEvent(input$addCohortButton, {
    i <- sprintf('%02d', input$addCohortButton + 1)
    cohort_id <- sprintf('cohort%s', i)
    cohort_filter_id <- sprintf('cohort_filter%s', i)
    tab_title <- sprintf('Cohort #%s', i)
    previous_tab <- sprintf('%02d', input$addCohortButton)
    tab_placement <- sprintf('Cohort #%s', previous_tab)
    insertTab(inputId = "cohortTabs",
              tab = tabPanel(tab_title,
                             orCompositionUI(cohort_filter_id, dataset)),
              target = tab_placement,
              position = "after")
    cohort_data[[cohort_filter_id]] <-
      orCompositionServer(cohort_filter_id, dataset)
  })

  output$downloadData <- downloadHandler(
    filename = function() {
      "cohort_data.csv"
    },
    content = function(fname) {
      print("Generating download...")
      # Loop on combos of cohorts and tables to generate CSVs
      outer(names(cohort_data),
            names(dataset[["Manifest"]]),
            function(x, y) {
              print(paste0("Writing CSV for ", x, " and ", y, "..."))
              write.csv({
                dataset[[y]]["Patient ID" %in% cohort_data[[x]], ]
              },
              file = paste0(tempdir(), "/", x, "_", y, ".csv"))
            })
      # Zip all into a single download
      print("Zipping files...")
      zip(tempdir(), files = outer(names(cohort_data),
                                   names(dataset[["Manifest"]]),
                                   function(x, y) paste0(x, "_", y, ".csv")))
      },
    contentType = "application/zip")
}


# Bind UI and server to an application==========================================
shinyApp(ui = ui, server = server)
