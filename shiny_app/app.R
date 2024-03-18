# Load packages=================================================================
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(shinydashboard)
library(sortable)

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
      menuItem("Cohort Generation", tabName = "cohort_tab",
               icon = icon("file-medical"))
    )
  ),
  ## Dashboard Body=============================================================
  dashboardBody(
    tabItems(
      ### Combined Data tab====
      tabItem(tabName = "cohort_tab",
              fluidRow(h2("Cohort Creation")),
              fluidRow(h3("Step 1: Select fields to utilize for filtering and cohort building")),
              radioButtons("toggle_pt_gend",
                           label = "Patient: Gender",
                           choices = c("Filter", "Sort", "Both", "Neither"),
                           inline = TRUE),
              radioButtons("toggle_pt_race",
                           label = "Patient: Race",
                           choices = c("Filter", "Sort", "Both", "Neither"),
                           inline = TRUE),
              radioButtons("toggle_pt_ethn",
                           label = "Patient: Ethnicity",
                           choices = c("Filter", "Sort", "Both", "Neither"),
                           inline = TRUE),
              radioButtons("toggle_cn_name",
                           label = "Condition: Name",
                           choices = c("Filter", "Sort", "Both", "Neither"),
                           inline = TRUE),
              radioButtons("toggle_cn_site",
                           label = "Condition: Site",
                           choices = c("Filter", "Sort", "Both", "Neither"),
                           inline = TRUE),
              radioButtons("toggle_sp_name",
                           label = "Specimen: Name",
                           choices = c("Filter", "Sort", "Both", "Neither"),
                           inline = TRUE),
              radioButtons("toggle_sp_site",
                           label = "Specimen: Site",
                           choices = c("Filter", "Sort", "Both", "Neither"),
                           inline = TRUE),
              radioButtons("toggle_dr_doctyp",
                           label = "DocumentReference: Document Type",
                           choices = c("Filter", "Sort", "Both", "Neither"),
                           inline = TRUE),
              radioButtons("toggle_dr_expstr",
                           label = "DocumentReference: Experiment Strategy",
                           choices = c("Filter", "Sort", "Both", "Neither"),
                           inline = TRUE),
              radioButtons("toggle_dr_datcat",
                           label = "DocumentReference: Data Category",
                           choices = c("Filter", "Sort", "Both", "Neither"),
                           inline = TRUE),
              fluidRow(h3("Step 2: Filter dataset to be used for cohort creation.")),
              h4("Patient Filters:"),
              fluidRow(
                column(4,
                       pickerInput("filter_pt_race",
                                   label = "Race",
                                   choices = NULL,
                                   multiple = TRUE,
                                   options = picker_defaults)),
                column(4,
                       pickerInput("filter_pt_ethn",
                                   label = "Ethnicity",
                                   choices = NULL,
                                   multiple = TRUE,
                                   options = picker_defaults)),
                column(4,
                       pickerInput("filter_pt_gend",
                                   label = "Gender",
                                   choices = NULL,
                                   multiple = TRUE,
                                   options = picker_defaults))),
              h4("Condition Filters:"),
              fluidRow(
                column(4,
                       pickerInput("filter_cn_name",
                                   label = "Name",
                                   choices = NULL,
                                   multiple = TRUE,
                                   options = picker_defaults)),
                column(4,
                       pickerInput("filter_cn_site",
                                   label = "Body Site",
                                   choices = NULL,
                                   multiple = TRUE,
                                   options = picker_defaults))),
              h4("Specimen Filters:"),
              fluidRow(
                column(4,
                       pickerInput("filter_sp_name",
                                   label = "Name",
                                   choices = NULL,
                                   multiple = TRUE,
                                   options = picker_defaults)),
                column(4,
                       pickerInput("filter_sp_site",
                                   label = "Body Site",
                                   choices = NULL,
                                   multiple = TRUE,
                                   options = picker_defaults))),
              h4("Document Reference Filters:"),
              fluidRow(
                column(4,
                       pickerInput("filter_dr_doctyp",
                                   label = "Document Type",
                                   choices = NULL,
                                   multiple = TRUE,
                                   options = picker_defaults)),
                column(4,
                       pickerInput("filter_dr_expstr",
                                   label = "Experiment Strategy",
                                   choices = NULL,
                                   multiple = TRUE,
                                   options = picker_defaults)),
                column(4,
                       pickerInput("filter_dr_datcat",
                                   label = "Data Category",
                                   choices = NULL,
                                   multiple = TRUE,
                                   options = picker_defaults))),
              fluidRow(h3("Step 3: Create virtual cohorts using selected categories and thresholds.")),
              uiOutput("sort_pt_gend"),
              uiOutput("sort_pt_race"),
              uiOutput("sort_pt_ethn"),
              uiOutput("sort_cn_name"),
              uiOutput("sort_cn_site"),
              uiOutput("sort_sp_name"),
              uiOutput("sort_sp_site"),
              uiOutput("sort_dr_doctyp"),
              uiOutput("sort_dr_expstr"),
              uiOutput("sort_dr_datcat"),
              fluidRow(h3("Step 4: Review and download cohort data.")),
              actionButton("download", "Download Dataset"))
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
    for (n in 1:NROW(crossfilter_list)) {
      filter_values <- cross_filters[[crossfilter_list[[n, 2]]]][["Patient Identifier"]]
      if (!is.null(filter_values)) {
        data <- data[data[["Patient Identifier"]] %in% filter_values, ]
      }
    }
    return(data)
  }

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
    input$race
    input$ethnicity
    input$gender
    data <- dataset[["Patient"]] |>
      dplyr::select("ResearchStudy Identifier",
                    "Patient Identifier",
                    "Race",
                    "Ethnicity",
                    "Gender") |>
      apply_tab_filters(tibble::tribble(
        ~column_name,               ~filter_name,
        "ResearchStudy Identifier", "research_study_identifier",
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
    input$condition_name
    input$condition_body_site_name
    data <- dataset[["Condition"]] |>
      dplyr::select("Patient Identifier",
                    "Condition" = "Condition Name",
                    "Body Site" = "Body Site Name",
                    "Verification Status",
                    "Clinical Status") |>
      apply_tab_filters(tibble::tribble(
        ~column_name,             ~filter_name,
        "Patient Identifier",     "condition_patient_id",
        "Condition",              "condition_name",
        "Body Site",              "condition_body_site_name",
        "Verification Status",    "verification_status",
        "Clinical Status",        "clinical_status"
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
    input$specimen_type_name
    input$collection_body_name
    data <- dataset[["Specimen"]]|>
      dplyr::select("Patient Identifier",
                    "Specimen Identifier",
                    "Specimen Type" = "Specimen Type Name",
                    "Body Site" = "Body Site Name") |>
      apply_tab_filters(tibble::tribble(
        ~column_name,                 ~filter_name,
        "Patient Identifier",         "specimen_patient_id",
        "Specimen Identifier",        "specimen_identifier",
        "Specimen Type",              "specimen_type_name",
        "Body Site",                  "collection_body_name"
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
    input$doc_type
    input$experiment_strategy
    input$data_category
    data <- dataset[["DocumentReference"]] |>
      dplyr::select("Patient Identifier",
                    "Data Type" = "Document Type",
                    "Analysis" = "Experiment Strategy",
                    "Data Category",
                    "URL") |>
      apply_tab_filters(tibble::tribble(
        ~column_name,               ~filter_name,
        "Data Type",                "doc_type",
        "Analysis",                 "experiment_strategy",
        "Data Category",            "data_category"
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
                      "filter_pt_race",
                      selected = NULL,
                      choices = sort(unique(dataset[["Patient"]]$Race)))
  })
  observe({
    updatePickerInput(session,
                      "filter_pt_ethn",
                      selected = NULL,
                      choices = sort(unique(
                        dataset[["Patient"]]$Ethnicity)))
  })
  observe({
    updatePickerInput(session,
                      "filter_pt_gend",
                      selected = NULL,
                      choices = sort(unique(
                        dataset[["Patient"]]$Gender)))
  })
  observe({
    updatePickerInput(session,
                      "filter_cn_name",
                      selected = NULL,
                      choices = sort(unique(
                        dataset[["Condition"]]$`Condition`)))
  })
  observe({
    updatePickerInput(session,
                      "filter_cn_site",
                      selected = NULL,
                      choices = sort(unique(
                        dataset[["Condition"]]$`Body Site`)))
  })
  observe({
    updatePickerInput(session,
                      "filter_sp_name",
                      selected = NULL,
                      choices = sort(unique(
                        dataset[["Specimen"]]$`Body Site`)))
  })
  observe({
    updatePickerInput(session,
                      "filter_sp_site",
                      selected = NULL,
                      choices = sort(unique(
                        dataset[["Specimen"]]$`Specimen Type`)))
  })
  observe({
    updatePickerInput(session,
                      "filter_dr_doctyp",
                      selected = NULL,
                      choices = sort(unique(
                        dataset[["DocumentReference"]]$`Data Type`)))
  })
  observe({
    updatePickerInput(
      session,
      "filter_dr_expstr",
      selected = NULL,
      choices = sort(unique(
        dataset[["DocumentReference"]]$`Analysis`)))
  })
  observe({
    updatePickerInput(session,
                      "filter_dr_datcat",
                      selected = NULL,
                      choices = sort(unique(
                        dataset[["DocumentReference"]]$`Data Category`)))
  })

  ## Create sortable buckets====================================================
  pt_gend_cohorts <- reactiveValues(available = unique(dataset[["Patient"]]$Gender),
                                   available_filtered = unique(dataset[["Patient"]]$Gender),
                                   cohort_a = NULL,
                                   cohort_a_filtered = NULL,
                                   cohort_b = NULL,
                                   cohort_b_filtered = NULL)
  pt_race_cohorts <- reactiveValues(available = unique(dataset[["Patient"]]$Race),
                                    available_filtered = unique(dataset[["Patient"]]$Race),
                                    cohort_a = NULL,
                                    cohort_a_filtered = NULL,
                                    cohort_b = NULL,
                                    cohort_b_filtered = NULL)
  pt_ethn_cohorts <- reactiveValues(available = unique(dataset[["Patient"]]$Ethnicity),
                                    available_filtered = unique(dataset[["Patient"]]$Ethnicity),
                                    cohort_a = NULL,
                                    cohort_a_filtered = NULL,
                                    cohort_b = NULL,
                                    cohort_b_filtered = NULL)
  cn_name_cohorts <- reactiveValues(available = unique(dataset[["Condition"]]$`Condition Name`),
                                    available_filtered = unique(dataset[["Condition"]]$`Condition Name`),
                                    cohort_a = NULL,
                                    cohort_a_filtered = NULL,
                                    cohort_b = NULL,
                                    cohort_b_filtered = NULL)
  cn_site_cohorts <- reactiveValues(available = unique(dataset[["Condition"]]$`Body Site Name`),
                                    available_filtered = unique(dataset[["Condition"]]$`Body Site Name`),
                                    cohort_a = NULL,
                                    cohort_a_filtered = NULL,
                                    cohort_b = NULL,
                                    cohort_b_filtered = NULL)
  sp_name_cohorts <- reactiveValues(available = unique(dataset[["Specimen"]]$`Specimen Type Name`),
                                    available_filtered = unique(dataset[["Specimen"]]$`Specimen Type Name`),
                                    cohort_a = NULL,
                                    cohort_a_filtered = NULL,
                                    cohort_b = NULL,
                                    cohort_b_filtered = NULL)
  sp_site_cohorts <- reactiveValues(available = unique(dataset[["Specimen"]]$`Body Site Name`),
                                    available_filtered = unique(dataset[["Specimen"]]$`Body Site Name`),
                                    cohort_a = NULL,
                                    cohort_a_filtered = NULL,
                                    cohort_b = NULL,
                                    cohort_b_filtered = NULL)
  dr_doctyp_cohorts <- reactiveValues(available = unique(dataset[["DocumentReference"]]$`Document Type`),
                                      available_filtered = unique(dataset[["DocumentReference"]]$`Document Type`),
                                      cohort_a = NULL,
                                      cohort_a_filtered = NULL,
                                      cohort_b = NULL,
                                      cohort_b_filtered = NULL)
  dr_expstr_cohorts <- reactiveValues(available = unique(dataset[["DocumentReference"]]$`Experiment Strategy`),
                                      available_filtered = unique(dataset[["DocumentReference"]]$`Experiment Strategy`),
                                      cohort_a = NULL,
                                      cohort_a_filtered = NULL,
                                      cohort_b = NULL,
                                      cohort_b_filtered = NULL)
  dr_datcat_cohorts <- reactiveValues(available = unique(dataset[["DocumentReference"]]$`Data Category`),
                                      available_filtered = unique(dataset[["DocumentReference"]]$`Data Category`),
                                      cohort_a = NULL,
                                      cohort_a_filtered = NULL,
                                      cohort_b = NULL,
                                      cohort_b_filtered = NULL)

  output$sort_pt_gend <- renderUI({
    sortable::bucket_list(
      header = "Patient: Gender",
      sortable::add_rank_list(text = "Select from here",
                              labels = pt_gend_cohorts$available_filtered,
                              options = sortable::sortable_options(
                                onSort = sortable::sortable_js_capture_input("filtered_gend_available")
                              )),
      sortable::add_rank_list(text = "Cohort A",
                              labels = pt_gend_cohorts$cohort_a_filtered,
                              options = sortable::sortable_options(
                                onSort = sortable::sortable_js_capture_input("filtered_gend_cohort_a")
                              )),
      sortable::add_rank_list(text = "Cohort B",
                              labels = pt_gend_cohorts$cohort_b_filtered,
                              options = sortable::sortable_options(
                                onSort = sortable::sortable_js_capture_input("filtered_gend_cohort_b")
                              ))
    )
  })

  observeEvent(input$gender, {
    if (is.null(input$gender)) {
      pt_gend_cohorts$available_filtered <- union(pt_gend_cohorts$available,
                                                 input$filtered_gend_available)
      pt_gend_cohorts$cohort_a_filtered <- union(pt_gend_cohorts$cohort_a,
                                                input$filtered_gend_cohort_a)
      pt_gend_cohorts$cohort_b_filtered <- union(pt_gend_cohorts$cohort_b,
                                                input$filtered_gend_cohort_b)

    } else {
      pt_gend_cohorts$available_filtered <- union(
        intersect(pt_gend_cohorts$available, input$gender),
        intersect(input$filtered_gend_available, input$gender))
      pt_gend_cohorts$cohort_a_filtered <- union(
        intersect(pt_gend_cohorts$cohort_a, input$gender),
        intersect(input$filtered_gend_cohort_a, input$gender))
      pt_gend_cohorts$cohort_b_filtered <- union(
        intersect(pt_gend_cohorts$cohort_b, input$gender),
        intersect(input$filtered_gend_cohort_b, input$gender))
    }
  })

  observeEvent(input$filtered_gend_available, {
    pt_gend_cohorts$available <-
      setdiff(union(pt_gend_cohorts$available, input$filtered_gend_available),
              union(input$filtered_gend_cohort_a, input$filtered_gend_cohort_b))
    pt_gend_cohorts$available_filtered <- input$filtered_gend_available
  })

  observeEvent(input$filtered_gend_cohort_a, {
    pt_gend_cohorts$cohort_a <-
      setdiff(union(pt_gend_cohorts$cohort_a, input$filtered_gend_cohort_a),
              union(input$filtered_gend_available, input$filtered_gend_cohort_b))
    pt_gend_cohorts$cohort_a_filtered <- input$filtered_gend_cohort_a
  })

  observeEvent(input$filtered_gend_cohort_b, {
    pt_gend_cohorts$cohort_b <-
      setdiff(union(pt_gend_cohorts$cohort_b, input$filtered_gend_cohort_b),
            union(input$filtered_gend_available, input$filtered_gend_cohort_a))
    pt_gend_cohorts$cohort_b_filtered <- input$filtered_gend_cohort_b
  })

  output$sort_pt_race <- renderUI({
    sortable::bucket_list(
      header = "Patient: Race",
      sortable::add_rank_list(text = "Select from here",
                              labels = pt_race_cohorts$available_filtered,
                              options = sortable::sortable_options(
                                onSort = sortable::sortable_js_capture_input("filtered_race_available")
                              )),
      sortable::add_rank_list(text = "Cohort A",
                              labels = pt_race_cohorts$cohort_a_filtered,
                              options = sortable::sortable_options(
                                onSort = sortable::sortable_js_capture_input("filtered_race_cohort_a")
                              )),
      sortable::add_rank_list(text = "Cohort B",
                              labels = pt_race_cohorts$cohort_b_filtered,
                              options = sortable::sortable_options(
                                onSort = sortable::sortable_js_capture_input("filtered_race_cohort_b")
                              ))
    )
  })

  observeEvent(input$race, {
    if (is.null(input$race)) {
      pt_race_cohorts$available_filtered <- union(pt_race_cohorts$available,
                                                 input$filtered_race_available)
      pt_race_cohorts$cohort_a_filtered <- union(pt_race_cohorts$cohort_a,
                                                input$filtered_race_cohort_a)
      pt_race_cohorts$cohort_b_filtered <- union(pt_race_cohorts$cohort_b,
                                                input$filtered_race_cohort_b)

    } else {
      pt_race_cohorts$available_filtered <- union(
        intersect(pt_race_cohorts$available, input$race),
        intersect(input$filtered_race_available, input$race))
      pt_race_cohorts$cohort_a_filtered <- union(
        intersect(pt_race_cohorts$cohort_a, input$race),
        intersect(input$filtered_race_cohort_a, input$race))
      pt_race_cohorts$cohort_b_filtered <- union(
        intersect(pt_race_cohorts$cohort_b, input$race),
        intersect(input$filtered_race_cohort_b, input$race))
    }
  })

  observeEvent(input$filtered_race_available, {
    pt_race_cohorts$available <-
      setdiff(union(pt_race_cohorts$available, input$filtered_race_available),
              union(input$filtered_race_cohort_a, input$filtered_race_cohort_b))
    pt_race_cohorts$available_filtered <- input$filtered_race_available
  })

  observeEvent(input$filtered_race_cohort_a, {
    pt_race_cohorts$cohort_a <-
      setdiff(union(pt_race_cohorts$cohort_a, input$filtered_race_cohort_a),
              union(input$filtered_race_available, input$filtered_race_cohort_b))
    pt_race_cohorts$cohort_a_filtered <- input$filtered_race_cohort_a
  })

  observeEvent(input$filtered_race_cohort_b, {
    pt_race_cohorts$cohort_b <-
      setdiff(union(pt_race_cohorts$cohort_b, input$filtered_race_cohort_b),
              union(input$filtered_race_available, input$filtered_race_cohort_a))
    pt_race_cohorts$cohort_b_filtered <- input$filtered_race_cohort_b
  })

  output$sort_pt_ethn <- renderUI({
    sortable::bucket_list(
      header = "Patient: Ethnicity",
      sortable::add_rank_list(text = "Select from here",
                              labels = pt_ethn_cohorts$available_filtered,
                              options = sortable::sortable_options(
                                onSort = sortable::sortable_js_capture_input("filtered_ethn_available")
                              )),
      sortable::add_rank_list(text = "Cohort A",
                              labels = pt_ethn_cohorts$cohort_a_filtered,
                              options = sortable::sortable_options(
                                onSort = sortable::sortable_js_capture_input("filtered_ethn_cohort_a")
                              )),
      sortable::add_rank_list(text = "Cohort B",
                              labels = pt_ethn_cohorts$cohort_b_filtered,
                              options = sortable::sortable_options(
                                onSort = sortable::sortable_js_capture_input("filtered_ethn_cohort_b")
                              ))
    )
  })

  observeEvent(input$ethnicity, {
    if (is.null(input$ethnicity)) {
      pt_ethn_cohorts$available_filtered <- union(pt_ethn_cohorts$available,
                                                 input$filtered_ethn_available)
      pt_ethn_cohorts$cohort_a_filtered <- union(pt_ethn_cohorts$cohort_a,
                                                input$filtered_ethn_cohort_a)
      pt_ethn_cohorts$cohort_b_filtered <- union(pt_ethn_cohorts$cohort_b,
                                                input$filtered_ethn_cohort_b)

    } else {
      pt_ethn_cohorts$available_filtered <- union(
        intersect(pt_ethn_cohorts$available, input$ethnicity),
        intersect(input$filtered_ethn_available, input$ethnicity))
      pt_ethn_cohorts$cohort_a_filtered <- union(
        intersect(pt_ethn_cohorts$cohort_a, input$ethnicity),
        intersect(input$filtered_ethn_cohort_a, input$ethnicity))
      pt_ethn_cohorts$cohort_b_filtered <- union(
        intersect(pt_ethn_cohorts$cohort_b, input$ethnicity),
        intersect(input$filtered_ethn_cohort_b, input$ethnicity))
    }
  })

  observeEvent(input$filtered_ethn_available, {
    pt_ethn_cohorts$available <-
      setdiff(union(pt_ethn_cohorts$available, input$filtered_ethn_available),
              union(input$filtered_ethn_cohort_a, input$filtered_ethn_cohort_b))
    pt_ethn_cohorts$available_filtered <- input$filtered_ethn_available
  })

  observeEvent(input$filtered_ethn_cohort_a, {
    pt_ethn_cohorts$cohort_a <-
      setdiff(union(pt_ethn_cohorts$cohort_a, input$filtered_ethn_cohort_a),
              union(input$filtered_ethn_available, input$filtered_ethn_cohort_b))
    pt_ethn_cohorts$cohort_a_filtered <- input$filtered_ethn_cohort_a
  })

  observeEvent(input$filtered_ethn_cohort_b, {
    pt_ethn_cohorts$cohort_b <-
      setdiff(union(pt_ethn_cohorts$cohort_b, input$filtered_ethn_cohort_b),
              union(input$filtered_ethn_available, input$filtered_ethn_cohort_a))
    pt_ethn_cohorts$cohort_b_filtered <- input$filtered_ethn_cohort_b
  })

  output$sort_cn_name <- renderUI({
    sortable::bucket_list(
      header = "Condition: Name",
      sortable::add_rank_list(text = "Select from here",
                              labels = cn_name_cohorts$available_filtered,
                              options = sortable::sortable_options(
                                onSort = sortable::sortable_js_capture_input("filtered_cn_name_available")
                              )),
      sortable::add_rank_list(text = "Cohort A",
                              labels = cn_name_cohorts$cohort_a_filtered,
                              options = sortable::sortable_options(
                                onSort = sortable::sortable_js_capture_input("filtered_cn_name_cohort_a")
                              )),
      sortable::add_rank_list(text = "Cohort B",
                              labels = cn_name_cohorts$cohort_b_filtered,
                              options = sortable::sortable_options(
                                onSort = sortable::sortable_js_capture_input("filtered_cn_name_cohort_b")
                              ))
    )
  })

  observeEvent(input$condition_name, {
    if (is.null(input$condition_name)) {
      cn_name_cohorts$available_filtered <- union(cn_name_cohorts$available,
                                                 input$filtered_cn_name_available)
      cn_name_cohorts$cohort_a_filtered <- union(cn_name_cohorts$cohort_a,
                                                input$filtered_cn_name_cohort_a)
      cn_name_cohorts$cohort_b_filtered <- union(cn_name_cohorts$cohort_b,
                                                input$filtered_cn_name_cohort_b)

    } else {
      cn_name_cohorts$available_filtered <- union(
        intersect(cn_name_cohorts$available, input$condition_name),
        intersect(input$filtered_cn_name_available, input$condition_name))
      cn_name_cohorts$cohort_a_filtered <- union(
        intersect(cn_name_cohorts$cohort_a, input$condition_name),
        intersect(input$filtered_cn_name_cohort_a, input$condition_name))
      cn_name_cohorts$cohort_b_filtered <- union(
        intersect(cn_name_cohorts$cohort_b, input$condition_name),
        intersect(input$filtered_cn_name_cohort_b, input$condition_name))
    }
  })

  observeEvent(input$filtered_cn_name_available, {
    cn_name_cohorts$available <-
      setdiff(union(cn_name_cohorts$available, input$filtered_cn_name_available),
              union(input$filtered_cn_name_cohort_a, input$filtered_cn_name_cohort_b))
    cn_name_cohorts$available_filtered <- input$filtered_cn_name_available
  })

  observeEvent(input$filtered_cn_name_cohort_a, {
    cn_name_cohorts$cohort_a <-
      setdiff(union(cn_name_cohorts$cohort_a, input$filtered_cn_name_cohort_a),
              union(input$filtered_cn_name_available, input$filtered_cn_name_cohort_b))
    cn_name_cohorts$cohort_a_filtered <- input$filtered_cn_name_cohort_a
  })

  observeEvent(input$filtered_cn_name_cohort_b, {
    cn_name_cohorts$cohort_b <-
      setdiff(union(cn_name_cohorts$cohort_b, input$filtered_cn_name_cohort_b),
              union(input$filtered_cn_name_available, input$filtered_cn_name_cohort_a))
    cn_name_cohorts$cohort_b_filtered <- input$filtered_cn_name_cohort_b
  })

  output$sort_cn_site <- renderUI({
    sortable::bucket_list(
      header = "Condition: Site",
      sortable::add_rank_list(text = "Select from here",
                              labels = cn_site_cohorts$available_filtered,
                              options = sortable::sortable_options(
                                onSort = sortable::sortable_js_capture_input("filtered_cn_site_available")
                              )),
      sortable::add_rank_list(text = "Cohort A",
                              labels = cn_site_cohorts$cohort_a_filtered,
                              options = sortable::sortable_options(
                                onSort = sortable::sortable_js_capture_input("filtered_cn_site_cohort_a")
                              )),
      sortable::add_rank_list(text = "Cohort B",
                              labels = cn_site_cohorts$cohort_b_filtered,
                              options = sortable::sortable_options(
                                onSort = sortable::sortable_js_capture_input("filtered_cn_site_cohort_b")
                              ))
    )
  })

  observeEvent(input$condition_body_site_name, {
    if (is.null(input$condition_body_site_name)) {
      cn_site_cohorts$available_filtered <- union(cn_site_cohorts$available,
                                                  input$filtered_cn_site_available)
      cn_site_cohorts$cohort_a_filtered <- union(cn_site_cohorts$cohort_a,
                                                 input$filtered_cn_site_cohort_a)
      cn_site_cohorts$cohort_b_filtered <- union(cn_site_cohorts$cohort_b,
                                                 input$filtered_cn_site_cohort_b)

    } else {
      cn_site_cohorts$available_filtered <- union(
        intersect(cn_site_cohorts$available, input$condition_body_site_name),
        intersect(input$filtered_cn_site_available, input$condition_body_site_name))
      cn_site_cohorts$cohort_a_filtered <- union(
        intersect(cn_site_cohorts$cohort_a, input$condition_body_site_name),
        intersect(input$filtered_cn_site_cohort_a, input$condition_body_site_name))
      cn_site_cohorts$cohort_b_filtered <- union(
        intersect(cn_site_cohorts$cohort_b, input$condition_body_site_name),
        intersect(input$filtered_cn_site_cohort_b, input$condition_body_site_name))
    }
  })

  observeEvent(input$filtered_cn_site_available, {
    cn_site_cohorts$available <-
      setdiff(union(cn_site_cohorts$available, input$filtered_cn_site_available),
              union(input$filtered_cn_site_cohort_a, input$filtered_cn_site_cohort_b))
    cn_site_cohorts$available_filtered <- input$filtered_cn_site_available
  })

  observeEvent(input$filtered_cn_site_cohort_a, {
    cn_site_cohorts$cohort_a <-
      setdiff(union(cn_site_cohorts$cohort_a, input$filtered_cn_site_cohort_a),
              union(input$filtered_cn_site_available, input$filtered_cn_site_cohort_b))
    cn_site_cohorts$cohort_a_filtered <- input$filtered_cn_site_cohort_a
  })

  observeEvent(input$filtered_cn_site_cohort_b, {
    cn_site_cohorts$cohort_b <-
      setdiff(union(cn_site_cohorts$cohort_b, input$filtered_cn_site_cohort_b),
              union(input$filtered_cn_site_available, input$filtered_cn_site_cohort_a))
    cn_site_cohorts$cohort_b_filtered <- input$filtered_cn_site_cohort_b
  })

  output$sort_sp_name <- renderUI({
    sortable::bucket_list(
      header = "Specimen: Name",
      sortable::add_rank_list(text = "Select from here",
                              labels = sp_name_cohorts$available_filtered,
                              options = sortable::sortable_options(
                                onSort = sortable::sortable_js_capture_input("filtered_sp_name_available")
                              )),
      sortable::add_rank_list(text = "Cohort A",
                              labels = sp_name_cohorts$cohort_a_filtered,
                              options = sortable::sortable_options(
                                onSort = sortable::sortable_js_capture_input("filtered_sp_name_cohort_a")
                              )),
      sortable::add_rank_list(text = "Cohort B",
                              labels = sp_name_cohorts$cohort_b_filtered,
                              options = sortable::sortable_options(
                                onSort = sortable::sortable_js_capture_input("filtered_sp_name_cohort_b")
                              ))
    )
  })

  observeEvent(input$specimen_type_name, {
    if (is.null(input$specimen_type_name)) {
      sp_name_cohorts$available_filtered <- union(sp_name_cohorts$available,
                                                  input$filtered_sp_name_available)
      sp_name_cohorts$cohort_a_filtered <- union(sp_name_cohorts$cohort_a,
                                                 input$filtered_sp_name_cohort_a)
      sp_name_cohorts$cohort_b_filtered <- union(sp_name_cohorts$cohort_b,
                                                 input$filtered_sp_name_cohort_b)

    } else {
      sp_name_cohorts$available_filtered <- union(
        intersect(sp_name_cohorts$available, input$specimen_type_name),
        intersect(input$filtered_sp_name_available, input$specimen_type_name))
      sp_name_cohorts$cohort_a_filtered <- union(
        intersect(sp_name_cohorts$cohort_a, input$specimen_type_name),
        intersect(input$filtered_sp_name_cohort_a, input$specimen_type_name))
      sp_name_cohorts$cohort_b_filtered <- union(
        intersect(sp_name_cohorts$cohort_b, input$specimen_type_name),
        intersect(input$filtered_sp_name_cohort_b, input$specimen_type_name))
    }
  })

  observeEvent(input$filtered_sp_name_available, {
    sp_name_cohorts$available <-
      setdiff(union(sp_name_cohorts$available, input$filtered_sp_name_available),
              union(input$filtered_sp_name_cohort_a, input$filtered_sp_name_cohort_b))
    sp_name_cohorts$available_filtered <- input$filtered_sp_name_available
  })

  observeEvent(input$filtered_sp_name_cohort_a, {
    sp_name_cohorts$cohort_a <-
      setdiff(union(sp_name_cohorts$cohort_a, input$filtered_sp_name_cohort_a),
              union(input$filtered_sp_name_available, input$filtered_sp_name_cohort_b))
    sp_name_cohorts$cohort_a_filtered <- input$filtered_sp_name_cohort_a
  })

  observeEvent(input$filtered_sp_name_cohort_b, {
    sp_name_cohorts$cohort_b <-
      setdiff(union(sp_name_cohorts$cohort_b, input$filtered_sp_name_cohort_b),
              union(input$filtered_sp_name_available, input$filtered_sp_name_cohort_a))
    sp_name_cohorts$cohort_b_filtered <- input$filtered_sp_name_cohort_b
  })

  output$sort_sp_site <- renderUI({
    sortable::bucket_list(
      header = "Specimen: Site",
      sortable::add_rank_list(text = "Select from here",
                              labels = sp_site_cohorts$available_filtered,
                              options = sortable::sortable_options(
                                onSort = sortable::sortable_js_capture_input("filtered_sp_site_available")
                              )),
      sortable::add_rank_list(text = "Cohort A",
                              labels = sp_site_cohorts$cohort_a_filtered,
                              options = sortable::sortable_options(
                                onSort = sortable::sortable_js_capture_input("filtered_sp_site_cohort_a")
                              )),
      sortable::add_rank_list(text = "Cohort B",
                              labels = sp_site_cohorts$cohort_b_filtered,
                              options = sortable::sortable_options(
                                onSort = sortable::sortable_js_capture_input("filtered_sp_site_cohort_b")
                              ))
    )
  })

  observeEvent(input$collection_body_type, {
    if (is.null(input$collection_body_type)) {
      sp_site_cohorts$available_filtered <- union(sp_site_cohorts$available,
                                                  input$filtered_sp_site_available)
      sp_site_cohorts$cohort_a_filtered <- union(sp_site_cohorts$cohort_a,
                                                 input$filtered_sp_site_cohort_a)
      sp_site_cohorts$cohort_b_filtered <- union(sp_site_cohorts$cohort_b,
                                                 input$filtered_sp_site_cohort_b)

    } else {
      sp_site_cohorts$available_filtered <- union(
        intersect(sp_site_cohorts$available, input$collection_body_type),
        intersect(input$filtered_sp_site_available, input$collection_body_type))
      sp_site_cohorts$cohort_a_filtered <- union(
        intersect(sp_site_cohorts$cohort_a, input$collection_body_type),
        intersect(input$filtered_sp_site_cohort_a, input$collection_body_type))
      sp_site_cohorts$cohort_b_filtered <- union(
        intersect(sp_site_cohorts$cohort_b, input$collection_body_type),
        intersect(input$filtered_sp_site_cohort_b, input$collection_body_type))
    }
  })

  observeEvent(input$filtered_sp_site_available, {
    sp_site_cohorts$available <-
      setdiff(union(sp_site_cohorts$available, input$filtered_sp_site_available),
              union(input$filtered_sp_site_cohort_a, input$filtered_sp_site_cohort_b))
    sp_site_cohorts$available_filtered <- input$filtered_sp_site_available
  })

  observeEvent(input$filtered_sp_site_cohort_a, {
    sp_site_cohorts$cohort_a <-
      setdiff(union(sp_site_cohorts$cohort_a, input$filtered_sp_site_cohort_a),
              union(input$filtered_sp_site_available, input$filtered_sp_site_cohort_b))
    sp_site_cohorts$cohort_a_filtered <- input$filtered_sp_site_cohort_a
  })

  observeEvent(input$filtered_sp_site_cohort_b, {
    sp_site_cohorts$cohort_b <-
      setdiff(union(sp_site_cohorts$cohort_b, input$filtered_sp_site_cohort_b),
              union(input$filtered_sp_site_available, input$filtered_sp_site_cohort_a))
    sp_site_cohorts$cohort_b_filtered <- input$filtered_sp_site_cohort_b
  })

  output$sort_dr_doctyp <- renderUI({
    sortable::bucket_list(
      header = "Document Reference: Document Type",
      sortable::add_rank_list(text = "Select from here",
                              labels = dr_doctyp_cohorts$available_filtered,
                              options = sortable::sortable_options(
                                onSort = sortable::sortable_js_capture_input("filtered_dr_doctyp_available")
                              )),
      sortable::add_rank_list(text = "Cohort A",
                              labels = dr_doctyp_cohorts$cohort_a_filtered,
                              options = sortable::sortable_options(
                                onSort = sortable::sortable_js_capture_input("filtered_dr_doctyp_cohort_a")
                              )),
      sortable::add_rank_list(text = "Cohort B",
                              labels = dr_doctyp_cohorts$cohort_b_filtered,
                              options = sortable::sortable_options(
                                onSort = sortable::sortable_js_capture_input("filtered_dr_doctyp_cohort_b")
                              ))
    )
  })

  observeEvent(input$doc_type, {
    if (is.null(input$doc_type)) {
      dr_doctyp_cohorts$available_filtered <- union(dr_doctyp_cohorts$available,
                                                  input$filtered_dr_doctyp_available)
      dr_doctyp_cohorts$cohort_a_filtered <- union(dr_doctyp_cohorts$cohort_a,
                                                 input$filtered_dr_doctyp_cohort_a)
      dr_doctyp_cohorts$cohort_b_filtered <- union(dr_doctyp_cohorts$cohort_b,
                                                 input$filtered_dr_doctyp_cohort_b)

    } else {
      dr_doctyp_cohorts$available_filtered <- union(
        intersect(dr_doctyp_cohorts$available, input$doc_type),
        intersect(input$filtered_dr_doctyp_available, input$doc_type))
      dr_doctyp_cohorts$cohort_a_filtered <- union(
        intersect(dr_doctyp_cohorts$cohort_a, input$doc_type),
        intersect(input$filtered_dr_doctyp_cohort_a, input$doc_type))
      dr_doctyp_cohorts$cohort_b_filtered <- union(
        intersect(dr_doctyp_cohorts$cohort_b, input$doc_type),
        intersect(input$filtered_dr_doctyp_cohort_b, input$doc_type))
    }
  })

  observeEvent(input$filtered_dr_doctyp_available, {
    dr_doctyp_cohorts$available <-
      setdiff(union(dr_doctyp_cohorts$available, input$filtered_dr_doctyp_available),
              union(input$filtered_dr_doctyp_cohort_a, input$filtered_dr_doctyp_cohort_b))
    dr_doctyp_cohorts$available_filtered <- input$filtered_dr_doctyp_available
  })

  observeEvent(input$filtered_dr_doctyp_cohort_a, {
    dr_doctyp_cohorts$cohort_a <-
      setdiff(union(dr_doctyp_cohorts$cohort_a, input$filtered_dr_doctyp_cohort_a),
              union(input$filtered_dr_doctyp_available, input$filtered_dr_doctyp_cohort_b))
    dr_doctyp_cohorts$cohort_a_filtered <- input$filtered_dr_doctyp_cohort_a
  })

  observeEvent(input$filtered_dr_doctyp_cohort_b, {
    dr_doctyp_cohorts$cohort_b <-
      setdiff(union(dr_doctyp_cohorts$cohort_b, input$filtered_dr_doctyp_cohort_b),
              union(input$filtered_dr_doctyp_available, input$filtered_dr_doctyp_cohort_a))
    dr_doctyp_cohorts$cohort_b_filtered <- input$filtered_dr_doctyp_cohort_b
  })

  output$sort_dr_expstr <- renderUI({
    sortable::bucket_list(
      header = "Document Reference: Experiment Strategy",
      sortable::add_rank_list(text = "Select from here",
                              labels = dr_expstr_cohorts$available_filtered,
                              options = sortable::sortable_options(
                                onSort = sortable::sortable_js_capture_input("filtered_dr_expstr_available")
                              )),
      sortable::add_rank_list(text = "Cohort A",
                              labels = dr_expstr_cohorts$cohort_a_filtered,
                              options = sortable::sortable_options(
                                onSort = sortable::sortable_js_capture_input("filtered_dr_expstr_cohort_a")
                              )),
      sortable::add_rank_list(text = "Cohort B",
                              labels = dr_expstr_cohorts$cohort_b_filtered,
                              options = sortable::sortable_options(
                                onSort = sortable::sortable_js_capture_input("filtered_dr_expstr_cohort_b")
                              ))
    )
  })

  observeEvent(input$experiment_strategy, {
    if (is.null(input$experiment_strategy)) {
      dr_expstr_cohorts$available_filtered <- union(dr_expstr_cohorts$available,
                                                  input$filtered_dr_expstr_available)
      dr_expstr_cohorts$cohort_a_filtered <- union(dr_expstr_cohorts$cohort_a,
                                                 input$filtered_dr_expstr_cohort_a)
      dr_expstr_cohorts$cohort_b_filtered <- union(dr_expstr_cohorts$cohort_b,
                                                 input$filtered_dr_expstr_cohort_b)

    } else {
      dr_expstr_cohorts$available_filtered <- union(
        intersect(dr_expstr_cohorts$available, input$experiment_strategy),
        intersect(input$filtered_dr_expstr_available, input$experiment_strategy))
      dr_expstr_cohorts$cohort_a_filtered <- union(
        intersect(dr_expstr_cohorts$cohort_a, input$experiment_strategy),
        intersect(input$filtered_dr_expstr_cohort_a, input$experiment_strategy))
      dr_expstr_cohorts$cohort_b_filtered <- union(
        intersect(dr_expstr_cohorts$cohort_b, input$experiment_strategy),
        intersect(input$filtered_dr_expstr_cohort_b, input$experiment_strategy))
    }
  })

  observeEvent(input$filtered_dr_expstr_available, {
    dr_expstr_cohorts$available <-
      setdiff(union(dr_expstr_cohorts$available, input$filtered_dr_expstr_available),
              union(input$filtered_dr_expstr_cohort_a, input$filtered_dr_expstr_cohort_b))
    dr_expstr_cohorts$available_filtered <- input$filtered_dr_expstr_available
  })

  observeEvent(input$filtered_dr_expstr_cohort_a, {
    dr_expstr_cohorts$cohort_a <-
      setdiff(union(dr_expstr_cohorts$cohort_a, input$filtered_dr_expstr_cohort_a),
              union(input$filtered_dr_expstr_available, input$filtered_dr_expstr_cohort_b))
    dr_expstr_cohorts$cohort_a_filtered <- input$filtered_dr_expstr_cohort_a
  })

  observeEvent(input$filtered_dr_expstr_cohort_b, {
    dr_expstr_cohorts$cohort_b <-
      setdiff(union(dr_expstr_cohorts$cohort_b, input$filtered_dr_expstr_cohort_b),
              union(input$filtered_dr_expstr_available, input$filtered_dr_expstr_cohort_a))
    dr_expstr_cohorts$cohort_b_filtered <- input$filtered_dr_expstr_cohort_b
  })

  output$sort_dr_datcat <- renderUI({
    sortable::bucket_list(
      header = "Document Reference: Data Category",
      sortable::add_rank_list(text = "Select from here",
                              labels = dr_datcat_cohorts$available_filtered,
                              options = sortable::sortable_options(
                                onSort = sortable::sortable_js_capture_input("filtered_dr_datcat_available")
                              )),
      sortable::add_rank_list(text = "Cohort A",
                              labels = dr_datcat_cohorts$cohort_a_filtered,
                              options = sortable::sortable_options(
                                onSort = sortable::sortable_js_capture_input("filtered_dr_datcat_cohort_a")
                              )),
      sortable::add_rank_list(text = "Cohort B",
                              labels = dr_datcat_cohorts$cohort_b_filtered,
                              options = sortable::sortable_options(
                                onSort = sortable::sortable_js_capture_input("filtered_dr_datcat_cohort_b")
                              ))
    )
  })

  observeEvent(input$data_category, {
    if (is.null(input$data_category)) {
      dr_datcat_cohorts$available_filtered <- union(dr_datcat_cohorts$available,
                                                  input$filtered_dr_datcat_available)
      dr_datcat_cohorts$cohort_a_filtered <- union(dr_datcat_cohorts$cohort_a,
                                                 input$filtered_dr_datcat_cohort_a)
      dr_datcat_cohorts$cohort_b_filtered <- union(dr_datcat_cohorts$cohort_b,
                                                 input$filtered_dr_datcat_cohort_b)

    } else {
      dr_datcat_cohorts$available_filtered <- union(
        intersect(dr_datcat_cohorts$available, input$data_category),
        intersect(input$filtered_dr_datcat_available, input$data_category))
      dr_datcat_cohorts$cohort_a_filtered <- union(
        intersect(dr_datcat_cohorts$cohort_a, input$data_category),
        intersect(input$filtered_dr_datcat_cohort_a, input$data_category))
      dr_datcat_cohorts$cohort_b_filtered <- union(
        intersect(dr_datcat_cohorts$cohort_b, input$data_category),
        intersect(input$filtered_dr_datcat_cohort_b, input$data_category))
    }
  })

  observeEvent(input$filtered_dr_datcat_available, {
    dr_datcat_cohorts$available <-
      setdiff(union(dr_datcat_cohorts$available, input$filtered_dr_datcat_available),
              union(input$filtered_dr_datcat_cohort_a, input$filtered_dr_datcat_cohort_b))
    dr_datcat_cohorts$available_filtered <- input$filtered_dr_datcat_available
  })

  observeEvent(input$filtered_dr_datcat_cohort_a, {
    dr_datcat_cohorts$cohort_a <-
      setdiff(union(dr_datcat_cohorts$cohort_a, input$filtered_dr_datcat_cohort_a),
              union(input$filtered_dr_datcat_available, input$filtered_dr_datcat_cohort_b))
    dr_datcat_cohorts$cohort_a_filtered <- input$filtered_dr_datcat_cohort_a
  })

  observeEvent(input$filtered_dr_datcat_cohort_b, {
    dr_datcat_cohorts$cohort_b <-
      setdiff(union(dr_datcat_cohorts$cohort_b, input$filtered_dr_datcat_cohort_b),
              union(input$filtered_dr_datcat_available, input$filtered_dr_datcat_cohort_a))
    dr_datcat_cohorts$cohort_b_filtered <- input$filtered_dr_datcat_cohort_b
  })

  ## Show or hide filter and sort options=========================================
  observeEvent(input$toggle_pt_gend, {
    if (input$toggle_pt_gend %in% c("Filter", "Both")) {
      shinyjs::show("filter_pt_gend")
    } else {
      shinyjs::hide("filter_pt_gend")
    }
    if (input$toggle_pt_gend %in% c("Sort", "Both")) {
      shinyjs::show("sort_pt_gend")
    } else {
      shinyjs::hide("sort_pt_gend")
    }
  })
  observeEvent(input$toggle_pt_race, {
    if (input$toggle_pt_race %in% c("Filter", "Both")) {
      shinyjs::show("filter_pt_race")
    } else {
      shinyjs::hide("filter_pt_race")
    }
    if (input$toggle_pt_race %in% c("Sort", "Both")) {
      shinyjs::show("sort_pt_race")
    } else {
      shinyjs::hide("sort_pt_race")
    }
  })
  observeEvent(input$toggle_pt_ethn, {
    if (input$toggle_pt_ethn %in% c("Filter", "Both")) {
      shinyjs::show("filter_pt_ethn")
    } else {
      shinyjs::hide("filter_pt_ethn")
    }
    if (input$toggle_pt_ethn %in% c("Sort", "Both")) {
      shinyjs::show("sort_pt_ethn")
    } else {
      shinyjs::hide("sort_pt_ethn")
    }
  })
  observeEvent(input$toggle_cn_name, {
    if (input$toggle_cn_name %in% c("Filter", "Both")) {
      shinyjs::show("filter_cn_name")
    } else {
      shinyjs::hide("filter_cn_name")
    }
    if (input$toggle_cn_name %in% c("Sort", "Both")) {
      shinyjs::show("sort_cn_name")
    } else {
      shinyjs::hide("sort_cn_name")
    }
  })
  observeEvent(input$toggle_cn_site, {
    if (input$toggle_cn_site %in% c("Filter", "Both")) {
      shinyjs::show("filter_cn_site")
    } else {
      shinyjs::hide("filter_cn_site")
    }
    if (input$toggle_cn_site %in% c("Sort", "Both")) {
      shinyjs::show("sort_cn_site")
    } else {
      shinyjs::hide("sort_cn_site")
    }
  })
  observeEvent(input$toggle_sp_name, {
    if (input$toggle_sp_name %in% c("Filter", "Both")) {
      shinyjs::show("filter_sp_name")
    } else {
      shinyjs::hide("filter_sp_name")
    }
    if (input$toggle_sp_name %in% c("Sort", "Both")) {
      shinyjs::show("sort_sp_name")
    } else {
      shinyjs::hide("sort_sp_name")
    }
  })
  observeEvent(input$toggle_sp_site, {
    if (input$toggle_sp_site %in% c("Filter", "Both")) {
      shinyjs::show("filter_sp_site")
    } else {
      shinyjs::hide("filter_sp_site")
    }
    if (input$toggle_sp_site %in% c("Sort", "Both")) {
      shinyjs::show("sort_sp_site")
    } else {
      shinyjs::hide("sort_sp_site")
    }
  })
  observeEvent(input$toggle_dr_doctyp, {
    if (input$toggle_dr_doctyp %in% c("Filter", "Both")) {
      shinyjs::show("filter_dr_doctyp")
    } else {
      shinyjs::hide("filter_dr_doctyp")
    }
    if (input$toggle_dr_doctyp %in% c("Sort", "Both")) {
      shinyjs::show("sort_dr_doctyp")
    } else {
      shinyjs::hide("sort_dr_doctyp")
    }
  })
  observeEvent(input$toggle_dr_expstr, {
    if (input$toggle_dr_expstr %in% c("Filter", "Both")) {
      shinyjs::show("filter_dr_expstr")
    } else {
      shinyjs::hide("filter_dr_expstr")
    }
    if (input$toggle_dr_expstr %in% c("Sort", "Both")) {
      shinyjs::show("sort_dr_expstr")
    } else {
      shinyjs::hide("sort_dr_expstr")
    }
  })
  observeEvent(input$toggle_dr_datcat, {
    if (input$toggle_dr_datcat %in% c("Filter", "Both")) {
      shinyjs::show("filter_dr_datcat")
    } else {
      shinyjs::hide("filter_dr_datcat")
    }
    if (input$toggle_dr_datcat %in% c("Sort", "Both")) {
      shinyjs::show("sort_dr_datcat")
    } else {
      shinyjs::hide("sort_dr_datcat")
    }
  })

  }



# Bind UI and server to an application==========================================
shinyApp(ui = ui, server = server)
