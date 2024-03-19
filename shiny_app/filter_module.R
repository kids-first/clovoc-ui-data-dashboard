
filterUI <- function(id, dataset) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("ui"))
}

filterServer <- function(id, dataset) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    manifest <- dataset[["Manifest"]]

    `%!in%` <- function(x, y) !(`%in%`(x, y))

    cat_operators <- c("is" = "==",
                       "is not" = "!=") #,
                       # "in" = "%in%",
                       # "not in" = "'!%in%'")

    num_operators <- c("≥" = ">=",
                       ">" = ">",
                       "=" = "==",
                       "≠" = "!=",
                       "<" = "<",
                       "≤" = "<=")

    field_options <- reactive({
      req(input$category)
      names(manifest[[input$category]])
    })

    observe({
      req(input$category)
      updateSelectInput(session = session,
                        inputId = "field",
                        choices = field_options(),
                        selected = NULL)
    })

    field_type <- reactiveVal()
    unit_col <- reactiveVal()
    group_col <- reactiveVal()

    observe({
      req(input$category, input$field)
      field_type({ manifest[[input$category]][[input$field]][["type"]] })
      unit_col({ manifest[[input$category]][[input$field]][["unit"]] })
      group_col({ manifest[[input$category]][[input$field]][["grouping"]] })
    })

    group_options <- reactiveVal()

    observe({
      req(input$category, input$field)
      if (!is.null(group_col()) &&
          (group_col() %in% names(dataset[[input$category]]))) {
        group_options({ unique(dataset[[input$category]][[group_col()]]) })
      } else {
        group_options(NULL)
      }
    })

    observe({
      req(input$field)
      if(length(group_options()) > 0) {
        updateSelectInput(session = session,
                          inputId = "grouping",
                          choices = group_options(),
                          selected = NULL)
      }
    })

    categorical_choices <- reactive({
      unique(dataset[[input$category]][[input$field]])
    })

    observe({
      req(field_type())
      if(field_type() == "Categorical") {
        updateSelectInput(session = session,
                          inputId = "categorical",
                          choices = categorical_choices(),
                          selected = NULL)
      }
    })

    unit1 <- reactiveVal()
    unit2 <- reactiveVal()

    observe({
      req(input$field, input$grouping)
      df <- dataset[[input$category]]
      if (!is.null(unit_col()) && (unit_col() %in% names(df)) &&
          !is.null(group_col()) && (group_col() %in% names(df))) {
        my_unit <- df %>%
          dplyr::filter(.data[[group_col()]] == input$grouping) %>%
          dplyr::select(unit_col()) %>%
          unique() %>%
          unlist()
        unit1(my_unit)
        unit2(my_unit)
      } else {
        unit1(NULL)
        unit2(NULL)
      }
    })

    output$unit1 <- renderText({ unit1() })
    output$unit2 <- renderText({ unit2() })

    output$ui <- renderUI({
      fluidPage(
        fluidRow(
          column(
            wellPanel(
              fluidRow(
                column(selectInput(ns("category"), "Category:",
                                   choices = names(manifest)), width=6),
                column(selectInput(ns("field"), "Field:",
                                   choices = NULL), width=6))),
            width=5),
          column(
            wellPanel(
              fluidRow(uiOutput(ns("uiGrouping")))),
              width=7)
        ))
    })

    output$uiGrouping <- renderUI({
      req(input$field)
      if (length(group_options()) > 0) {
          fluidRow(
            column(selectInput(ns("grouping"), "Type:", choices = NULL),
                   width=2),
            column(uiOutput(ns("uiFilterTypes")), width=10)
          )
      } else {
        uiOutput(ns("uiFilterTypes"))
      }
    })

    output$uiFilterTypes <- renderUI({
      req(field_type())
      if (field_type() == "Categorical") {
          fluidRow(
            column(selectInput(ns("operator_cat"), "Operator:",
                               choices = cat_operators),
                   width=6),
            column(selectInput(ns("categorical"), "Category:",
                               choices = NULL),
                   width=6)
          )
      } else if (field_type() == "Numerical" && is.null(unit_col())) {
          fluidRow(
            column(selectInput(ns("operator_num_1"), "Operator:",
                               choices = num_operators,
                               width = "8%"),
                   width=3),
            column(numericInput(ns("numerical_1"), "Value #1:",
                                value = 0),
                   width=3),
            column(selectInput(ns("logical"), "",
                               choices = c("AND" = "&", "OR" = "|")),
                   width=2),
            column(selectInput(ns("operator_num_2"), "Operator:",
                               choices = num_operators),
                   width=3),
            column(numericInput(ns("numerical_2"), "Value #2:",
                                value = 0),
                   width=3)
          )
      } else if (field_type() == "Numerical" && !is.null(unit_col())) {
          fluidRow(
            column(selectInput(ns("operator_num_1"), "Operator:",
                               choices = num_operators),
                   width = 2),
            column(numericInput(ns("numerical_1"), "Value #1:",
                                value = 0),
                   width = 2),
            column(textOutput(ns("unit1")),
                   width = 1),
            column(selectInput(ns("logical"), "",
                               choices = c("AND" = "&", "OR" = "|")),
                   width = 2),
            column(selectInput(ns("operator_num_2"), "Operator:",
                               choices = num_operators),
                   width = 2),
            column(numericInput(ns("numerical_2"), "Value #2:",
                                value = 0),
                   width = 2),
            column(textOutput(ns("unit2")),
                   width = 1)
          )
      }
    })

    # Produce results list of patient IDs
    results <- reactiveVal()

    observe({
      req(input$category, input$field, field_type())
      if (field_type() == "Categorical") {
        req(input$operator_cat, input$categorical)
        results({
          dataset[[input$category]] %>%
            dplyr::rowwise() %>%
            dplyr::filter({
              do.call(input$operator_cat, list(.data[[input$field]],
                                               input$categorical))
            }) %>%
            dplyr::ungroup() %>%
            dplyr::select("Patient ID") %>%
            unique()
        })
      } else if (field_type() == "Numerical"){
        req(input$logical,
            input$operator_num_1,
            input$numerical_1,
            input$operator_num_2,
            input$numerical_2)
        results({
          dataset[[input$category]] %>%
            dplyr::rowwise() %>%
            dplyr::filter({
              do.call(input$logical,
                      list(
                        do.call(input$operator_num_1,
                                list(.data[[input$field]],
                                     input$numerical_1)),
                        do.call(input$operator_num_2,
                                list(.data[[input$field]],
                                     input$numerical_2))
                      ))
            }) %>%
            dplyr::ungroup() %>%
            dplyr::select("Patient ID") %>%
            unique()
        })
      }
    })

    # Return results
    results
  })
}
