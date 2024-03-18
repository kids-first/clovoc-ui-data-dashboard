
andCompositionUI <- function(id, dataset) {
  ns <- shiny::NS(id)
  fluidPage(
    wellPanel(
      style = "background:slategray4",
      br(),
      filterUI(ns("filter0001"), dataset),
      actionButton(ns("andButton"), "AND", icon = icon("plus"))
    )
  )
}

andCompositionServer <- function(id, dataset) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    results <- reactiveValues()
    results[["filter0001"]] <-
      filterServer("filter0001", dataset)
    observeEvent(input$andButton, {
      i <- sprintf('%04d', input$andButton + 1)
      filter_id <- sprintf('filter%s', i)
      insertUI(
        selector = paste0("#", ns("andButton")),
        where = "beforeBegin",
        ui = filterUI(ns(filter_id), dataset)
      )
      results[[filter_id]] <-
        filterServer(filter_id, dataset)
    })

    # Produce final result
    reactive({ Reduce(intersect, results()) })
  })
}


orCompositionUI <- function(id, dataset) {
  ns <- shiny::NS(id)
  fluidPage(
    wellPanel(
      style = "background:slategray2",
      br(),
      andCompositionUI(ns("composedAnds0001"), dataset),
      actionButton(ns("orButton"), "OR", icon = icon("plus"))
    )
  )
}

orCompositionServer <- function(id, dataset) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    results <- reactiveValues()
    results[["composedAnds0001"]] <-
      andCompositionServer("composedAnds0001", dataset)
    observeEvent(input$orButton, {
      i <- sprintf('%04d', input$orButton + 1)
      composition_id <- sprintf('composedAnds%s', i)
      insertUI(
        selector = paste0("#", ns("orButton")),
        where = "beforeBegin",
        ui = andCompositionUI(ns(composition_id), dataset)
      )
      results[[composition_id]] <-
        andCompositionServer(composition_id, dataset)
    })

    # Produce final result
    reactive({ Reduce(intersect, results) })
  })
}
