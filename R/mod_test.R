mod_test_ui = function(id) {
  ns = NS(id)
  tagList(
    numericInput(ns("bins"), "bins", value = 10, min = 1),
    textOutput(outputId = ns("test"))
  )
}

mod_test_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$test <- renderText({
        input$bins
    })
  })
}
