#' Run the Shiny app
#'
#' @export
run_app <- function() {
  ui <- shiny::page_fluid(
    bslib::bs_theme(version = 5),
    shiny::titlePanel("rshinypkg demo"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::sliderInput("n", "Number of points", min = 10, max = 200, value = 50)
      ),
      shiny::mainPanel(
        shiny::plotOutput("p")
      )
    )
  )

  server <- function(input, output, session) {
    output$p <- shiny::renderPlot({
      x <- rnorm(input$n)
      y <- rnorm(input$n)
      plot(x, y, pch = 19, col = "#2C7FB8", main = "Demo scatter")
    })
  }

  shiny::shinyApp(ui = ui, server = server)
}
