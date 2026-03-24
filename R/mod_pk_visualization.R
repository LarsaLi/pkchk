#' teal module: PK visualization
#' @param id module id
#' @return teal module
#' @export
#' @family teal-modules
mod_pk_visualization <- function(id) {
  teal::module(
    label = "PK Visualization",
    server = function(id, data) {
      shiny::moduleServer(id, function(input, output, session) {
        adppk <- shiny::reactive(data()[["ADPPK"]])

        obs_data <- shiny::reactive({
          d <- adppk()
          shiny::req(d)
          if (!all(c("TIME", "AVAL", "USUBJID") %in% names(d))) return(NULL)
          if ("EVID" %in% names(d)) d <- d[d$EVID == 0, , drop = FALSE]
          d
        })

        output$pk_spaghetti <- shiny::renderPlot({
          d <- obs_data()
          shiny::req(d)
          if (requireNamespace("ggplot2", quietly = TRUE)) {
            p <- ggplot2::ggplot(d, ggplot2::aes_string(x = "TIME", y = "AVAL", group = "USUBJID")) +
              ggplot2::geom_line(alpha = 0.25, color = "#93c5fd") +
              ggplot2::geom_point(alpha = 0.45, size = 1.1, color = "#1d4ed8") +
              ggplot2::theme_minimal() +
              ggplot2::labs(x = "TIME", y = "AVAL", title = "Concentration-Time Spaghetti")
            if (isTRUE(input$log_y)) p <- p + ggplot2::scale_y_log10()
            print(p)
          } else {
            graphics::plot(as.numeric(d$TIME), as.numeric(d$AVAL), pch = 19, col = "#1d4ed8")
          }
        })

        output$time_dev_plot <- shiny::renderPlot({
          d <- adppk()
          shiny::req(d)
          shiny::req(all(c("TIME", "NTIME") %in% names(d)))
          dev <- as.numeric(d$TIME) - as.numeric(d$NTIME)
          if (requireNamespace("ggplot2", quietly = TRUE)) {
            p <- ggplot2::ggplot(data.frame(dev = dev), ggplot2::aes(x = dev)) +
              ggplot2::geom_histogram(bins = 30, fill = "#86efac", color = "#14532d") +
              ggplot2::theme_minimal() +
              ggplot2::labs(x = "TIME - NTIME", y = "Count", title = "Sampling Time Deviation")
            print(p)
          } else {
            graphics::hist(dev, main = "Sampling Time Deviation", xlab = "TIME - NTIME", col = "#86efac")
          }
        })
      })
    },
    ui = function(id) {
      ns <- shiny::NS(id)
      teal.widgets::standard_layout(
        output = shiny::div(
          shiny::plotOutput(ns("pk_spaghetti"), height = "300px"),
          shiny::plotOutput(ns("time_dev_plot"), height = "260px")
        ),
        encoding = shiny::div(
          shiny::checkboxInput(ns("log_y"), "Log Y axis", value = FALSE)
        )
      )
    }
  )
}
