#' teal module: data overview
#' @param id module id
#' @return teal module
#' @export
#' @family teal-modules
mod_data_overview <- function(id) {
  teal::module(
    label = "Data Overview",
    server = function(id, data) {
      shiny::moduleServer(id, function(input, output, session) {
        output$info <- shiny::renderTable({
          d <- data()[["ADPPK"]]
          if (is.null(d)) return(data.frame(info = "ADPPK not available"))
          data.frame(
            metric = c("records", "subjects", "periods", "variables"),
            value = c(
              nrow(d),
              if ("USUBJID" %in% names(d)) length(unique(d$USUBJID)) else NA,
              if ("APERIOD" %in% names(d)) length(unique(stats::na.omit(d$APERIOD))) else NA,
              ncol(d)
            ),
            stringsAsFactors = FALSE
          )
        })
      })
    },
    ui = function(id) {
      ns <- shiny::NS(id)
      teal.widgets::standard_layout(
        output = shiny::tableOutput(ns("info")),
        encoding = shiny::div(
          shiny::p("Use the downstream modules for check execution and visualization.")
        )
      )
    }
  )
}
