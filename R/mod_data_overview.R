#' teal module: data overview
#'
#' Provides a browsable data preview, per-column missingness summary, and
#' dataset-level metrics for ADPPK.
#'
#' @param id module id
#' @return teal module
#' @export
#' @family teal-modules
mod_data_overview <- function(id) {
  # P2-8: guard for teal availability
  if (!requireNamespace("teal", quietly = TRUE))
    stop("mod_data_overview() requires the 'teal' package. Install it first.", call. = FALSE)
  if (!requireNamespace("teal.widgets", quietly = TRUE))
    stop("mod_data_overview() requires the 'teal.widgets' package. Install it first.", call. = FALSE)

  teal::module(
    label = "Data Overview",
    server = function(id, data) {
      shiny::moduleServer(id, function(input, output, session) {
        adppk <- shiny::reactive({
          d <- data()[["ADPPK"]]
          shiny::req(d)
          d
        })

        # Dataset-level metrics
        output$metrics_table <- shiny::renderTable({
          d <- adppk()
          data.frame(
            metric = c("records", "subjects", "periods", "variables",
                       "EVID=0 obs rows", "EVID=1 dose rows",
                       "n_missing_DV", "n_BLQ"),
            value = c(
              nrow(d),
              if ("USUBJID" %in% names(d)) length(unique(d$USUBJID)) else NA,
              if ("APERIOD" %in% names(d)) length(unique(stats::na.omit(d$APERIOD))) else NA,
              ncol(d),
              if ("EVID" %in% names(d)) sum(as.numeric(d$EVID) == 0, na.rm = TRUE) else NA,
              if ("EVID" %in% names(d)) sum(as.numeric(d$EVID) == 1, na.rm = TRUE) else NA,
              if ("DV" %in% names(d)) sum(is.na(d$DV)) else NA,
              if ("BLQFN" %in% names(d)) sum(as.numeric(d$BLQFN) == 1, na.rm = TRUE) else
                if ("BLQFL" %in% names(d)) sum(toupper(as.character(d$BLQFL)) == "Y", na.rm = TRUE) else NA
            ),
            stringsAsFactors = FALSE
          )
        }, striped = TRUE, hover = TRUE)

        # P1-8: per-column missingness summary
        output$missing_table <- DT::renderDataTable({
          d <- adppk()
          pct_miss <- vapply(d, function(x) round(100 * mean(is.na(x)), 2), numeric(1))
          df <- data.frame(
            variable    = names(pct_miss),
            type        = vapply(d, function(x) class(x)[1], character(1)),
            n_missing   = vapply(d, function(x) sum(is.na(x)), integer(1)),
            pct_missing = pct_miss,
            stringsAsFactors = FALSE
          )
          df <- df[order(-df$pct_missing), , drop = FALSE]
          DT::datatable(df,
            options  = list(pageLength = 15, scrollX = TRUE),
            rownames = FALSE,
            caption  = "Per-column missingness (sorted by % missing)"
          ) |>
            DT::formatStyle(
              "pct_missing",
              background = DT::styleColorBar(c(0, 100), "#fca5a5"),
              backgroundSize = "100% 80%",
              backgroundRepeat = "no-repeat",
              backgroundPosition = "center"
            )
        })

        # P1-8: browsable raw data preview
        output$data_preview <- DT::renderDataTable({
          DT::datatable(
            adppk(),
            filter  = "top",
            options = list(pageLength = 15, scrollX = TRUE, autoWidth = TRUE),
            rownames = FALSE,
            caption  = "ADPPK data (filterable)"
          )
        })
      })
    },
    ui = function(id) {
      ns <- shiny::NS(id)
      teal.widgets::standard_layout(
        output = shiny::div(
          shiny::h4("Dataset metrics"),
          shiny::tableOutput(ns("metrics_table")),
          shiny::hr(),
          shiny::h4("Per-column missingness"),
          DT::dataTableOutput(ns("missing_table")),
          shiny::hr(),
          shiny::h4("Data preview"),
          DT::dataTableOutput(ns("data_preview"))
        ),
        encoding = shiny::div(
          shiny::p("Use the downstream modules for check execution and visualization.")
        )
      )
    }
  )
}
