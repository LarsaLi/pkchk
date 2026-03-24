#' teal module: check runner
#' @param id module id
#' @return teal module
#' @export
#' @family teal-modules
mod_check_runner <- function(id) {
  teal::module(
    label = "QC Checks",
    server = function(id, data) {
      shiny::moduleServer(id, function(input, output, session) {
        adppk <- shiny::reactive(data()[["ADPPK"]])
        addose <- shiny::reactive(data()[["ADDOSE"]])

        cfg <- shiny::reactive({
          if (!is.null(input$cfg_file) && nzchar(input$cfg_file$datapath)) {
            load_check_config(input$cfg_file$datapath)
          } else {
            load_check_config()
          }
        })

        check_out <- shiny::eventReactive(input$run_btn, {
          run_checks(adppk(), addose(), selected = input$selected_checks, cfg = cfg())
        })

        output$readiness_gauge <- shiny::renderPlot({
          shiny::req(check_out())
          s <- checks_to_summary(check_out())
          m <- model_readiness_score(s)
          draw_readiness_gauge(m)
        })

        output$check_table <- DT::renderDataTable({
          shiny::req(check_out())
          s <- checks_to_summary(check_out())
          DT::datatable(s, options = list(pageLength = 12, scrollX = TRUE), rownames = FALSE)
        })
      })
    },
    ui = function(id) {
      ns <- shiny::NS(id)
      teal.widgets::standard_layout(
        output = shiny::div(
          shiny::plotOutput(ns("readiness_gauge"), height = "220px"),
          DT::dataTableOutput(ns("check_table"))
        ),
        encoding = shiny::div(
          shiny::fileInput(ns("cfg_file"), "YAML config"),
          shiny::checkboxGroupInput(
            ns("selected_checks"),
            "Checks",
            choices = stats::setNames(checks_registry()$id, checks_registry()$label),
            selected = checks_registry()$id
          ),
          shiny::actionButton(ns("run_btn"), "Run checks", class = "btn-success", width = "100%")
        )
      )
    }
  )
}

#' draw readiness gauge
#' @param m output from model_readiness_score
#' @return ggplot object or base plot
#' @keywords internal
draw_readiness_gauge <- function(m) {
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    df <- data.frame(x = 1, y = m$score)
    ggplot2::ggplot(df, ggplot2::aes_string(x = "x", y = "y")) +
      ggplot2::geom_col(width = 0.35, fill = if (m$status == "READY") "#10b981" else if (m$status == "BLOCKED") "#ef4444" else "#f59e0b") +
      ggplot2::coord_cartesian(ylim = c(0, 100)) +
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.title = ggplot2::element_blank(), axis.text.x = ggplot2::element_blank()) +
      ggplot2::ggtitle(sprintf("Model readiness: %s (%s)", m$score, m$status))
  } else {
    graphics::barplot(
      m$score,
      ylim = c(0, 100),
      col = if (m$status == "READY") "darkgreen" else if (m$status == "BLOCKED") "firebrick" else "goldenrod",
      main = sprintf("Model readiness: %s (%s)", m$score, m$status),
      ylab = "Score"
    )
  }
}
