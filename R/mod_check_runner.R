#' teal module: check runner
#' @param id module id
#' @return teal module
#' @export
#' @family teal-modules
mod_check_runner <- function(id) {
  # P2-8: guard for teal availability
  if (!requireNamespace("teal", quietly = TRUE))
    stop("mod_check_runner() requires the 'teal' package. Install it first.", call. = FALSE)
  if (!requireNamespace("teal.widgets", quietly = TRUE))
    stop("mod_check_runner() requires the 'teal.widgets' package. Install it first.", call. = FALSE)

  # P0-6: consistent profile name — file is checks_popppk_pool.yml (triple-p in source)
  # UI label uses "poppk_pool" for clarity; path corrected to match actual filename.
  teal::module(
    label = "QC Checks",
    server = function(id, data) {
      shiny::moduleServer(id, function(input, output, session) {
        adppk  <- shiny::reactive(data()[["ADPPK"]])
        addose <- shiny::reactive(data()[["ADDOSE"]])

        cfg <- shiny::reactive({
          if (!is.null(input$cfg_file) && nzchar(input$cfg_file$datapath)) {
            load_check_config(input$cfg_file$datapath)
          } else {
            cfg_path <- switch(
              input$profile,
              default    = system.file("config", "checks_default.yml",       package = "pkchk"),
              sad_strict = system.file("config", "checks_sad_strict.yml",    package = "pkchk"),
              mad_sparse = system.file("config", "checks_mad_sparse.yml",    package = "pkchk"),
              poppk_pool = system.file("config", "checks_popppk_pool.yml",   package = "pkchk"),
              system.file("config", "checks_default.yml", package = "pkchk")
            )
            load_check_config(cfg_path)
          }
        })

        check_out <- shiny::eventReactive(input$run_btn, {
          run_checks(adppk(), addose(), selected = input$selected_checks, cfg = cfg())
        })

        # P2-5: compute summary once and reuse
        check_summary <- shiny::reactive({
          shiny::req(check_out())
          checks_to_summary(check_out())
        })

        output$readiness_gauge <- shiny::renderPlot({
          draw_readiness_gauge(model_readiness_score(check_summary()))
        })

        output$check_table <- DT::renderDataTable({
          s  <- check_summary()
          dt <- DT::datatable(s, options = list(pageLength = 12, scrollX = TRUE), rownames = FALSE)
          dt <- DT::formatStyle(
            dt,
            "status",
            backgroundColor = DT::styleEqual(c("pass", "fail", "skip", "error"),
                                              c("#dcfce7", "#fee2e2", "#f3f4f6", "#fef3c7")),
            color = DT::styleEqual(c("pass", "fail", "skip", "error"),
                                   c("#166534", "#991b1b", "#374151", "#92400e"))
          )
          DT::formatStyle(
            dt,
            "severity",
            fontWeight = DT::styleEqual(c("model_blocker", "error", "warn", "info"),
                                        c("700", "600", "500", "400"))
          )
        })

        output$download_summary <- shiny::downloadHandler(
          filename = function() paste0("pkchk-teal-check-summary-", Sys.Date(), ".csv"),
          content  = function(file) {
            shiny::req(check_out())
            utils::write.csv(check_summary(), file, row.names = FALSE)
          }
        )

        output$download_report <- shiny::downloadHandler(
          filename = function() paste0("pkchk-teal-check-report-", Sys.Date(), ".html"),
          content  = function(file) {
            shiny::req(check_out())
            generate_check_report_html(adppk(), check_out(), file, cfg = cfg())
          }
        )
      })
    },
    ui = function(id) {
      ns <- shiny::NS(id)
      # P2-5: call checks_registry() once
      reg <- checks_registry()
      # P3-7: default to model_blocker checks only to avoid freezing the UI
      default_cfg  <- tryCatch(load_check_config(), error = function(e) NULL)
      blocker_ids  <- if (!is.null(default_cfg)) {
        sev <- severity_overrides(default_cfg)
        intersect(reg$id, names(sev)[sev == "model_blocker"])
      } else {
        reg$id[seq_len(min(10L, nrow(reg)))]
      }

      teal.widgets::standard_layout(
        output = shiny::div(
          shiny::plotOutput(ns("readiness_gauge"), height = "220px"),
          DT::dataTableOutput(ns("check_table"))
        ),
        encoding = shiny::div(
          shiny::selectInput(
            ns("profile"),
            "Quick profile",
            choices  = c("default", "sad_strict", "mad_sparse", "poppk_pool"),
            selected = "default"
          ),
          shiny::fileInput(ns("cfg_file"), "YAML config (optional override)"),
          shiny::checkboxGroupInput(
            ns("selected_checks"),
            "Checks",
            choices  = stats::setNames(reg$id, reg$label),
            selected = blocker_ids   # P3-7: only blockers pre-selected
          ),
          shiny::actionButton(ns("run_btn"), "Run checks", class = "btn-success", width = "100%"),
          shiny::br(), shiny::br(),
          shiny::downloadButton(ns("download_summary"), "Download summary (CSV)"),
          shiny::downloadButton(ns("download_report"),  "Download report (HTML)")
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
    # P1-5: replace deprecated aes_string() with aes() + .data pronoun
    df <- data.frame(x = 1, y = m$score)
    ggplot2::ggplot(df, ggplot2::aes(x = .data[["x"]], y = .data[["y"]])) +
      ggplot2::geom_col(
        width = 0.35,
        fill  = if (m$status == "READY") "#10b981" else if (m$status == "BLOCKED") "#ef4444" else "#f59e0b"
      ) +
      ggplot2::coord_cartesian(ylim = c(0, 100)) +
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.title = ggplot2::element_blank(),
                     axis.text.x = ggplot2::element_blank()) +
      ggplot2::ggtitle(sprintf("Model readiness: %s (%s)", m$score, m$status))
  } else {
    graphics::barplot(
      m$score,
      ylim = c(0, 100),
      col  = if (m$status == "READY") "darkgreen" else if (m$status == "BLOCKED") "firebrick" else "goldenrod",
      main = sprintf("Model readiness: %s (%s)", m$score, m$status),
      ylab = "Score"
    )
  }
}
