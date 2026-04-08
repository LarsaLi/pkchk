#' teal module: PK visualization
#' @param id module id
#' @return teal module
#' @export
#' @family teal-modules
mod_pk_visualization <- function(id) {
  # P2-8: guard for teal availability
  if (!requireNamespace("teal", quietly = TRUE))
    stop("mod_pk_visualization() requires the 'teal' package. Install it first.", call. = FALSE)
  if (!requireNamespace("teal.widgets", quietly = TRUE))
    stop("mod_pk_visualization() requires the 'teal.widgets' package. Install it first.", call. = FALSE)

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
            df <- d
            df$TIME_NUM  <- suppressWarnings(as.numeric(df$TIME))
            df$AVAL_NUM  <- suppressWarnings(as.numeric(df$AVAL))
            df$GROUP_COL <- if ("ARM" %in% names(df)) as.character(df$ARM) else
              if ("POPPKFL" %in% names(df)) as.character(df$POPPKFL) else "ALL"
            df$BLQ_MARK  <- if ("BLQFL" %in% names(df)) toupper(as.character(df$BLQFL)) == "Y" else FALSE
            lloq <- if ("LLOQ" %in% names(df)) suppressWarnings(min(as.numeric(df$LLOQ), na.rm = TRUE)) else NA_real_

            # P1-5: replace deprecated aes_string() with aes() + .data pronoun
            p <- ggplot2::ggplot(
              df,
              ggplot2::aes(x     = .data[["TIME_NUM"]],
                           y     = .data[["AVAL_NUM"]],
                           group = .data[["USUBJID"]],
                           color = .data[["GROUP_COL"]])
            ) +
              ggplot2::geom_line(alpha = 0.22) +
              ggplot2::geom_point(alpha = 0.5, size = 1.1) +
              ggplot2::theme_minimal() +
              ggplot2::labs(x = "TIME", y = "AVAL", color = "Group",
                            title = "Concentration-Time Spaghetti")

            mean_df <- stats::aggregate(AVAL_NUM ~ TIME_NUM, data = df,
                                        FUN = function(x) mean(x, na.rm = TRUE))
            sd_df   <- stats::aggregate(AVAL_NUM ~ TIME_NUM, data = df,
                                        FUN = function(x) stats::sd(x, na.rm = TRUE))
            ov <- merge(mean_df, sd_df, by = "TIME_NUM", suffixes = c("_mean", "_sd"))
            names(ov) <- c("TIME_NUM", "mean", "sd")
            ov$ymin <- pmax(0, ov$mean - ov$sd)
            ov$ymax <- ov$mean + ov$sd

            p <- p +
              ggplot2::geom_ribbon(
                data = ov,
                ggplot2::aes(x = .data[["TIME_NUM"]],
                             ymin = .data[["ymin"]],
                             ymax = .data[["ymax"]]),
                inherit.aes = FALSE, alpha = 0.15, fill = "#111827"
              ) +
              ggplot2::geom_line(
                data = ov,
                ggplot2::aes(x = .data[["TIME_NUM"]], y = .data[["mean"]]),
                inherit.aes = FALSE, linewidth = 0.9, color = "#111827"
              )

            if (is.finite(lloq))
              p <- p + ggplot2::geom_hline(yintercept = lloq, linetype = 2, color = "#ef4444")

            if (any(df$BLQ_MARK, na.rm = TRUE))
              p <- p + ggplot2::geom_point(
                data  = df[df$BLQ_MARK, , drop = FALSE],
                shape = 4, stroke = 1.1, size = 2, color = "#dc2626"
              )

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
            p <- ggplot2::ggplot(data.frame(dev = dev), ggplot2::aes(x = .data[["dev"]])) +
              ggplot2::geom_histogram(bins = 30, fill = "#86efac", color = "#14532d") +
              ggplot2::theme_minimal() +
              ggplot2::labs(x = "TIME - NTIME", y = "Count", title = "Sampling Time Deviation")
            print(p)
          } else {
            graphics::hist(dev, main = "Sampling Time Deviation", xlab = "TIME - NTIME", col = "#86efac")
          }
        })

        output$dose_cmax_plot <- shiny::renderPlot({
          d <- adppk()
          shiny::req(d)
          shiny::req(all(c("USUBJID", "AVAL", "DOSE") %in% names(d)))
          if ("EVID" %in% names(d)) d <- d[d$EVID == 0, , drop = FALSE]
          cmax <- stats::aggregate(AVAL ~ USUBJID, d,
                                   function(x) max(as.numeric(x), na.rm = TRUE))
          dose <- stats::aggregate(DOSE ~ USUBJID, adppk(), function(x) {
            y <- as.numeric(x)
            y <- y[is.finite(y) & y > 0]
            if (length(y) == 0) NA_real_ else max(y)
          })
          m <- merge(cmax, dose, by = "USUBJID", all = FALSE)
          if (nrow(m) == 0) return(invisible(NULL))
          if (requireNamespace("ggplot2", quietly = TRUE)) {
            # P1-5: replace deprecated aes_string()
            p <- ggplot2::ggplot(m, ggplot2::aes(x = .data[["DOSE"]], y = .data[["AVAL"]])) +
              ggplot2::geom_point(color = "#0ea5e9", alpha = 0.8) +
              ggplot2::theme_minimal() +
              ggplot2::labs(x = "DOSE", y = "Cmax", title = "DOSE vs Cmax")
            print(p)
          } else {
            graphics::plot(m$DOSE, m$AVAL, pch = 19, col = "#0ea5e9",
                           xlab = "DOSE", ylab = "Cmax", main = "DOSE vs Cmax")
          }
        })
      })
    },
    ui = function(id) {
      ns <- shiny::NS(id)
      teal.widgets::standard_layout(
        output = shiny::div(
          shiny::plotOutput(ns("pk_spaghetti"),  height = "320px"),
          shiny::plotOutput(ns("time_dev_plot"), height = "240px"),
          shiny::plotOutput(ns("dose_cmax_plot"), height = "240px")
        ),
        encoding = shiny::div(
          shiny::checkboxInput(ns("log_y"), "Log Y axis", value = FALSE)
        )
      )
    }
  )
}
