#' Run the pkchk Shiny app
#'
#' @export
run_app <- function() {
  reg <- checks_registry()

  ui <- shiny::fluidPage(
    bslib::bs_theme(version = 5),
    shiny::titlePanel("pkchk: NONMEM PK data review & checks"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::h4("Data input"),
        shiny::fileInput("file_adppk", "Upload ADPPK (.csv/.xlsx)", accept = c(".csv", ".xlsx")),
        shiny::selectInput("study_type", "Or generate dummy study type", choices = c("SAD", "MAD")),
        shiny::numericInput("n_subj", "Subjects", value = 40, min = 10, max = 500),
        shiny::actionButton("gen_dummy", "Generate dummy ADPPK"),
        shiny::hr(),
        shiny::h4("Checks"),
        shiny::checkboxGroupInput("checks", "Select checks", choices = stats::setNames(reg$id, reg$label), selected = reg$id[1:8]),
        shiny::actionButton("run_checks", "Run selected checks"),
        shiny::hr(),
        shiny::downloadButton("download_checks", "Download checklist summary (CSV)"),
        shiny::downloadButton("download_report", "Download checklist report (HTML)")
      ),
      shiny::mainPanel(
        shiny::tabsetPanel(
          shiny::tabPanel("Summary", shiny::tableOutput("summary_tbl"), shiny::tableOutput("head_tbl")),
          shiny::tabPanel("Visualization", shiny::plotOutput("pk_plot"), shiny::plotOutput("arm_box_plot")),
          shiny::tabPanel("Checks", shiny::tableOutput("check_result_tbl"), shiny::tableOutput("check_issue_tbl"))
        )
      )
    )
  )

  server <- function(input, output, session) {
    rv <- shiny::reactiveValues(adppk = NULL, addose = NULL, check_out = NULL)

    shiny::observeEvent(input$gen_dummy, {
      x <- generate_dummy_adppk(study_type = input$study_type, n_subj = input$n_subj, seed = 123)
      rv$adppk <- x$adppk
      rv$addose <- x$addose
    })

    shiny::observeEvent(input$file_adppk, {
      shiny::req(input$file_adppk)
      ext <- tolower(tools::file_ext(input$file_adppk$name))
      if (ext == "csv") {
        rv$adppk <- utils::read.csv(input$file_adppk$datapath, stringsAsFactors = FALSE)
      } else if (ext %in% c("xlsx", "xls")) {
        rv$adppk <- as.data.frame(readxl::read_excel(input$file_adppk$datapath))
      } else {
        shiny::showNotification("Unsupported file format. Use .csv or .xlsx", type = "error")
        return()
      }
      rv$addose <- if (all(c("USUBJID", "DOSE") %in% names(rv$adppk))) {
        unique(rv$adppk[, intersect(c("USUBJID", "DOSE", "DOSEU", "ADY", "ROUTE"), names(rv$adppk)), drop = FALSE])
      } else {
        data.frame(USUBJID = unique(rv$adppk$USUBJID), stringsAsFactors = FALSE)
      }
    })

    output$summary_tbl <- shiny::renderTable({
      shiny::req(rv$adppk)
      data.frame(
        metric = c("n_records", "n_subjects", "n_paramcd", "n_checks_selected"),
        value = c(
          nrow(rv$adppk),
          if ("USUBJID" %in% names(rv$adppk)) length(unique(rv$adppk$USUBJID)) else NA,
          if ("PARAMCD" %in% names(rv$adppk)) length(unique(rv$adppk$PARAMCD)) else NA,
          length(input$checks)
        ),
        stringsAsFactors = FALSE
      )
    })

    output$head_tbl <- shiny::renderTable({
      shiny::req(rv$adppk)
      utils::head(rv$adppk, 10)
    })

    output$pk_plot <- shiny::renderPlot({
      shiny::req(rv$adppk)
      d <- rv$adppk
      if (!all(c("ATPTN", "AVAL") %in% names(d))) return(invisible(NULL))
      graphics::plot(as.numeric(d$ATPTN), as.numeric(d$AVAL), pch = 19, col = "#2C7FB8", xlab = "ATPTN", ylab = "AVAL", main = "PK profile (all records)")
    })

    output$arm_box_plot <- shiny::renderPlot({
      shiny::req(rv$adppk)
      d <- rv$adppk
      if (!all(c("ARM", "AVAL") %in% names(d))) return(invisible(NULL))
      graphics::boxplot(as.numeric(AVAL) ~ ARM, data = d, col = "#A6CEE3", main = "AVAL by ARM", ylab = "AVAL")
    })

    shiny::observeEvent(input$run_checks, {
      shiny::req(rv$adppk)
      rv$check_out <- run_checks(rv$adppk, rv$addose, selected = input$checks)
    })

    result_table <- shiny::reactive({
      shiny::req(rv$check_out)
      do.call(rbind, lapply(rv$check_out, function(x) {
        data.frame(
          check_id = x$check_id,
          passed = x$passed,
          n_issue = x$n_issue,
          message = x$message,
          stringsAsFactors = FALSE
        )
      }))
    })

    output$check_result_tbl <- shiny::renderTable({ result_table() })

    output$check_issue_tbl <- shiny::renderTable({
      shiny::req(rv$check_out)
      tabs <- lapply(rv$check_out, function(x) {
        if (is.null(x$issue_table) || nrow(x$issue_table) == 0) return(NULL)
        cbind(check_id = x$check_id, x$issue_table, stringsAsFactors = FALSE)
      })
      tabs <- Filter(Negate(is.null), tabs)
      if (length(tabs) == 0) return(data.frame(info = "No issues found", stringsAsFactors = FALSE))
      do.call(rbind, tabs)
    })

    output$download_checks <- shiny::downloadHandler(
      filename = function() paste0("pkchk_checklist_summary_", Sys.Date(), ".csv"),
      content = function(file) {
        utils::write.csv(result_table(), file, row.names = FALSE)
      }
    )

    output$download_report <- shiny::downloadHandler(
      filename = function() paste0("pkchk_checklist_report_", Sys.Date(), ".html"),
      content = function(file) {
        shiny::req(rv$check_out)
        res <- result_table()
        pass_n <- sum(res$passed, na.rm = TRUE)
        fail_n <- sum(!res$passed, na.rm = TRUE)
        html <- c(
          "<html><head><meta charset='utf-8'><title>pkchk checklist report</title></head><body>",
          sprintf("<h1>pkchk checklist report</h1><p>Date: %s</p>", Sys.Date()),
          sprintf("<p>Records: %s | Subjects: %s | Pass: %s | Fail: %s</p>",
                  nrow(rv$adppk),
                  if ("USUBJID" %in% names(rv$adppk)) length(unique(rv$adppk$USUBJID)) else NA,
                  pass_n, fail_n),
          "<h2>Summary</h2>",
          paste(utils::capture.output(print(res, row.names = FALSE)), collapse = "<br/>"),
          "<h2>Issues</h2>"
        )

        for (nm in names(rv$check_out)) {
          x <- rv$check_out[[nm]]
          html <- c(html, sprintf("<h3>%s</h3><p>%s</p>", x$check_id, x$message))
          if (!is.null(x$issue_table) && nrow(x$issue_table) > 0) {
            html <- c(html, "<pre>", paste(utils::capture.output(print(x$issue_table, row.names = FALSE)), collapse = "\n"), "</pre>")
          }
        }
        html <- c(html, "</body></html>")
        writeLines(html, file)
      }
    )
  }

  shiny::shinyApp(ui = ui, server = server)
}
