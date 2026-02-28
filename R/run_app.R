#' Run the pkchk Shiny app
#'
#' @export
run_app <- function() {
  reg <- checks_registry()

  ui <- shiny::page_fluid(
    bslib::bs_theme(version = 5),
    shiny::titlePanel("pkchk: NONMEM PK data review & checks"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::h4("Data input"),
        shiny::fileInput("file_adppk", "Upload ADPPK (.csv)", accept = c(".csv")),
        shiny::selectInput("study_type", "Or generate dummy study type", choices = c("SAD", "MAD")),
        shiny::numericInput("n_subj", "Subjects", value = 40, min = 10, max = 500),
        shiny::actionButton("gen_dummy", "Generate dummy ADPPK"),
        shiny::hr(),
        shiny::h4("Checks"),
        shiny::checkboxGroupInput("checks", "Select checks", choices = stats::setNames(reg$id, reg$label), selected = reg$id),
        shiny::actionButton("run_checks", "Run selected checks"),
        shiny::hr(),
        shiny::downloadButton("download_checks", "Download checklist report (CSV)")
      ),
      shiny::mainPanel(
        shiny::tabsetPanel(
          shiny::tabPanel("Summary", shiny::tableOutput("summary_tbl"), shiny::tableOutput("head_tbl")),
          shiny::tabPanel("Visualization", shiny::plotOutput("pk_plot")),
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
      rv$adppk <- utils::read.csv(input$file_adppk$datapath, stringsAsFactors = FALSE)
      rv$addose <- data.frame(USUBJID = unique(rv$adppk$USUBJID), stringsAsFactors = FALSE)
    })

    output$summary_tbl <- shiny::renderTable({
      shiny::req(rv$adppk)
      data.frame(
        metric = c("n_records", "n_subjects", "n_paramcd"),
        value = c(nrow(rv$adppk), length(unique(rv$adppk$USUBJID)), length(unique(rv$adppk$PARAMCD))),
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
      stats::plot(d$ATPTN, d$AVAL, pch = 19, col = "#2C7FB8", xlab = "ATPTN", ylab = "AVAL", main = "PK profile (all records)")
    })

    shiny::observeEvent(input$run_checks, {
      shiny::req(rv$adppk)
      rv$check_out <- run_checks(rv$adppk, rv$addose, selected = input$checks)
    })

    output$check_result_tbl <- shiny::renderTable({
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
      filename = function() paste0("pkchk_checklist_", Sys.Date(), ".csv"),
      content = function(file) {
        shiny::req(rv$check_out)
        res <- do.call(rbind, lapply(rv$check_out, function(x) {
          data.frame(
            check_id = x$check_id,
            passed = x$passed,
            n_issue = x$n_issue,
            message = x$message,
            stringsAsFactors = FALSE
          )
        }))
        utils::write.csv(res, file, row.names = FALSE)
      }
    )
  }

  shiny::shinyApp(ui = ui, server = server)
}
