#' Run the pkchk Shiny app
#'
#' @export
run_app <- function() {
  reg <- checks_registry()

  ui <- shiny::fluidPage(
    theme = bslib::bs_theme(version = 5),
    shiny::titlePanel("pkchk: NONMEM PK data review & checks"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::h4("Data input"),
        shiny::fileInput("file_adppk", "Upload ADPPK (.csv/.xlsx)", accept = c(".csv", ".xlsx")),
        shiny::fileInput("file_cfg", "Optional check config (.yml/.yaml)", accept = c(".yml", ".yaml")),
        shiny::selectInput("study_type", "Or generate dummy study type", choices = c("SAD", "MAD")),
        shiny::numericInput("n_subj", "Subjects", value = 40, min = 10, max = 500),
        shiny::actionButton("gen_dummy", "Generate dummy ADPPK"),
        shiny::actionButton("load_example", "Load built-in example"),
        shiny::downloadButton("download_dummy_sources", "Download dummy DM/EX/PC/ADPPK"),
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
    rv <- shiny::reactiveValues(adppk = NULL, addose = NULL, dm = NULL, ex = NULL, pc = NULL, check_out = NULL, cfg = load_check_config())

    shiny::observeEvent(input$gen_dummy, {
      x <- generate_dummy_pk_package(study_type = input$study_type, n_subj = input$n_subj, seed = 123)
      rv$dm <- x$dm
      rv$ex <- x$ex
      rv$pc <- x$pc
      rv$adppk <- x$adppk
      rv$addose <- x$addose
    })

    shiny::observeEvent(input$load_example, {
      ex <- system.file("extdata", "example_adppk.csv", package = "pkchk")
      if (!nzchar(ex)) {
        shiny::showNotification("No built-in example found", type = "error")
        return()
      }
      rv$adppk <- utils::read.csv(ex, stringsAsFactors = FALSE)
      rv$addose <- if (all(c("USUBJID", "DOSE") %in% names(rv$adppk))) {
        unique(rv$adppk[, intersect(c("USUBJID", "DOSE", "DOSEU", "ADY", "ROUTE"), names(rv$adppk)), drop = FALSE])
      } else {
        data.frame(USUBJID = unique(rv$adppk$USUBJID), stringsAsFactors = FALSE)
      }
    })

    shiny::observeEvent(input$file_cfg, {
      shiny::req(input$file_cfg)
      ext <- tolower(tools::file_ext(input$file_cfg$name))
      if (!ext %in% c("yml", "yaml")) {
        shiny::showNotification("Config must be .yml/.yaml", type = "error")
        return()
      }
      rv$cfg <- load_check_config(input$file_cfg$datapath)
      ids <- enabled_checks(rv$cfg)
      shiny::updateCheckboxGroupInput(session, "checks", selected = ids)
      shiny::showNotification(sprintf("Config loaded: %s checks enabled", length(ids)), type = "message")
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
      rv$check_out <- run_checks(rv$adppk, rv$addose, selected = input$checks, cfg = rv$cfg)
    })

    result_table <- shiny::reactive({
      shiny::req(rv$check_out)
      checks_to_summary(rv$check_out)
    })

    output$check_result_tbl <- shiny::renderTable({ result_table() })

    output$check_issue_tbl <- shiny::renderTable({
      shiny::req(rv$check_out)
      checks_to_issues(rv$check_out)
    })

    output$download_dummy_sources <- shiny::downloadHandler(
      filename = function() paste0("pkchk_dummy_sources_", Sys.Date(), ".zip"),
      content = function(file) {
        shiny::req(rv$dm, rv$ex, rv$pc, rv$adppk)
        td <- tempdir()
        f_dm <- file.path(td, "DM.csv")
        f_ex <- file.path(td, "EX.csv")
        f_pc <- file.path(td, "PC.csv")
        f_ad <- file.path(td, "ADPPK.csv")
        utils::write.csv(rv$dm, f_dm, row.names = FALSE)
        utils::write.csv(rv$ex, f_ex, row.names = FALSE)
        utils::write.csv(rv$pc, f_pc, row.names = FALSE)
        utils::write.csv(rv$adppk, f_ad, row.names = FALSE)
        owd <- getwd()
        on.exit(setwd(owd), add = TRUE)
        setwd(td)
        utils::zip(zipfile = file, files = c("DM.csv", "EX.csv", "PC.csv", "ADPPK.csv"))
      }
    )

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
        generate_check_report_html(rv$adppk, rv$check_out, file, cfg = rv$cfg)
      }
    )
  }

  shiny::shinyApp(ui = ui, server = server)
}
