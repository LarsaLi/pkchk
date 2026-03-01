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
        shiny::selectInput("study_type", "Generate dummy study type", choices = c("SAD", "MAD")),
        shiny::numericInput("n_subj", "Subjects", value = 40, min = 10, max = 500),
        shiny::numericInput("period_n", "Number of periods", value = 1, min = 1, max = 10),
        shiny::actionButton("gen_dummy", "Generate dummy ADPPK"),
        shiny::downloadButton("download_dummy_sources", "Download dummy DM/EX/PC/ADPPK"),
        shiny::hr(),
        shiny::h4("Checks"),
        shiny::checkboxGroupInput("checks", "Select checks", choices = stats::setNames(reg$id, reg$label), selected = reg$id[1:8]),
        shiny::actionButton("select_all_checks", "Select all checks"),
        shiny::actionButton("run_checks", "Run selected checks"),
        shiny::hr(),
        shiny::downloadButton("download_checks", "Download checklist summary (CSV)"),
        shiny::downloadButton("download_report", "Download checklist report (HTML)")
      ),
      shiny::mainPanel(
        shiny::tabsetPanel(
          shiny::tabPanel("Summary", shiny::tableOutput("summary_tbl"), shiny::tableOutput("summary_more_tbl"), DT::dataTableOutput("adppk_dt")),
          shiny::tabPanel("Visualization", shiny::plotOutput("pk_plot"), shiny::plotOutput("arm_box_plot")),
          shiny::tabPanel("Checks", shiny::tableOutput("check_result_tbl"), shiny::tableOutput("check_issue_tbl"))
        )
      )
    )
  )

  server <- function(input, output, session) {
    rv <- shiny::reactiveValues(adppk = NULL, addose = NULL, dm = NULL, ex = NULL, pc = NULL, check_out = NULL, cfg = load_check_config())

    shiny::observeEvent(input$gen_dummy, {
      x <- generate_dummy_pk_package(study_type = input$study_type, n_subj = input$n_subj, period_n = input$period_n, seed = 123)
      rv$dm <- x$dm
      rv$ex <- x$ex
      rv$pc <- x$pc
      rv$adppk <- x$adppk
      rv$addose <- x$addose
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

    shiny::observeEvent(input$select_all_checks, {
      shiny::updateCheckboxGroupInput(session, "checks", selected = reg$id)
    })

    output$summary_tbl <- shiny::renderTable({
      shiny::req(rv$adppk)
      data.frame(
        metric = c("n_records", "n_subjects", "n_paramcd", "n_checks_selected", "n_periods", "pct_evid0", "pct_evid1"),
        value = c(
          nrow(rv$adppk),
          if ("USUBJID" %in% names(rv$adppk)) length(unique(rv$adppk$USUBJID)) else NA,
          if ("PARAMCD" %in% names(rv$adppk)) length(unique(rv$adppk$PARAMCD)) else NA,
          length(input$checks),
          if ("APERIOD" %in% names(rv$adppk)) length(unique(stats::na.omit(rv$adppk$APERIOD))) else NA,
          if ("EVID" %in% names(rv$adppk)) round(100 * mean(rv$adppk$EVID == 0, na.rm = TRUE), 2) else NA,
          if ("EVID" %in% names(rv$adppk)) round(100 * mean(rv$adppk$EVID == 1, na.rm = TRUE), 2) else NA
        ),
        stringsAsFactors = FALSE
      )
    })

    output$summary_more_tbl <- shiny::renderTable({
      shiny::req(rv$adppk)
      d <- rv$adppk
      data.frame(
        metric = c("n_missing_DV", "n_blq", "min_TIME", "max_TIME"),
        value = c(
          if ("DV" %in% names(d)) sum(is.na(d$DV)) else NA,
          if ("BLQFL" %in% names(d)) sum(toupper(as.character(d$BLQFL)) == "Y", na.rm = TRUE) else NA,
          if ("TIME" %in% names(d)) round(min(as.numeric(d$TIME), na.rm = TRUE), 3) else NA,
          if ("TIME" %in% names(d)) round(max(as.numeric(d$TIME), na.rm = TRUE), 3) else NA
        ),
        stringsAsFactors = FALSE
      )
    })

    output$adppk_dt <- DT::renderDataTable({
      shiny::req(rv$adppk)
      DT::datatable(rv$adppk, filter = "top", options = list(pageLength = 25, scrollX = TRUE))
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
