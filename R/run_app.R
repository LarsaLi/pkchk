#' Run the pkchk Shiny app
#'
#' @export
run_app <- function() {
  reg <- checks_registry()

  ui <- shiny::fluidPage(
    theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
    shiny::tags$head(
      shiny::tags$style(shiny::HTML(".app-subtitle{color:#5c6b73;margin-top:-8px;margin-bottom:12px;} .btn{margin-right:6px;margin-bottom:6px;}"))
    ),
    shiny::titlePanel("pkchk - NONMEM PK data review & checks"),
    shiny::div(class = "app-subtitle", "Upload ADPPK or generate CDISC-like dummy data from DM/EX/PC, then run configurable QC checks."),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::h4("Data"),
        shiny::fileInput("file_adppk", "Upload ADPPK (.csv/.xlsx)", accept = c(".csv", ".xlsx")),
        shiny::fileInput("file_cfg", "Optional check config (.yml/.yaml)", accept = c(".yml", ".yaml")),
        shiny::selectInput("study_type", "Dummy study type", choices = c("SAD", "MAD")),
        shiny::numericInput("n_subj", "Subjects", value = 40, min = 10, max = 500),
        shiny::numericInput("period_n", "Periods", value = 1, min = 1, max = 10),
        shiny::actionButton("gen_dummy", "Generate dummy ADPPK", class = "btn-primary"),
        shiny::downloadButton("download_dummy_sources", "Download DM/EX/PC/ADPPK"),
        shiny::hr(),
        shiny::h4("Checks"),
        shiny::checkboxGroupInput("checks", "Select checks", choices = stats::setNames(reg$id, reg$label), selected = reg$id[1:8]),
        shiny::actionButton("select_all_checks", "Select all"),
        shiny::actionButton("run_checks", "Run checks", class = "btn-success"),
        shiny::hr(),
        shiny::downloadButton("download_checks", "Download check summary (CSV)"),
        shiny::downloadButton("download_report", "Download check report (HTML)")
      ),
      shiny::mainPanel(
        shiny::tabsetPanel(
          shiny::tabPanel("Summary", shiny::uiOutput("kpi_cards"), DT::dataTableOutput("summary_dt"), DT::dataTableOutput("adppk_dt")),
          shiny::tabPanel("Visualization", shiny::plotOutput("pk_plot", height = 320), shiny::plotOutput("arm_box_plot", height = 320)),
          shiny::tabPanel("Checks", DT::dataTableOutput("check_result_dt"), DT::dataTableOutput("check_issue_dt"))
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
      shiny::showNotification(sprintf("Dummy generated: %s records, %s subjects", nrow(rv$adppk), length(unique(rv$adppk$USUBJID))), type = "message")
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
      shiny::showNotification(sprintf("Uploaded ADPPK: %s rows, %s columns", nrow(rv$adppk), ncol(rv$adppk)), type = "message")
    })

    shiny::observeEvent(input$select_all_checks, {
      shiny::updateCheckboxGroupInput(session, "checks", selected = reg$id)
    })

    output$kpi_cards <- shiny::renderUI({
      shiny::req(rv$adppk)
      d <- rv$adppk
      nrec <- nrow(d)
      nsub <- if ("USUBJID" %in% names(d)) length(unique(d$USUBJID)) else NA
      nper <- if ("APERIOD" %in% names(d)) length(unique(stats::na.omit(d$APERIOD))) else NA
      pblq <- if ("BLQFL" %in% names(d)) round(100 * mean(toupper(as.character(d$BLQFL)) == "Y", na.rm = TRUE), 2) else NA

      shiny::div(
        style = "display:flex; gap:12px; flex-wrap:wrap; margin-bottom:12px;",
        bslib::value_box(title = "Records", value = nrec, theme = bslib::value_box_theme(bg = "#e9f5ff", fg = "#0b4f7a")),
        bslib::value_box(title = "Subjects", value = nsub, theme = bslib::value_box_theme(bg = "#eef9ef", fg = "#1c6b2a")),
        bslib::value_box(title = "Periods", value = nper, theme = bslib::value_box_theme(bg = "#fff5e6", fg = "#8a4b00")),
        bslib::value_box(title = "BLQ %", value = pblq, theme = bslib::value_box_theme(bg = "#f7efff", fg = "#5b2a86"))
      )
    })

    output$summary_dt <- DT::renderDataTable({
      shiny::req(rv$adppk)
      d <- rv$adppk
      x <- data.frame(
        metric = c("n_records", "n_subjects", "n_paramcd", "n_checks_selected", "n_periods", "pct_evid0", "pct_evid1", "n_missing_DV", "n_blq", "min_TIME", "max_TIME"),
        value = c(
          nrow(d),
          if ("USUBJID" %in% names(d)) length(unique(d$USUBJID)) else NA,
          if ("PARAMCD" %in% names(d)) length(unique(d$PARAMCD)) else NA,
          length(input$checks),
          if ("APERIOD" %in% names(d)) length(unique(stats::na.omit(d$APERIOD))) else NA,
          if ("EVID" %in% names(d)) round(100 * mean(d$EVID == 0, na.rm = TRUE), 2) else NA,
          if ("EVID" %in% names(d)) round(100 * mean(d$EVID == 1, na.rm = TRUE), 2) else NA,
          if ("DV" %in% names(d)) sum(is.na(d$DV)) else NA,
          if ("BLQFL" %in% names(d)) sum(toupper(as.character(d$BLQFL)) == "Y", na.rm = TRUE) else NA,
          if ("TIME" %in% names(d)) round(min(as.numeric(d$TIME), na.rm = TRUE), 3) else NA,
          if ("TIME" %in% names(d)) round(max(as.numeric(d$TIME), na.rm = TRUE), 3) else NA
        ),
        stringsAsFactors = FALSE
      )
      DT::datatable(x, options = list(dom = 't', paging = FALSE), rownames = FALSE)
    })

    output$adppk_dt <- DT::renderDataTable({
      shiny::req(rv$adppk)
      DT::datatable(rv$adppk, filter = "top", options = list(pageLength = 25, scrollX = TRUE, autoWidth = TRUE), rownames = FALSE)
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

    output$check_result_dt <- DT::renderDataTable({
      DT::datatable(result_table(), options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE)
    })

    output$check_issue_dt <- DT::renderDataTable({
      shiny::req(rv$check_out)
      DT::datatable(checks_to_issues(rv$check_out), options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE)
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
