#' Run the pkchk Shiny app (dashboard UI)
#'
#' @export
run_app <- function() {
  # P1-4: serve external CSS from inst/app/www/
  shiny::addResourcePath("pkchk_www", system.file("app", "www", package = "pkchk"))
  reg <- checks_registry()

  ui <- shinydashboard::dashboardPage(
    skin = "black",
    shinydashboard::dashboardHeader(
      title = shiny::tags$span(style = "font-weight:700;", "pkchk"),
      titleWidth = 320
    ),
    shinydashboard::dashboardSidebar(
      width = 320,
      shinydashboard::sidebarMenu(
        id = "tabs",
        shinydashboard::menuItem("Data", tabName = "data", icon = shiny::icon("database")),
        shinydashboard::menuItem("Summary", tabName = "summary", icon = shiny::icon("table")),
        shinydashboard::menuItem("Visualization", tabName = "viz", icon = shiny::icon("chart-line")),
        shinydashboard::menuItem("Checks", tabName = "checks", icon = shiny::icon("check-circle"))
      ),
      shiny::hr(),
      shiny::h4("Data input"),
      shiny::fileInput("file_adppk", "Upload ADPPK (.csv/.xlsx)", accept = c(".csv", ".xlsx")),
      shiny::fileInput("file_cfg", "Optional check config (.yml/.yaml)", accept = c(".yml", ".yaml")),
      shiny::selectInput("study_type", "Dummy study type", choices = c("SAD", "MAD")),
      shiny::numericInput("n_subj", "Subjects", value = 40, min = 10, max = 500),
      shiny::numericInput("period_n", "Periods", value = 1, min = 1, max = 10),
      shiny::checkboxInput("inject_test_issues", "Inject realistic test issues", value = TRUE),
      shiny::selectInput("issue_level", "Issue level", choices = c("low", "medium", "high"), selected = "medium"),
      shiny::actionButton("gen_dummy", "Generate dummy ADPPK", class = "btn-primary"),
      shiny::downloadButton("download_dummy_sources", "Download DM/EX/PC/ADPPK"),
      shiny::hr(),
      shiny::h4("Checks"),
      shiny::checkboxGroupInput("checks", "Select checks", choices = stats::setNames(reg$id, reg$label), selected = reg$id[1:8]),
      shiny::actionButton("select_all_checks", "Select all"),
      shiny::actionButton("clear_all_checks", "Clear all"),
      shiny::actionButton("select_blockers", "Select model blockers"),
      shiny::actionButton("run_checks", "Run checks", class = "btn-success"),
      shiny::hr(),
      shiny::downloadButton("download_checks", "Download check summary (CSV)"),
      shiny::downloadButton("download_blockers", "Download model blockers (CSV)"),
      shiny::downloadButton("download_report", "Download check report (HTML)")
    ),
    shinydashboard::dashboardBody(
      shiny::tags$head(
        # P1-4: CSS moved to external file inst/app/www/styles.css
        shiny::tags$link(rel = "stylesheet", type = "text/css", href = "pkchk_www/styles.css")
      ),
      shinydashboard::tabItems(
        shinydashboard::tabItem(
          tabName = "data",
          shiny::fluidRow(
            shinydashboard::box(
              title = "Data status", width = 12, status = "primary", solidHeader = TRUE,
              shiny::uiOutput("data_status")
            )
          )
        ),
        shinydashboard::tabItem(
          tabName = "summary",
          shiny::fluidRow(
            shinydashboard::valueBoxOutput("vb_records", width = 2),
            shinydashboard::valueBoxOutput("vb_subjects", width = 2),
            shinydashboard::valueBoxOutput("vb_periods", width = 2),
            shinydashboard::valueBoxOutput("vb_blq", width = 2),
            shinydashboard::valueBoxOutput("vb_readiness", width = 2),
            shinydashboard::valueBoxOutput("vb_blockers", width = 2)
          ),
          shiny::fluidRow(
            shinydashboard::box(title = "Summary metrics", width = 4, status = "primary", solidHeader = TRUE,
                                DT::dataTableOutput("summary_dt")),
            shinydashboard::box(title = "ADPPK data review", width = 8, status = "primary", solidHeader = TRUE,
                                DT::dataTableOutput("adppk_dt"))
          )
        ),
        shinydashboard::tabItem(
          tabName = "viz",
          shiny::fluidRow(
            shinydashboard::box(title = "PK profile", width = 6, status = "info", solidHeader = TRUE,
                                shiny::plotOutput("pk_plot", height = 320)),
            shinydashboard::box(title = "AVAL by ARM", width = 6, status = "info", solidHeader = TRUE,
                                shiny::plotOutput("arm_box_plot", height = 320))
          )
        ),
        shinydashboard::tabItem(
          tabName = "checks",
          shiny::fluidRow(
            shinydashboard::box(
              title = "Check results (click a row for details)", width = 12, status = "success", solidHeader = TRUE,
              DT::dataTableOutput("check_result_dt"),
              shiny::br(),
              shiny::actionButton("open_check_modal", "Open selected check details", class = "btn-info")
            )
          ),
          shiny::fluidRow(
            shinydashboard::box(title = "Selected check issue details", width = 12, status = "warning", solidHeader = TRUE,
                                shiny::uiOutput("selected_check_title"),
                                DT::dataTableOutput("check_issue_selected_dt"))
          ),
          shiny::fluidRow(
            shinydashboard::box(title = "All issue details", width = 12, status = "warning", solidHeader = TRUE,
                                DT::dataTableOutput("check_issue_dt"))
          )
        )
      )
    )
  )

  server <- function(input, output, session) {
    rv <- shiny::reactiveValues(
      adppk = NULL, addose = NULL, dm = NULL, ex = NULL, pc = NULL,
      check_out = NULL, cfg = load_check_config(), source = "none", last_update = NA_character_
    )

    shiny::observeEvent(input$gen_dummy, {
      # P3-8: server-side input validation
      n_val <- as.integer(input$n_subj)
      p_val <- as.integer(input$period_n)
      if (is.na(n_val) || n_val < 1 || n_val > 10000) {
        shiny::showNotification("n_subj must be between 1 and 10000", type = "error")
        return()
      }
      if (is.na(p_val) || p_val < 1 || p_val > 50) {
        shiny::showNotification("period_n must be between 1 and 50", type = "error")
        return()
      }
      # P3-5: random seed for interactive generation so each click gives new data
      x <- generate_dummy_pk_package(
        study_type = input$study_type,
        n_subj = n_val,
        period_n = p_val,
        inject_test_issues = isTRUE(input$inject_test_issues),
        issue_level = input$issue_level,
        seed = sample.int(.Machine$integer.max, 1L)
      )
      rv$dm <- x$dm; rv$ex <- x$ex; rv$pc <- x$pc
      rv$adppk <- x$adppk; rv$addose <- x$addose
      rv$source <- sprintf("dummy (%s, n=%s, periods=%s)", input$study_type, input$n_subj, input$period_n)
      rv$last_update <- as.character(Sys.time())
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
      rv$source <- sprintf("upload (%s)", input$file_adppk$name)
      rv$last_update <- as.character(Sys.time())
      shiny::showNotification(sprintf("Uploaded ADPPK: %s rows, %s columns", nrow(rv$adppk), ncol(rv$adppk)), type = "message")
    })

    shiny::observeEvent(input$select_all_checks, {
      shiny::updateCheckboxGroupInput(session, "checks", selected = reg$id)
    })

    shiny::observeEvent(input$clear_all_checks, {
      shiny::updateCheckboxGroupInput(session, "checks", selected = character(0))
    })

    shiny::observeEvent(input$select_blockers, {
      # P3-2: derive blocker IDs dynamically from the current config rather than
      # a hardcoded list that could drift out of sync with the YAML.
      sev <- severity_overrides(rv$cfg)
      blocker_ids <- names(sev)[sev == "model_blocker"]
      shiny::updateCheckboxGroupInput(session, "checks", selected = intersect(reg$id, blocker_ids))
    })

    output$data_status <- shiny::renderUI({
      if (is.null(rv$adppk)) {
        shiny::tags$div("No dataset loaded yet. Upload ADPPK or generate dummy data.")
      } else {
        shiny::tags$ul(
          shiny::tags$li(sprintf("Source: %s", rv$source)),
          shiny::tags$li(sprintf("Records: %s", nrow(rv$adppk))),
          shiny::tags$li(sprintf("Columns: %s", ncol(rv$adppk))),
          shiny::tags$li(sprintf("Last update: %s", rv$last_update))
        )
      }
    })

    output$vb_records <- shinydashboard::renderValueBox({
      shiny::req(rv$adppk)
      shinydashboard::valueBox(format(nrow(rv$adppk), big.mark = ","), "Records", icon = shiny::icon("table"), color = "aqua")
    })

    output$vb_subjects <- shinydashboard::renderValueBox({
      shiny::req(rv$adppk)
      nsub <- if ("USUBJID" %in% names(rv$adppk)) length(unique(rv$adppk$USUBJID)) else NA
      shinydashboard::valueBox(nsub, "Subjects", icon = shiny::icon("users"), color = "green")
    })

    output$vb_periods <- shinydashboard::renderValueBox({
      shiny::req(rv$adppk)
      nper <- if ("APERIOD" %in% names(rv$adppk)) length(unique(stats::na.omit(rv$adppk$APERIOD))) else NA
      shinydashboard::valueBox(nper, "Periods", icon = shiny::icon("layer-group"), color = "yellow")
    })

    blq_stats <- shiny::reactive({
      shiny::req(rv$adppk)
      d <- rv$adppk
      obs <- if ("EVID" %in% names(d)) d[as.numeric(d$EVID) == 0, , drop = FALSE] else d
      if (nrow(obs) == 0) return(list(n_blq = 0, n_obs = 0, pct = NA_real_))

      if ("BLQFN" %in% names(obs)) {
        n_blq <- sum(as.numeric(obs$BLQFN) == 1, na.rm = TRUE)
      } else if ("BLQFL" %in% names(obs)) {
        n_blq <- sum(toupper(as.character(obs$BLQFL)) == "Y", na.rm = TRUE)
      } else {
        n_blq <- NA_real_
      }
      n_obs <- nrow(obs)
      pct <- if (is.na(n_blq) || n_obs == 0) NA_real_ else round(100 * n_blq / n_obs, 2)
      list(n_blq = n_blq, n_obs = n_obs, pct = pct)
    })

    output$vb_blq <- shinydashboard::renderValueBox({
      s <- blq_stats()
      val <- if (is.na(s$pct)) "NA" else paste0(s$pct, "%")
      shinydashboard::valueBox(val, sprintf("BLQ (%s/%s)", s$n_blq, s$n_obs), icon = shiny::icon("flask"), color = "purple")
    })

    readiness <- shiny::reactive({
      if (is.null(rv$check_out)) return(model_readiness_score(NULL))
      # P1-2: reuse result_table() reactive instead of calling checks_to_summary again
      model_readiness_score(result_table())
    })

    output$vb_readiness <- shinydashboard::renderValueBox({
      r <- readiness()
      col <- if (r$status == "READY") "green" else if (r$status == "REVIEW") "yellow" else if (r$status == "BLOCKED") "red" else "light-blue"
      shinydashboard::valueBox(r$score, "Model readiness", icon = shiny::icon("gauge-high"), color = col)
    })

    output$vb_blockers <- shinydashboard::renderValueBox({
      r <- readiness()
      shinydashboard::valueBox(r$blockers, "Model blockers", icon = shiny::icon("triangle-exclamation"), color = ifelse(r$blockers > 0, "red", "green"))
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
          blq_stats()$n_blq,
          if ("TIME" %in% names(d)) round(min(as.numeric(d$TIME), na.rm = TRUE), 3) else NA,
          if ("TIME" %in% names(d)) round(max(as.numeric(d$TIME), na.rm = TRUE), 3) else NA
        ),
        stringsAsFactors = FALSE
      )
      DT::datatable(x, options = list(dom = 't', paging = FALSE), rownames = FALSE)
    })

    output$adppk_dt <- DT::renderDataTable({
      shiny::req(rv$adppk)
      DT::datatable(rv$adppk, filter = "top", options = list(pageLength = 20, scrollX = TRUE, autoWidth = TRUE), rownames = FALSE)
    })

    output$pk_plot <- shiny::renderPlot({
      shiny::req(rv$adppk)
      d <- rv$adppk
      if (!all(c("ATPTN", "AVAL") %in% names(d))) {
        graphics::plot.new(); graphics::text(0.5, 0.5, "ATPTN/AVAL not available")
        return(invisible(NULL))
      }
      idx <- which(!is.na(d$ATPTN) & !is.na(d$AVAL) & d$EVID == 0)
      if (length(idx) == 0) { graphics::plot.new(); graphics::text(0.5, 0.5, "No EVID=0 observation records"); return(invisible(NULL)) }
      graphics::plot(as.numeric(d$ATPTN[idx]), as.numeric(d$AVAL[idx]), pch = 19, col = "#2C7FB8", xlab = "ATPTN", ylab = "AVAL", main = "PK profile (EVID=0)")
    })

    output$arm_box_plot <- shiny::renderPlot({
      shiny::req(rv$adppk)
      d <- rv$adppk
      if (!all(c("ARM", "AVAL") %in% names(d))) {
        graphics::plot.new(); graphics::text(0.5, 0.5, "ARM/AVAL not available")
        return(invisible(NULL))
      }
      idx <- which(!is.na(d$ARM) & !is.na(d$AVAL) & d$EVID == 0)
      if (length(idx) == 0) { graphics::plot.new(); graphics::text(0.5, 0.5, "No EVID=0 observation records"); return(invisible(NULL)) }
      graphics::boxplot(as.numeric(AVAL) ~ ARM, data = d[idx, , drop = FALSE], col = "#A6CEE3", main = "AVAL by ARM", ylab = "AVAL")
    })

    shiny::observeEvent(input$run_checks, {
      shiny::req(rv$adppk)
      rv$check_out <- run_checks(rv$adppk, rv$addose, selected = input$checks, cfg = rv$cfg)
      shiny::showNotification("Checks completed", type = "message")
      shinydashboard::updateTabItems(session, "tabs", "checks")
    })

    result_table <- shiny::reactive({
      shiny::req(rv$check_out)
      checks_to_summary(rv$check_out)
    })

    output$check_result_dt <- DT::renderDataTable({
      x <- result_table_view()
      DT::datatable(
        x[, c("check_id", "rule_version", "severity", "status", "n_issue_view", "message")],
        escape = FALSE,
        options = list(
          pageLength = 15,
          scrollX = TRUE,
          columnDefs = list(list(targets = 4, title = "n_issue"))
        ),
        rownames = FALSE,
        selection = "single",
        callback = DT::JS(
          "table.on('click', 'a.issue-link', function(e){",
          "  e.preventDefault();",
          "  var ck = $(this).data('check');",
          "  Shiny.setInputValue('check_link_clicked', ck, {priority: 'event'});",
          "});"
        )
      )
    })

    result_table_view <- shiny::reactive({
      rt <- result_table()
      rt$n_issue_view <- ifelse(
        rt$n_issue > 0,
        sprintf("<a href='#' class='issue-link' data-check='%s'>%s</a>", rt$check_id, rt$n_issue),
        as.character(rt$n_issue)
      )
      rt
    })

    selected_check <- shiny::reactive({
      shiny::req(rv$check_out)
      clicked <- input$check_link_clicked
      if (!is.null(clicked) && nzchar(clicked)) return(clicked)
      idx <- input$check_result_dt_rows_selected
      if (is.null(idx) || length(idx) == 0) return(NULL)
      rt <- result_table()
      rt$check_id[idx[1]]
    })

    output$selected_check_title <- shiny::renderUI({
      cid <- selected_check()
      if (is.null(cid)) return(shiny::tags$em("Select one check above to inspect detailed records."))
      msg <- rv$check_out[[cid]]$message
      shiny::tags$div(
        shiny::tags$b(cid),
        shiny::tags$div(style = "margin-top:6px;color:#475467;", msg)
      )
    })

    output$check_issue_selected_dt <- DT::renderDataTable({
      shiny::req(rv$check_out)
      cid <- selected_check()
      if (is.null(cid)) {
        return(DT::datatable(data.frame(info = "No check selected", stringsAsFactors = FALSE), options = list(dom = 't'), rownames = FALSE))
      }
      tab <- rv$check_out[[cid]]$issue_table
      if (is.null(tab) || nrow(tab) == 0) {
        return(DT::datatable(data.frame(info = "No issue rows for selected check", stringsAsFactors = FALSE), options = list(dom = 't'), rownames = FALSE))
      }
      DT::datatable(tab, options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE)
    })

    shiny::observeEvent(input$open_check_modal, {
      shiny::req(rv$check_out)
      cid <- selected_check()
      if (is.null(cid)) {
        shiny::showNotification("Please select one check row first.", type = "warning")
        return()
      }
      tab <- rv$check_out[[cid]]$issue_table
      msg <- rv$check_out[[cid]]$message
      if (is.null(tab) || nrow(tab) == 0) {
        tab <- data.frame(info = "No issue rows for selected check", stringsAsFactors = FALSE)
      }

      # contextual ADPPK record preview (when USUBJID/TIME available)
      rec_preview <- data.frame(info = "No linked ADPPK preview available", stringsAsFactors = FALSE)
      if (!is.null(rv$adppk) && nrow(rv$adppk) > 0 && "USUBJID" %in% names(tab) && "USUBJID" %in% names(rv$adppk)) {
        ids <- unique(stats::na.omit(tab$USUBJID))
        p <- rv$adppk[rv$adppk$USUBJID %in% ids, , drop = FALSE]
        if ("TIME" %in% names(tab) && "TIME" %in% names(rv$adppk)) {
          tvals <- unique(stats::na.omit(as.numeric(tab$TIME)))
          if (length(tvals) > 0) p <- p[as.numeric(p$TIME) %in% tvals, , drop = FALSE]
        }
        if (nrow(p) > 0) rec_preview <- utils::head(p, 30)
      }

      shiny::showModal(shiny::modalDialog(
        title = paste("Check details:", cid),
        shiny::tags$p(msg),
        shiny::tags$h4("Issue rows"),
        DT::datatable(tab, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE),
        shiny::tags$h4("Linked ADPPK record preview"),
        DT::datatable(rec_preview, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE),
        easyClose = TRUE,
        size = "l"
      ))
    })

    output$check_issue_dt <- DT::renderDataTable({
      shiny::req(rv$check_out)
      DT::datatable(checks_to_issues(rv$check_out), options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE)
    })

    output$download_dummy_sources <- shiny::downloadHandler(
      filename = function() paste0("pkchk_dummy_sources_", Sys.Date(), ".zip"),
      content = function(file) {
        shiny::req(rv$dm, rv$ex, rv$pc, rv$adppk)
        # P1-3: use absolute paths + "-j" flag instead of setwd() to avoid
        # changing the process working directory (race condition in multi-user servers)
        td <- tempfile("pkchk_zip_")
        dir.create(td, showWarnings = FALSE)
        f_dm <- file.path(td, "DM.csv")
        f_ex <- file.path(td, "EX.csv")
        f_pc <- file.path(td, "PC.csv")
        f_ad <- file.path(td, "ADPPK.csv")
        utils::write.csv(rv$dm,    f_dm, row.names = FALSE)
        utils::write.csv(rv$ex,    f_ex, row.names = FALSE)
        utils::write.csv(rv$pc,    f_pc, row.names = FALSE)
        utils::write.csv(rv$adppk, f_ad, row.names = FALSE)
        utils::zip(zipfile = file, files = c(f_dm, f_ex, f_pc, f_ad), flags = "-j")
      }
    )

    output$download_checks <- shiny::downloadHandler(
      filename = function() paste0("pkchk_checklist_summary_", Sys.Date(), ".csv"),
      content = function(file) {
        # P0-3: req() inside downloadHandler content prevents crash before checks run
        shiny::req(rv$check_out)
        utils::write.csv(result_table(), file, row.names = FALSE)
      }
    )

    output$download_blockers <- shiny::downloadHandler(
      filename = function() paste0("pkchk_model_blockers_", Sys.Date(), ".csv"),
      content = function(file) {
        shiny::req(rv$check_out)
        # P1-2: reuse result_table() reactive instead of calling checks_to_summary again
        x <- result_table()
        x <- x[x$severity == "model_blocker" & x$status == "fail", , drop = FALSE]
        if (nrow(x) == 0) x <- data.frame(info = "No model blocker failures", stringsAsFactors = FALSE)
        utils::write.csv(x, file, row.names = FALSE)
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
