#' Run pkchk as a teal application
#'
#' This is an optional teal-based UI entrypoint that keeps the existing
#' `run_app()` dashboard workflow unchanged.
#'
#' @param adppk Optional ADPPK data frame.
#' @param addose Optional ADDOSE data frame.
#' @param dm Optional DM data frame.
#' @param ex Optional EX data frame.
#' @param pc Optional PC data frame.
#' @return Starts a shiny app.
#' @export
#' @family application
run_teal_app <- function(adppk = NULL, addose = NULL, dm = NULL, ex = NULL, pc = NULL) {
  req_pkgs <- c("teal", "teal.data", "teal.widgets", "teal.modules.general", "teal.reporter")
  missing <- req_pkgs[!vapply(req_pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    stop(
      "run_teal_app() requires optional packages: ", paste(missing, collapse = ", "),
      ". Install them first.",
      call. = FALSE
    )
  }

  if (is.null(adppk) || is.null(addose) || is.null(dm) || is.null(ex) || is.null(pc)) {
    pkg <- generate_dummy_pk_package(
      study_type = "SAD",
      n_subj = 40,
      inject_test_issues = TRUE,
      issue_level = "medium"
    )
    adppk <- pkg$adppk
    addose <- pkg$addose
    dm <- pkg$dm
    ex <- pkg$ex
    pc <- pkg$pc
  }

  data <- teal.data::teal_data(
    ADPPK = adppk,
    ADDOSE = addose,
    DM = dm,
    EX = ex,
    PC = pc
  )

  app <- teal::init(
    data = data,
    modules = teal::modules(
      mod_data_overview("data_overview"),
      mod_check_runner("check_runner"),
      mod_pk_visualization("pk_viz"),
      teal.reporter::reporter_previewer_module("reporter")
    ),
    title = "pkchk - NONMEM PK Dataset QC",
    header = shiny::tagList(
      shiny::tags$span("pkchk", style = "font-weight:700; font-size:16px;"),
      shiny::tags$span(
        paste0("v", utils::packageVersion("pkchk")),
        style = "color:#6b7280; font-size:13px; margin-left:8px;"
      )
    )
  )

  shiny::runApp(app)
}
