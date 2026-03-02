#' Convert check results to summary data frame
#' @param check_out Named list from `run_checks()`
#' @return data.frame
#' @export
checks_to_summary <- function(check_out) {
  do.call(rbind, lapply(check_out, function(x) {
    data.frame(
      check_id = x$check_id,
      rule_version = if (!is.null(x$rule_version)) x$rule_version else "1.0.0",
      severity = if (!is.null(x$severity)) x$severity else "error",
      status = if (!is.null(x$status)) x$status else if (isTRUE(x$passed)) "pass" else "fail",
      passed = x$passed,
      n_issue = x$n_issue,
      message = x$message,
      stringsAsFactors = FALSE
    )
  }))
}

#' Compute model readiness score from check summary
#' @param summary_df Output of `checks_to_summary()`
#' @return list with score and counters.
#' @export
model_readiness_score <- function(summary_df) {
  if (is.null(summary_df) || nrow(summary_df) == 0) {
    return(list(score = 0, blockers = 0, fail = 0, pass = 0, skip = 0, status = "NO_DATA"))
  }
  pass_n <- sum(summary_df$status == "pass", na.rm = TRUE)
  fail_n <- sum(summary_df$status == "fail", na.rm = TRUE)
  skip_n <- sum(summary_df$status == "skip", na.rm = TRUE)
  blocker_n <- sum(summary_df$status == "fail" & summary_df$severity == "model_blocker", na.rm = TRUE)
  base <- if ((pass_n + fail_n) == 0) 0 else round(100 * pass_n / (pass_n + fail_n), 1)
  score <- max(0, base - blocker_n * 10)
  status <- if (blocker_n > 0) "BLOCKED" else if (score >= 90) "READY" else if (score >= 70) "REVIEW" else "NOT_READY"
  list(score = score, blockers = blocker_n, fail = fail_n, pass = pass_n, skip = skip_n, status = status)
}

#' Build issue table from check results
#' @param check_out Named list from `run_checks()`
#' @return data.frame
#' @export
checks_to_issues <- function(check_out) {
  suggestion_map <- c(
    required_vars = "Add missing required ADPPK variables before modeling.",
    name_label_len = "Shorten variable names/labels to standard limits.",
    pk_no_dose = "Confirm dosing extraction and merge keys for affected subjects.",
    poppk_consistency = "Review POPPK inclusion rule and flag derivation logic.",
    char_num_mapping = "Rebuild controlled terminology map (char <-> numeric).",
    predose_time = "Recalculate relative times using first dose anchor.",
    sampling_dev_10pct = "Check nominal/actual time derivation and sampling windows.",
    duplicates = "Deduplicate by subject-time-event key after merges.",
    mdv_assignment = "Re-derive MDV from DV/EVID and missingness rules.",
    amt_for_dose = "Ensure AMT is populated for every EVID=1 record."
  )

  tabs <- lapply(check_out, function(x) {
    if (is.null(x$issue_table) || nrow(x$issue_table) == 0) return(NULL)
    sug <- if (is.null(suggestion_map[[x$check_id]])) "Review source derivation and study-specific rule." else unname(suggestion_map[[x$check_id]])
    cbind(
      check_id = x$check_id,
      severity = if (!is.null(x$severity)) x$severity else "error",
      suggestion = sug,
      x$issue_table,
      stringsAsFactors = FALSE
    )
  })
  tabs <- Filter(Negate(is.null), tabs)
  if (length(tabs) == 0) return(data.frame(info = "No issues found", stringsAsFactors = FALSE))
  do.call(rbind, tabs)
}

#' Generate HTML checklist report
#' @param adppk ADPPK data frame
#' @param check_out Named list from `run_checks()`
#' @param file Output html path
#' @param cfg Optional check config list.
#' @return Invisibly returns file path
#' @export
generate_check_report_html <- function(adppk, check_out, file, cfg = NULL) {
  res <- checks_to_summary(check_out)
  pass_n <- sum(res$status == "pass", na.rm = TRUE)
  fail_n <- sum(res$status == "fail", na.rm = TRUE)
  skip_n <- sum(res$status == "skip", na.rm = TRUE)

  pkg_ver <- as.character(utils::packageVersion("pkchk"))
  git_sha <- "NA"
  if (dir.exists(".git")) {
    git_sha <- tryCatch(suppressWarnings(system2("git", c("rev-parse", "--short", "HEAD"), stdout = TRUE, stderr = FALSE)[1]), error = function(e) "NA")
  }

  html <- c(
    "<html><head><meta charset='utf-8'><title>pkchk checklist report</title>\n",
    "<style>body{font-family:-apple-system,BlinkMacSystemFont,Segoe UI,Roboto,sans-serif;margin:24px;}\n",
    "table{border-collapse:collapse;width:100%;margin:12px 0;}th,td{border:1px solid #ddd;padding:6px;font-size:13px;}\n",
    "th{background:#f5f5f5;} .ok{color:#0a7d23;} .bad{color:#b00020;} .muted{color:#555;} </style></head><body>",
    sprintf("<h1>pkchk checklist report</h1><p class='muted'>Date: %s</p>", Sys.Date()),
    sprintf("<p><b>Package</b>: pkchk %s | <b>Git</b>: %s</p>", pkg_ver, git_sha),
    sprintf("<p><b>Records</b>: %s | <b>Subjects</b>: %s | <b>Pass</b>: <span class='ok'>%s</span> | <b>Fail</b>: <span class='bad'>%s</span> | <b>Skip</b>: %s</p>",
            nrow(adppk), if ("USUBJID" %in% names(adppk)) length(unique(adppk$USUBJID)) else NA, pass_n, fail_n, skip_n),
    "<h2>Summary</h2>",
    "<table><tr><th>check_id</th><th>severity</th><th>status</th><th>n_issue</th><th>message</th></tr>"
  )

  for (i in seq_len(nrow(res))) {
    row <- res[i, ]
    html <- c(html, sprintf("<tr><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td></tr>",
                            row$check_id, row$severity, row$status, row$n_issue, row$message))
  }
  html <- c(html, "</table>")

  if (!is.null(cfg) && !is.null(cfg$checks)) {
    html <- c(html, "<h2>Effective configuration</h2>",
              sprintf("<p>Profile: %s | Version: %s</p>",
                      ifelse(is.null(cfg$profile), "NA", cfg$profile),
                      ifelse(is.null(cfg$version), "NA", cfg$version)))

    ids <- names(cfg$checks)
    html <- c(html, "<table><tr><th>check_id</th><th>enabled</th><th>severity</th><th>rule_version</th><th>thresholds</th></tr>")
    for (id in ids) {
      cki <- cfg$checks[[id]]
      en <- ifelse(is.null(cki$enabled), NA, cki$enabled)
      sev <- ifelse(is.null(cki$severity), "error", cki$severity)
      rv <- ifelse(is.null(cki$rule_version), "1.0.0", cki$rule_version)
      thr <- cki[setdiff(names(cki), c("enabled", "severity", "rule_version"))]
      thr_txt <- if (length(thr) == 0) "" else paste(names(thr), unlist(thr), sep = "=", collapse = "; ")
      html <- c(html, sprintf("<tr><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td></tr>", id, en, sev, rv, thr_txt))
    }
    html <- c(html, "</table>")
  }

  html <- c(html, "<h2>Issue details</h2>")

  for (nm in names(check_out)) {
    x <- check_out[[nm]]
    html <- c(html, sprintf("<h3>%s <span class='muted'>(%s)</span></h3><p>%s</p>", x$check_id, if (!is.null(x$severity)) x$severity else "error", x$message))
    if (!is.null(x$issue_table) && nrow(x$issue_table) > 0) {
      cap <- utils::capture.output(print(x$issue_table, row.names = FALSE))
      html <- c(html, "<pre>", paste(cap, collapse = "\n"), "</pre>")
    }
  }

  html <- c(html, "</body></html>")
  writeLines(html, file)
  invisible(file)
}
