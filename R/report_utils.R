#' Convert check results to summary data frame
#' @param check_out Named list from `run_checks()`
#' @return data.frame
#' @export
checks_to_summary <- function(check_out) {
  do.call(rbind, lapply(check_out, function(x) {
    data.frame(
      check_id     = x$check_id,
      rule_version = if (!is.null(x$rule_version)) x$rule_version else "1.0.0",
      severity     = if (!is.null(x$severity)) x$severity else "error",
      status       = if (!is.null(x$status)) x$status else if (isTRUE(x$passed)) "pass" else "fail",
      passed       = x$passed,
      n_issue      = x$n_issue,
      message      = x$message,
      stringsAsFactors = FALSE
    )
  }))
}

#' Compute model readiness score from check summary
#'
#' @details
#' **Scoring formula:**
#' \enumerate{
#'   \item `base = 100 * pass_n / (pass_n + fail_n)` (skipped checks excluded from denominator)
#'   \item `score = max(0, base - blocker_n * 10)`
#' }
#' The 10-point penalty per failing model-blocker reflects that a single
#' structural issue (e.g. missing required variables) can make a dataset
#' entirely unusable for modeling regardless of how well other checks pass.
#'
#' **Status thresholds:**
#' \itemize{
#'   \item `BLOCKED` — any model_blocker check has status "fail"
#'   \item `READY`   — score >= 90 (no blockers)
#'   \item `REVIEW`  — score >= 70 (no blockers)
#'   \item `NOT_READY` — score < 70 (no blockers)
#' }
#'
#' @param summary_df Output of `checks_to_summary()`
#' @return list with score and counters.
#' @export
model_readiness_score <- function(summary_df) {
  if (is.null(summary_df) || nrow(summary_df) == 0) {
    return(list(score = 0, blockers = 0, fail = 0, pass = 0, skip = 0, status = "NO_DATA"))
  }
  pass_n    <- sum(summary_df$status == "pass",  na.rm = TRUE)
  fail_n    <- sum(summary_df$status == "fail",  na.rm = TRUE)
  skip_n    <- sum(summary_df$status == "skip",  na.rm = TRUE)
  blocker_n <- sum(summary_df$status == "fail" & summary_df$severity == "model_blocker", na.rm = TRUE)
  base   <- if ((pass_n + fail_n) == 0) 0 else round(100 * pass_n / (pass_n + fail_n), 1)
  score  <- max(0, base - blocker_n * 10)
  status <- if (blocker_n > 0) "BLOCKED" else if (score >= 90) "READY" else if (score >= 70) "REVIEW" else "NOT_READY"
  list(score = score, blockers = blocker_n, fail = fail_n, pass = pass_n, skip = skip_n, status = status)
}

#' Build issue table from check results
#' @param check_out Named list from `run_checks()`
#' @return data.frame
#' @export
checks_to_issues <- function(check_out) {
  suggestion_map <- c(
    required_vars   = "Add missing required ADPPK variables before modeling.",
    name_label_len  = "Shorten variable names/labels to standard limits.",
    pk_no_dose      = "Confirm dosing extraction and merge keys for affected subjects.",
    poppk_consistency = "Review POPPK inclusion rule and flag derivation logic.",
    char_num_mapping = "Rebuild controlled terminology map (char <-> numeric).",
    predose_time    = "Recalculate relative times using first dose anchor.",
    sampling_dev_10pct = "Check nominal/actual time derivation and sampling windows.",
    duplicates      = "Deduplicate by subject-time-event key after merges.",
    mdv_assignment  = "Re-derive MDV from DV/EVID and missingness rules.",
    amt_for_dose    = "Ensure AMT is populated for every EVID=1 record."
  )

  tabs <- lapply(check_out, function(x) {
    if (is.null(x$issue_table) || nrow(x$issue_table) == 0) return(NULL)
    sug_val <- suggestion_map[x$check_id]
    sug <- if (length(sug_val) == 0 || is.na(sug_val)) "Review source derivation and study-specific rule." else unname(sug_val)
    cbind(
      check_id   = x$check_id,
      severity   = if (!is.null(x$severity)) x$severity else "error",
      suggestion = sug,
      x$issue_table,
      stringsAsFactors = FALSE
    )
  })
  tabs <- Filter(Negate(is.null), tabs)
  if (length(tabs) == 0) return(data.frame(info = "No issues found", stringsAsFactors = FALSE))

  # rbind with fill to handle different issue-table schemas across checks
  all_cols <- unique(unlist(lapply(tabs, names)))
  tabs2 <- lapply(tabs, function(df) {
    miss <- setdiff(all_cols, names(df))
    if (length(miss) > 0) for (m in miss) df[[m]] <- NA
    df[, all_cols, drop = FALSE]
  })
  out <- do.call(rbind, tabs2)
  rownames(out) <- NULL
  out
}

# Internal helper: convert a data frame to an HTML <table> with proper escaping
.df_to_html_table <- function(df, max_rows = 200L) {
  if (is.null(df) || nrow(df) == 0) return("<p><em>No rows</em></p>")
  df <- utils::head(df, max_rows)
  esc <- function(x) gsub("&", "&amp;",
          gsub("<", "&lt;",
          gsub(">", "&gt;",
          gsub("\"", "&quot;",
          gsub("'", "&#39;", as.character(x))))))
  hdr <- paste0("<th>", esc(names(df)), "</th>", collapse = "")
  rows <- apply(df, 1, function(r) {
    paste0("<tr>", paste0("<td>", esc(r), "</td>", collapse = ""), "</tr>")
  })
  suffix <- if (nrow(df) == max_rows && nrow(df) < nrow(df) + 1) "" else
    if (max_rows < Inf) sprintf("<p><em>Showing first %d rows</em></p>", max_rows) else ""
  paste0("<table><thead><tr>", hdr, "</tr></thead><tbody>",
         paste(rows, collapse = ""), "</tbody></table>", suffix)
}

#' Generate HTML checklist report
#' @param adppk ADPPK data frame
#' @param check_out Named list from `run_checks()`
#' @param file Output html path
#' @param cfg Optional check config list.
#' @return Invisibly returns file path
#' @export
generate_check_report_html <- function(adppk, check_out, file, cfg = NULL) {
  res    <- checks_to_summary(check_out)
  pass_n <- sum(res$status == "pass", na.rm = TRUE)
  fail_n <- sum(res$status == "fail", na.rm = TRUE)
  skip_n <- sum(res$status == "skip", na.rm = TRUE)

  pkg_ver <- as.character(utils::packageVersion("pkchk"))
  # P3-6: use tryCatch only; do NOT check working-directory .git — that is
  # wrong in a deployed Shiny server context.
  git_sha <- tryCatch(
    suppressWarnings(system2("git", c("rev-parse", "--short", "HEAD"),
                              stdout = TRUE, stderr = FALSE)[1]),
    error = function(e) "NA"
  )
  if (length(git_sha) == 0 || is.na(git_sha)) git_sha <- "NA"

  esc <- function(x) gsub("&", "&amp;",
          gsub("<", "&lt;",
          gsub(">", "&gt;", as.character(x))))

  html <- c(
    "<!DOCTYPE html><html><head><meta charset='utf-8'><title>pkchk checklist report</title>",
    "<style>",
    "body{font-family:-apple-system,BlinkMacSystemFont,Segoe UI,Roboto,sans-serif;margin:24px;}",
    "table{border-collapse:collapse;width:100%;margin:12px 0;}",
    "th,td{border:1px solid #ddd;padding:6px;font-size:13px;text-align:left;}",
    "th{background:#f5f5f5;}",
    ".ok{color:#0a7d23;} .bad{color:#b00020;} .muted{color:#555;}",
    "</style></head><body>",
    sprintf("<h1>pkchk checklist report</h1><p class='muted'>Date: %s</p>", Sys.Date()),
    sprintf("<p><b>Package</b>: pkchk %s | <b>Git</b>: %s</p>", esc(pkg_ver), esc(git_sha)),
    sprintf(
      "<p><b>Records</b>: %s | <b>Subjects</b>: %s | <b>Pass</b>: <span class='ok'>%s</span> | <b>Fail</b>: <span class='bad'>%s</span> | <b>Skip</b>: %s</p>",
      nrow(adppk),
      if ("USUBJID" %in% names(adppk)) length(unique(adppk$USUBJID)) else NA,
      pass_n, fail_n, skip_n
    ),
    "<h2>Summary</h2>",
    # P2-7: proper HTML table (was using capture.output(print()))
    "<table><thead><tr><th>check_id</th><th>severity</th><th>status</th><th>n_issue</th><th>message</th></tr></thead><tbody>"
  )

  for (i in seq_len(nrow(res))) {
    row    <- res[i, ]
    status_cls <- if (identical(row$status, "pass")) "ok" else if (identical(row$status, "fail")) "bad" else "muted"
    html <- c(html, sprintf(
      "<tr><td>%s</td><td>%s</td><td class='%s'>%s</td><td>%s</td><td>%s</td></tr>",
      esc(row$check_id), esc(row$severity), status_cls, esc(row$status),
      esc(row$n_issue), esc(row$message)
    ))
  }
  html <- c(html, "</tbody></table>")

  if (!is.null(cfg) && !is.null(cfg$checks)) {
    html <- c(html,
      "<h2>Effective configuration</h2>",
      sprintf("<p>Profile: %s | Version: %s</p>",
              esc(ifelse(is.null(cfg$profile), "NA", cfg$profile)),
              esc(ifelse(is.null(cfg$version), "NA", cfg$version))),
      "<table><thead><tr><th>check_id</th><th>enabled</th><th>severity</th><th>rule_version</th><th>thresholds</th></tr></thead><tbody>"
    )
    for (cid in names(cfg$checks)) {
      cki     <- cfg$checks[[cid]]
      en      <- ifelse(is.null(cki$enabled), NA, cki$enabled)
      sev     <- ifelse(is.null(cki$severity), "error", cki$severity)
      rv      <- ifelse(is.null(cki$rule_version), "1.0.0", cki$rule_version)
      thr     <- cki[setdiff(names(cki), c("enabled", "severity", "rule_version"))]
      thr_txt <- if (length(thr) == 0) "" else paste(names(thr), unlist(thr), sep = "=", collapse = "; ")
      html <- c(html, sprintf("<tr><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td></tr>",
                              esc(cid), esc(en), esc(sev), esc(rv), esc(thr_txt)))
    }
    html <- c(html, "</tbody></table>")
  }

  html <- c(html, "<h2>Issue details</h2>")

  for (nm in names(check_out)) {
    x <- check_out[[nm]]
    sev_label <- if (!is.null(x$severity)) x$severity else "error"
    html <- c(html,
      sprintf("<h3>%s <span class='muted'>(%s)</span></h3><p>%s</p>",
              esc(x$check_id), esc(sev_label), esc(x$message))
    )
    if (!is.null(x$issue_table) && nrow(x$issue_table) > 0) {
      # P2-7: real HTML table with htmlEscape on all values; was using <pre>print()</pre>
      html <- c(html, .df_to_html_table(x$issue_table))
    }
  }

  html <- c(html, "</body></html>")
  writeLines(html, file)
  invisible(file)
}
