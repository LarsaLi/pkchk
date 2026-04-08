# helper ---------------------------------------------------------------
.check_missing_vars <- function(data, vars, check_id) {
  miss <- setdiff(vars, names(data))
  if (length(miss) == 0) return(NULL)
  list(
    check_id = check_id,
    passed = FALSE,
    n_issue = length(miss),
    message = paste("Missing required columns:", paste(miss, collapse = ", ")),
    issue_table = data.frame(variable = miss, stringsAsFactors = FALSE)
  )
}

.pass_result <- function(check_id, message = "OK") {
  list(check_id = check_id, passed = TRUE, n_issue = 0, message = message, issue_table = data.frame())
}

.skip_result <- function(check_id, message = "Skipped") {
  out <- .pass_result(check_id, message)
  out$status <- "skip"
  out
}

# checks ---------------------------------------------------------------

#' Check required ADPPK variables
#' @param adppk ADPPK data frame.
#' @return List with check result.
#' @export
check_required_vars <- function(adppk) {
  req <- c("STUDYID", "USUBJID", "EVID", "MDV", "DV", "AMT", "TIME", "CMT", "PARAMCD", "AVAL")
  miss <- setdiff(req, names(adppk))
  list(
    check_id = "required_vars",
    passed = length(miss) == 0,
    n_issue = length(miss),
    message = if (length(miss) == 0) "Required variables present" else paste("Missing:", paste(miss, collapse = ", ")),
    issue_table = if (length(miss) == 0) data.frame() else data.frame(variable = miss, stringsAsFactors = FALSE)
  )
}

#' Check variable name and label length constraints
#' @param adppk ADPPK data frame.
#' @return List with check result.
#' @export
check_name_label_len <- function(adppk) {
  bad_names <- names(adppk)[nchar(names(adppk)) > 8]
  labs <- vapply(adppk, function(x) {
    lb <- attr(x, "label")
    if (is.null(lb)) "" else as.character(lb)
  }, character(1))
  bad_labs <- names(labs)[nchar(labs) > 40]

  issues <- data.frame(
    type = c(rep("name_len", length(bad_names)), rep("label_len", length(bad_labs))),
    variable = c(bad_names, bad_labs),
    stringsAsFactors = FALSE
  )

  list(
    check_id = "name_label_len",
    passed = nrow(issues) == 0,
    n_issue = nrow(issues),
    message = if (nrow(issues) == 0) "Variable name/label constraints satisfied" else "Variable name/label length violations",
    issue_table = issues
  )
}

#' Check subjects with PK records but no dosing records
#' @param adppk ADPPK data frame.
#' @param addose Dose-level data frame.
#' @return List with check result.
#' @export
check_pk_no_dose <- function(adppk, addose) {
  miss <- .check_missing_vars(adppk, "USUBJID", "pk_no_dose")
  if (!is.null(miss)) return(miss)
  if (nrow(addose) == 0 || !("USUBJID" %in% names(addose))) {
    return(list(
      check_id = "pk_no_dose", passed = FALSE, n_issue = 1,
      message = "ADDOSE missing or no USUBJID", issue_table = data.frame(reason = "Need ADDOSE with USUBJID", stringsAsFactors = FALSE)
    ))
  }

  pk_subj <- unique(adppk$USUBJID)
  dose_subj <- unique(addose$USUBJID)
  bad <- setdiff(pk_subj, dose_subj)
  issues <- if (length(bad) == 0) data.frame() else data.frame(USUBJID = bad, reason = "PK record without dose", stringsAsFactors = FALSE)
  list(
    check_id = "pk_no_dose",
    passed = length(bad) == 0,
    n_issue = length(bad),
    message = if (length(bad) == 0) "All PK subjects have dose records" else "Subjects with PK but no dose found",
    issue_table = issues
  )
}

#' Check POPPK consistency at subject and record level
#'
#' Non-POPPK subjects (POPPKFL != "Y") are flagged. NA values in POPPKFL are
#' treated as non-violations to avoid false positives on dosing rows where the
#' flag is not applicable.
#'
#' @param adppk ADPPK data frame.
#' @return List with check result.
#' @export
check_poppk_consistency <- function(adppk) {
  miss <- .check_missing_vars(adppk, c("USUBJID", "POPPKFL", "PARAMCD", "AVAL"), "poppk_consistency")
  if (!is.null(miss)) return(miss)

  # P0-1: guard against NA — NA POPPKFL is not a violation
  flag <- !is.na(adppk$POPPKFL) & adppk$POPPKFL != "Y"
  subj_bad <- unique(adppk$USUBJID[flag])
  rec_bad  <- adppk[flag, c("USUBJID", "PARAMCD", "AVAL", "POPPKFL"), drop = FALSE]
  list(
    check_id = "poppk_consistency",
    passed = nrow(rec_bad) == 0,
    n_issue = nrow(rec_bad),
    message = if (nrow(rec_bad) == 0) "All records in POPPK" else paste0(length(subj_bad), " subject(s), ", nrow(rec_bad), " record(s) not in POPPK"),
    issue_table = if (nrow(rec_bad) == 0) data.frame() else rec_bad
  )
}

#' Check char-numeric mapping consistency (RACE vs RACEN)
#' @param adppk ADPPK data frame.
#' @return List with check result.
#' @export
check_char_num_mapping <- function(adppk) {
  miss <- .check_missing_vars(adppk, c("RACE", "RACEN"), "char_num_mapping")
  if (!is.null(miss)) return(miss)

  map <- unique(adppk[, c("RACE", "RACEN")])
  cnt <- stats::aggregate(RACEN ~ RACE, map, function(x) length(unique(x)))
  bad_race <- cnt$RACE[cnt$RACEN > 1]
  bad <- map[map$RACE %in% bad_race, , drop = FALSE]
  list(
    check_id = "char_num_mapping",
    passed = nrow(bad) == 0,
    n_issue = nrow(bad),
    message = if (nrow(bad) == 0) "RACE/RACEN mapping is 1:1" else "RACE/RACEN mapping inconsistency found",
    issue_table = bad
  )
}

#' Check potential character truncation
#' @param adppk ADPPK data frame.
#' @return List with check result.
#' @export
check_char_truncation <- function(adppk) {
  char_cols <- names(adppk)[vapply(adppk, is.character, logical(1))]
  if (length(char_cols) == 0) return(.pass_result("char_truncation", "No character columns"))

  # P2-3: list accumulator instead of rbind-in-loop
  acc <- lapply(char_cols, function(v) {
    x <- adppk[[v]]
    x <- x[!is.na(x)]
    if (length(x) == 0) return(NULL)
    max_len <- max(nchar(x))
    pct_at_max <- mean(nchar(x) == max_len)
    has_marker <- any(grepl("\\.\\.\\.$", x))
    if ((max_len >= 12 && pct_at_max > 0.3) || has_marker) {
      data.frame(variable = v, max_len = max_len, pct_at_max = round(pct_at_max, 3),
                 has_marker = has_marker, stringsAsFactors = FALSE)
    } else NULL
  })
  issues <- do.call(rbind, Filter(Negate(is.null), acc))
  if (is.null(issues)) issues <- data.frame()

  list(
    check_id = "char_truncation",
    passed = nrow(issues) == 0,
    n_issue = nrow(issues),
    message = if (nrow(issues) == 0) "No obvious truncation pattern" else "Potential truncation pattern detected",
    issue_table = issues
  )
}

#' Check fixed covariates do not change within subject
#' @param adppk ADPPK data frame.
#' @return List with check result.
#' @export
check_fixed_covariates <- function(adppk) {
  vars <- intersect(c("AGE", "SEX", "RACE", "RACEN", "WT", "HT"), names(adppk))
  miss <- .check_missing_vars(adppk, "USUBJID", "fixed_covariates")
  if (!is.null(miss)) return(miss)
  if (length(vars) == 0) return(.pass_result("fixed_covariates", "No covariates available"))

  # P1-1, P2-3: vectorized per-variable check with list accumulator
  acc <- lapply(vars, function(v) {
    tab <- stats::aggregate(adppk[[v]], by = list(USUBJID = adppk$USUBJID),
                            FUN = function(x) length(unique(x[!is.na(x)])))
    bad <- tab$USUBJID[tab$x > 1]
    if (length(bad) == 0) return(NULL)
    data.frame(variable = v, USUBJID = bad, stringsAsFactors = FALSE)
  })
  issues <- do.call(rbind, Filter(Negate(is.null), acc))
  if (is.null(issues)) issues <- data.frame()

  list(
    check_id = "fixed_covariates",
    passed = nrow(issues) == 0,
    n_issue = nrow(issues),
    message = if (nrow(issues) == 0) "Fixed covariates are stable" else "Fixed covariates changed within subject",
    issue_table = issues
  )
}

#' Check predose time <= 0
#' @param adppk ADPPK data frame.
#' @return List with check result.
#' @export
check_predose_time <- function(adppk) {
  time_var <- intersect(c("TIME", "NTIME", "ATPTN"), names(adppk))[1]
  if (is.na(time_var)) return(.check_missing_vars(adppk, "TIME/NTIME/ATPTN", "predose_time"))
  atpt_var <- intersect(c("ATPT", "TPT"), names(adppk))[1]
  if (is.na(atpt_var)) return(.pass_result("predose_time", "ATPT missing; skipped"))

  is_pre <- grepl("PRE|PREDOSE|T00|D\\d+_T00", toupper(as.character(adppk[[atpt_var]])))
  bad <- adppk[is_pre & as.numeric(adppk[[time_var]]) > 0, c("USUBJID", atpt_var, time_var), drop = FALSE]

  list(
    check_id = "predose_time",
    passed = nrow(bad) == 0,
    n_issue = nrow(bad),
    message = if (nrow(bad) == 0) "Predose times are <= 0" else "Predose time > 0 found",
    issue_table = bad
  )
}

#' Check nominal vs actual time deviation > threshold
#' @param adppk ADPPK data frame.
#' @param threshold_pct Allowed percent deviation threshold.
#' @return List with check result.
#' @export
check_sampling_deviation <- function(adppk, threshold_pct = 10) {
  miss <- .check_missing_vars(adppk, c("TIME", "NTIME"), "sampling_dev_10pct")
  if (!is.null(miss)) return(miss)
  nt <- as.numeric(adppk$NTIME)
  tt <- as.numeric(adppk$TIME)
  dev <- abs(tt - nt) / pmax(1e-8, abs(nt))
  bad <- which(dev > (as.numeric(threshold_pct) / 100) & !is.na(dev))
  issues <- if (length(bad) == 0) data.frame() else data.frame(USUBJID = adppk$USUBJID[bad], NTIME = nt[bad], TIME = tt[bad], dev_pct = round(dev[bad] * 100, 2), stringsAsFactors = FALSE)

  list(
    check_id = "sampling_dev_10pct",
    passed = length(bad) == 0,
    n_issue = length(bad),
    message = if (length(bad) == 0) sprintf("Sampling deviations within %s%%", threshold_pct) else sprintf("Sampling deviation >%s%% found", threshold_pct),
    issue_table = issues
  )
}

#' Check unusual values for dose or concentration
#' @param adppk ADPPK data frame.
#' @return List with check result.
#' @export
check_unexpected_values <- function(adppk) {
  nums <- intersect(c("DOSE", "AVAL"), names(adppk))
  if (length(nums) == 0) return(.pass_result("unexpected_values", "No numeric dose/concentration variables"))

  # P2-3: list accumulator
  acc <- lapply(nums, function(v) {
    x <- as.numeric(adppk[[v]])
    x_valid <- x[!is.na(x)]
    if (length(x_valid) < 5) return(NULL)
    q <- stats::quantile(x_valid, probs = c(0.25, 0.75))
    iqr <- q[2] - q[1]
    lo <- q[1] - 3 * iqr
    hi <- q[2] + 3 * iqr
    idx <- which(!is.na(x) & (x < lo | x > hi))
    if (length(idx) == 0) return(NULL)
    data.frame(variable = v, USUBJID = adppk$USUBJID[idx], value = x[idx], lo = lo, hi = hi,
               stringsAsFactors = FALSE)
  })
  issues <- do.call(rbind, Filter(Negate(is.null), acc))
  if (is.null(issues)) issues <- data.frame()

  list(
    check_id = "unexpected_values",
    passed = nrow(issues) == 0,
    n_issue = nrow(issues),
    message = if (nrow(issues) == 0) "No unexpected extreme values" else "Unexpected extreme dose/concentration values found",
    issue_table = issues
  )
}

#' Check outliers in covariates
#' @param adppk ADPPK data frame.
#' @return List with check result.
#' @export
check_covariate_outliers <- function(adppk) {
  covs <- intersect(c("AGE", "WT", "HT"), names(adppk))
  if (length(covs) == 0) return(.pass_result("covariate_outliers", "No covariates available"))

  # P2-3: list accumulator
  acc <- lapply(covs, function(v) {
    x <- as.numeric(adppk[[v]])
    q <- stats::quantile(x, probs = c(0.25, 0.75), na.rm = TRUE)
    iqr <- q[2] - q[1]
    lo <- q[1] - 3 * iqr
    hi <- q[2] + 3 * iqr
    idx <- which(!is.na(x) & (x < lo | x > hi))
    if (length(idx) == 0) return(NULL)
    data.frame(variable = v, USUBJID = adppk$USUBJID[idx], value = x[idx], stringsAsFactors = FALSE)
  })
  issues <- do.call(rbind, Filter(Negate(is.null), acc))
  if (is.null(issues)) issues <- data.frame()

  list(
    check_id = "covariate_outliers",
    passed = nrow(issues) == 0,
    n_issue = nrow(issues),
    message = if (nrow(issues) == 0) "No major covariate outliers" else "Covariate outliers found",
    issue_table = issues
  )
}

#' Check large nominal/actual deviation (absolute)
#' @param adppk ADPPK data frame.
#' @param abs_dev_threshold Absolute deviation threshold.
#' @return List with check result.
#' @export
check_nominal_actual_deviation <- function(adppk, abs_dev_threshold = 2) {
  miss <- .check_missing_vars(adppk, c("TIME", "NTIME"), "nominal_actual_deviation")
  if (!is.null(miss)) return(miss)
  dev <- abs(as.numeric(adppk$TIME) - as.numeric(adppk$NTIME))
  idx <- which(dev > as.numeric(abs_dev_threshold) & !is.na(dev))
  issues <- if (length(idx) == 0) data.frame() else data.frame(USUBJID = adppk$USUBJID[idx], TIME = adppk$TIME[idx], NTIME = adppk$NTIME[idx], abs_dev = dev[idx], stringsAsFactors = FALSE)
  list(
    check_id = "nominal_actual_deviation",
    passed = nrow(issues) == 0,
    n_issue = nrow(issues),
    message = if (nrow(issues) == 0) sprintf("Nominal/actual absolute deviations <= %s", abs_dev_threshold) else sprintf("Large absolute nominal/actual deviations > %s found", abs_dev_threshold),
    issue_table = issues
  )
}

#' Check nominal and actual consistency (ordering)
#' @param adppk ADPPK data frame.
#' @return List with check result.
#' @export
check_nominal_actual_consistency <- function(adppk) {
  miss <- .check_missing_vars(adppk, c("USUBJID", "TIME", "NTIME"), "nominal_actual_consistency")
  if (!is.null(miss)) return(miss)

  # P1-1: replace subject loop with split() + lapply
  grps <- split(seq_len(nrow(adppk)), adppk$USUBJID)
  bad_ids <- names(Filter(isTRUE, lapply(grps, function(idx) {
    d <- adppk[idx, , drop = FALSE]
    if (nrow(d) < 3) return(FALSE)
    o_n <- order(as.numeric(d$NTIME))
    o_t <- order(as.numeric(d$TIME))
    !identical(o_n, o_t)
  })))
  bad <- if (length(bad_ids) == 0) data.frame() else
    data.frame(USUBJID = bad_ids, reason = "Time ordering mismatch", stringsAsFactors = FALSE)

  list(
    check_id = "nominal_actual_consistency",
    passed = nrow(bad) == 0,
    n_issue = nrow(bad),
    message = if (nrow(bad) == 0) "Nominal/actual ordering consistent" else "Nominal/actual ordering mismatch",
    issue_table = bad
  )
}

#' Check missingness by EVID
#' @param adppk ADPPK data frame.
#' @param high_missing_threshold_pct Missingness threshold percent.
#' @return List with check result.
#' @export
check_missing_by_evid <- function(adppk, high_missing_threshold_pct = 20) {
  miss <- .check_missing_vars(adppk, "EVID", "missing_by_evid")
  if (!is.null(miss)) return(miss)

  vars <- names(adppk)
  ev <- unique(adppk$EVID)

  # P2-3: list accumulator; P2-4: issue_table returns only violating rows
  acc <- lapply(ev, function(e) {
    d <- adppk[adppk$EVID == e, , drop = FALSE]
    if (nrow(d) == 0) return(NULL)
    p <- vapply(d, function(x) mean(is.na(x)), numeric(1))
    data.frame(EVID = e, variable = vars, missing_pct = round(100 * p, 2), stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, Filter(Negate(is.null), acc))

  hi <- out[out$missing_pct > as.numeric(high_missing_threshold_pct), , drop = FALSE]
  list(
    check_id = "missing_by_evid",
    passed = nrow(hi) == 0,
    n_issue = nrow(hi),
    message = if (nrow(hi) == 0) sprintf("Missingness by EVID <= %s%%", high_missing_threshold_pct) else sprintf("High missingness (>%s%%) by EVID", high_missing_threshold_pct),
    issue_table = hi  # P2-4: only violation rows, not full table
  )
}

#' Identify duplicate records
#' @param adppk ADPPK data frame.
#' @return List with check result.
#' @export
check_duplicates <- function(adppk) {
  key <- intersect(c("USUBJID", "PARAMCD", "ADY", "ATPTN", "TIME"), names(adppk))
  if (length(key) < 2) return(.pass_result("duplicates", "Not enough key columns; skipped"))
  dups <- duplicated(adppk[, key, drop = FALSE]) | duplicated(adppk[, key, drop = FALSE], fromLast = TRUE)
  issues <- adppk[dups, key, drop = FALSE]
  list(
    check_id = "duplicates",
    passed = nrow(issues) == 0,
    n_issue = nrow(issues),
    message = if (nrow(issues) == 0) "No duplicate records" else "Duplicate records found",
    issue_table = issues
  )
}

#' Check expected ranges for common fields
#'
#' DOSE range is only checked for EVID=1 dosing records; observation rows
#' (EVID=0) legitimately carry DOSE=NA or DOSE=0.
#'
#' @param adppk ADPPK data frame.
#' @return List with check result.
#' @export
check_expected_ranges <- function(adppk) {
  # P0-4: list accumulator + EVID filter for DOSE + !is.na guards
  acc <- list()

  if ("AGE" %in% names(adppk)) {
    age_num <- as.numeric(adppk$AGE)
    idx <- which(!is.na(age_num) & (age_num < 0 | age_num > 120))
    if (length(idx) > 0)
      acc[[length(acc) + 1L]] <- data.frame(variable = "AGE", USUBJID = adppk$USUBJID[idx],
                                             value = adppk$AGE[idx], stringsAsFactors = FALSE)
  }

  if ("DOSE" %in% names(adppk)) {
    # Only check dose rows; obs rows have DOSE=NA/0 by design
    dose_idx <- if ("EVID" %in% names(adppk)) {
      which(as.numeric(adppk$EVID) == 1)
    } else {
      seq_len(nrow(adppk))
    }
    if (length(dose_idx) > 0) {
      d_vals <- as.numeric(adppk$DOSE[dose_idx])
      bad <- dose_idx[which(!is.na(d_vals) & (d_vals <= 0 | d_vals > 1e6))]
      if (length(bad) > 0)
        acc[[length(acc) + 1L]] <- data.frame(variable = "DOSE", USUBJID = adppk$USUBJID[bad],
                                               value = adppk$DOSE[bad], stringsAsFactors = FALSE)
    }
  }

  if ("AVAL" %in% names(adppk)) {
    aval_num <- as.numeric(adppk$AVAL)
    idx <- which(!is.na(aval_num) & aval_num < 0)
    if (length(idx) > 0)
      acc[[length(acc) + 1L]] <- data.frame(variable = "AVAL", USUBJID = adppk$USUBJID[idx],
                                             value = adppk$AVAL[idx], stringsAsFactors = FALSE)
  }

  issues <- do.call(rbind, Filter(Negate(is.null), acc))
  if (is.null(issues)) issues <- data.frame()

  list(
    check_id = "expected_ranges",
    passed = nrow(issues) == 0,
    n_issue = nrow(issues),
    message = if (nrow(issues) == 0) "Expected ranges look good" else "Out-of-range values found",
    issue_table = issues
  )
}

#' BLOQ in middle of profile
#' @param adppk ADPPK data frame.
#' @return List with check result.
#' @export
check_bloq_middle <- function(adppk) {
  miss <- .check_missing_vars(adppk, c("USUBJID", "PARAMCD", "BLQFL", "ATPTN"), "bloq_middle")
  if (!is.null(miss)) return(miss)

  # P1-1, P3-1: replace string-key loop with split()
  grps <- split(seq_len(nrow(adppk)), list(adppk$USUBJID, adppk$PARAMCD), drop = TRUE)
  acc <- lapply(grps, function(idx) {
    ord <- idx[order(as.numeric(adppk$ATPTN[idx]))]
    d <- adppk[ord, , drop = FALSE]
    f <- toupper(as.character(d$BLQFL)) == "Y"
    if (sum(f, na.rm = TRUE) == 0) return(NULL)
    first <- which(!f)[1]
    last  <- utils::tail(which(!f), 1)
    if (!is.na(first) && !is.na(last) && first < last &&
        any(f[seq(first, last)], na.rm = TRUE)) {
      data.frame(USUBJID = d$USUBJID[1], PARAMCD = d$PARAMCD[1],
                 reason = "BLOQ in middle", stringsAsFactors = FALSE)
    } else NULL
  })
  bad <- do.call(rbind, Filter(Negate(is.null), acc))
  if (is.null(bad)) bad <- data.frame()

  list(
    check_id = "bloq_middle",
    passed = nrow(bad) == 0,
    n_issue = nrow(bad),
    message = if (nrow(bad) == 0) "No BLOQ-in-middle pattern" else "BLOQ in middle of profile detected",
    issue_table = bad
  )
}

#' Abnormally high predose concentrations
#' @param adppk ADPPK data frame.
#' @param predose_ratio_threshold Predose to subject max concentration ratio threshold.
#' @return List with check result.
#' @export
check_high_predose <- function(adppk, predose_ratio_threshold = 0.2) {
  miss <- .check_missing_vars(adppk, c("USUBJID", "AVAL", "ATPTN"), "high_predose")
  if (!is.null(miss)) return(miss)
  pre_idx <- which(as.numeric(adppk$ATPTN) <= 1)
  if (length(pre_idx) == 0) return(.pass_result("high_predose", "No predose-like records"))

  mx <- stats::aggregate(AVAL ~ USUBJID, adppk, max)
  names(mx)[2] <- "MAXAVAL"
  d <- merge(adppk[pre_idx, c("USUBJID", "AVAL", "ATPTN")], mx, by = "USUBJID", all.x = TRUE)
  bad <- d[!is.na(d$AVAL) & !is.na(d$MAXAVAL) & d$MAXAVAL > 0 &
             d$AVAL > as.numeric(predose_ratio_threshold) * d$MAXAVAL, , drop = FALSE]

  list(
    check_id = "high_predose",
    passed = nrow(bad) == 0,
    n_issue = nrow(bad),
    message = if (nrow(bad) == 0) sprintf("No predose concentrations above %.2f of subject max", predose_ratio_threshold) else sprintf("Predose concentrations above %.2f of subject max found", predose_ratio_threshold),
    issue_table = bad
  )
}

#' AMT present for dosing records
#' @param adppk ADPPK data frame.
#' @return List with check result.
#' @export
check_amt_for_dose <- function(adppk) {
  miss <- .check_missing_vars(adppk, c("EVID", "AMT"), "amt_for_dose")
  if (!is.null(miss)) return(miss)
  idx <- which(as.numeric(adppk$EVID) == 1 & (is.na(adppk$AMT) | as.numeric(adppk$AMT) <= 0))
  issues <- if (length(idx) == 0) data.frame() else adppk[idx, intersect(c("USUBJID", "EVID", "AMT", "ADY"), names(adppk)), drop = FALSE]
  list(
    check_id = "amt_for_dose",
    passed = length(idx) == 0,
    n_issue = length(idx),
    message = if (length(idx) == 0) "AMT is valid for dosing records" else "AMT invalid for dosing records",
    issue_table = issues
  )
}

#' MDV assignment consistency
#' @param adppk ADPPK data frame.
#' @return List with check result.
#' @export
check_mdv_assignment <- function(adppk) {
  miss <- .check_missing_vars(adppk, c("MDV", "DV", "EVID"), "mdv_assignment")
  if (!is.null(miss)) return(miss)
  evid <- as.numeric(adppk$EVID)
  mdv <- as.numeric(adppk$MDV)

  # BLQ samples are commonly retained with DV present while MDV stays 1.
  blq_flag <- rep(FALSE, nrow(adppk))
  for (nm in c("BLQFL", "BLQFN")) {
    if (nm %in% names(adppk)) {
      x <- adppk[[nm]]
      blq_flag <- blq_flag | (toupper(as.character(x)) %in% c("Y", "1", "TRUE"))
    }
  }

  idx <- which(
    (is.na(adppk$DV) & mdv != 1) |
      (!is.na(adppk$DV) & mdv != 0 & evid == 0 & !blq_flag) |
      (evid == 1 & mdv != 1)
  )
  issues <- if (length(idx) == 0) data.frame() else adppk[idx, intersect(c("USUBJID", "DV", "MDV", "EVID", "ADY"), names(adppk)), drop = FALSE]
  list(
    check_id = "mdv_assignment",
    passed = length(idx) == 0,
    n_issue = length(idx),
    message = if (length(idx) == 0) "MDV assignment consistent with DV" else "MDV assignment issues found",
    issue_table = issues
  )
}

#' EVID=4 appears once per period
#' @param adppk ADPPK data frame.
#' @return List with check result.
#' @export
check_evid4_once <- function(adppk) {
  miss <- .check_missing_vars(adppk, c("USUBJID", "EVID"), "evid4_once")
  if (!is.null(miss)) return(miss)
  period_var <- if ("APERIOD" %in% names(adppk)) "APERIOD" else if ("PERIOD" %in% names(adppk)) "PERIOD" else if ("ADY" %in% names(adppk)) "ADY" else NA_character_
  if (is.na(period_var)) return(.pass_result("evid4_once", "No period variable; skipped"))

  d <- adppk[as.numeric(adppk$EVID) == 4, c("USUBJID", period_var), drop = FALSE]
  if (nrow(d) == 0) return(.pass_result("evid4_once", "No EVID=4 records"))

  # P0-2: use table() to correctly count per subject-period; aggregate had a
  # column-name collision bug where PERIOD_ was used as both group and response.
  cnt <- as.data.frame(table(USUBJID = d$USUBJID, PERIOD = d[[period_var]]))
  bad <- cnt[cnt$Freq > 1, , drop = FALSE]

  list(
    check_id = "evid4_once",
    passed = nrow(bad) == 0,
    n_issue = nrow(bad),
    message = if (nrow(bad) == 0) "EVID=4 frequency acceptable" else "EVID=4 appears multiple times in period",
    issue_table = bad
  )
}

#' Actual times increase sequentially after first dose
#' @param adppk ADPPK data frame.
#' @return List with check result.
#' @export
check_time_sequential <- function(adppk) {
  miss <- .check_missing_vars(adppk, c("USUBJID", "TIME"), "time_sequential")
  if (!is.null(miss)) return(miss)

  # P1-1: replace for loop with split() + lapply
  grps <- split(as.numeric(adppk$TIME), adppk$USUBJID)
  bad_ids <- names(Filter(isTRUE, lapply(grps, function(t) {
    t <- t[!is.na(t)]
    length(t) >= 2 && any(diff(t) < 0)
  })))
  bad <- if (length(bad_ids) == 0) data.frame() else
    data.frame(USUBJID = bad_ids, stringsAsFactors = FALSE)

  list(
    check_id = "time_sequential",
    passed = nrow(bad) == 0,
    n_issue = nrow(bad),
    message = if (nrow(bad) == 0) "Times are sequential" else "Non-sequential times found",
    issue_table = bad
  )
}

#' Check event ordering at identical subject-time points
#' @param adppk ADPPK data frame.
#' @return List with check result.
#' @export
check_event_ordering <- function(adppk) {
  miss <- .check_missing_vars(adppk, c("USUBJID", "TIME", "EVID"), "event_ordering")
  if (!is.null(miss)) return(miss)

  # P1-1: replace for loop with split() + lapply
  key <- paste(adppk$USUBJID, as.numeric(adppk$TIME))
  grps <- split(seq_len(nrow(adppk)), key)

  acc <- lapply(grps, function(idx) {
    if (length(idx) < 2) return(NULL)
    evids <- adppk$EVID[idx]
    if (!any(evids == 1) || !any(evids == 0)) return(NULL)
    i_dose <- min(which(evids == 1))
    i_obs  <- min(which(evids == 0))
    if (i_obs < i_dose) {
      data.frame(USUBJID = adppk$USUBJID[idx[1]], TIME = adppk$TIME[idx[1]],
                 reason = "EVID=0 appears before EVID=1 at same TIME", stringsAsFactors = FALSE)
    } else NULL
  })
  bad <- do.call(rbind, Filter(Negate(is.null), acc))
  if (is.null(bad)) bad <- data.frame()

  list(
    check_id = "event_ordering",
    passed = nrow(bad) == 0,
    n_issue = nrow(bad),
    message = if (nrow(bad) == 0) "Event ordering valid at same TIME" else "Event ordering issues at same TIME",
    issue_table = bad
  )
}

#' Check every observation has a prior dose within subject-period
#' @param adppk ADPPK data frame.
#' @return List with check result.
#' @export
check_obs_has_prior_dose <- function(adppk) {
  miss <- .check_missing_vars(adppk, c("USUBJID", "EVID", "TIME"), "obs_has_prior_dose")
  if (!is.null(miss)) return(miss)
  d <- adppk
  if (!"APERIOD" %in% names(d)) d$APERIOD <- 1L

  # P1-1: replace double for loop with split() + lapply
  grps <- split(seq_len(nrow(d)), paste(d$USUBJID, d$APERIOD))

  acc <- lapply(grps, function(idx) {
    x <- d[idx, , drop = FALSE]
    t_dose <- as.numeric(x$TIME[x$EVID == 1])
    t_obs  <- as.numeric(x$TIME[x$EVID == 0])
    if (length(t_obs) == 0) return(NULL)
    sid <- x$USUBJID[1]
    p   <- x$APERIOD[1]
    if (length(t_dose) == 0)
      return(data.frame(USUBJID = sid, APERIOD = p,
                        reason = "No dose records in period", stringsAsFactors = FALSE))
    if (any(t_obs < min(t_dose, na.rm = TRUE), na.rm = TRUE))
      return(data.frame(USUBJID = sid, APERIOD = p,
                        reason = "Observation before first dose in period", stringsAsFactors = FALSE))
    NULL
  })
  bad <- do.call(rbind, Filter(Negate(is.null), acc))
  if (is.null(bad)) bad <- data.frame()

  list(
    check_id = "obs_has_prior_dose",
    passed = nrow(bad) == 0,
    n_issue = nrow(bad),
    message = if (nrow(bad) == 0) "All observations have prior dose context" else "Observation records without proper dose context",
    issue_table = bad
  )
}

#' Check dose time anchor consistency by subject-period
#' @param adppk ADPPK data frame.
#' @return List with check result.
#' @export
check_time_anchor_consistency <- function(adppk) {
  miss <- .check_missing_vars(adppk, c("USUBJID", "EVID", "TIME"), "time_anchor_consistency")
  if (!is.null(miss)) return(miss)
  d <- adppk
  if (!"APERIOD" %in% names(d)) d$APERIOD <- 1L

  # P1-1: replace double for loop with split() + lapply on dose rows only
  dose_rows <- d[!is.na(d$EVID) & as.numeric(d$EVID) == 1, , drop = FALSE]
  if (nrow(dose_rows) == 0) return(.pass_result("time_anchor_consistency", "No dosing records"))

  grps <- split(seq_len(nrow(dose_rows)), paste(dose_rows$USUBJID, dose_rows$APERIOD))
  acc <- lapply(grps, function(idx) {
    x <- dose_rows[idx, , drop = FALSE]
    min_time <- suppressWarnings(min(as.numeric(x$TIME), na.rm = TRUE))
    if (is.finite(min_time) && abs(min_time) > 1e-8) {
      data.frame(USUBJID = x$USUBJID[1], APERIOD = x$APERIOD[1],
                 min_dose_time = min_time, reason = "First dose TIME is not zero",
                 stringsAsFactors = FALSE)
    } else NULL
  })
  bad <- do.call(rbind, Filter(Negate(is.null), acc))
  if (is.null(bad)) bad <- data.frame()

  list(
    check_id = "time_anchor_consistency",
    passed = nrow(bad) == 0,
    n_issue = nrow(bad),
    message = if (nrow(bad) == 0) "Dose time anchor consistent (first dose TIME=0)" else "Dose time anchor inconsistencies found",
    issue_table = bad
  )
}

#' Align variable types/formats across studies
#'
#' Returns a skip result for single-study datasets (check is not applicable).
#'
#' @param adppk ADPPK data frame.
#' @return List with check result.
#' @export
check_cross_study_alignment <- function(adppk) {
  miss <- .check_missing_vars(adppk, "STUDYID", "cross_study_alignment")
  if (!is.null(miss)) return(miss)
  st <- unique(adppk$STUDYID)
  # P3-3: single-study is not applicable (skip), not a pass
  if (length(st) < 2) return(.skip_result("cross_study_alignment", "Single study only"))

  vars <- names(adppk)
  # P2-3: list accumulator
  acc <- lapply(vars, function(v) {
    cls <- tapply(adppk[[v]], adppk$STUDYID, function(x) class(x)[1])
    if (length(unique(cls)) > 1) {
      data.frame(variable = v, details = paste(names(cls), cls, collapse = "; "),
                 stringsAsFactors = FALSE)
    } else NULL
  })
  issues <- do.call(rbind, Filter(Negate(is.null), acc))
  if (is.null(issues)) issues <- data.frame()

  list(
    check_id = "cross_study_alignment",
    passed = nrow(issues) == 0,
    n_issue = nrow(issues),
    message = if (nrow(issues) == 0) "Cross-study variable types aligned" else "Cross-study type/format misalignment found",
    issue_table = issues
  )
}

#' Standardize category and numeric attributes
#' @param adppk ADPPK data frame.
#' @return List with check result.
#' @export
check_standardized_values <- function(adppk) {
  # P2-3: list accumulator; P0-4 style: add !is.na guards
  acc <- list()
  if ("SEX" %in% names(adppk)) {
    bad <- which(!is.na(adppk$SEX) & !toupper(trimws(adppk$SEX)) %in% c("M", "F", "U", "UNKNOWN"))
    if (length(bad) > 0)
      acc[[length(acc) + 1L]] <- data.frame(variable = "SEX", USUBJID = adppk$USUBJID[bad],
                                             value = adppk$SEX[bad], stringsAsFactors = FALSE)
  }
  if ("RACE" %in% names(adppk)) {
    bad <- which(!is.na(adppk$RACE) & grepl("^\\s|\\s$", adppk$RACE))
    if (length(bad) > 0)
      acc[[length(acc) + 1L]] <- data.frame(variable = "RACE", USUBJID = adppk$USUBJID[bad],
                                             value = adppk$RACE[bad], stringsAsFactors = FALSE)
  }
  if ("DOSE" %in% names(adppk)) {
    dose_num <- as.numeric(adppk$DOSE)
    bad <- which(!is.na(dose_num) & round(dose_num, 6) != dose_num)
    if (length(bad) > 0)
      acc[[length(acc) + 1L]] <- data.frame(variable = "DOSE", USUBJID = adppk$USUBJID[bad],
                                             value = adppk$DOSE[bad], stringsAsFactors = FALSE)
  }
  issues <- do.call(rbind, Filter(Negate(is.null), acc))
  if (is.null(issues)) issues <- data.frame()

  list(
    check_id = "standardized_values",
    passed = nrow(issues) == 0,
    n_issue = nrow(issues),
    message = if (nrow(issues) == 0) "Categorical/numeric values appear standardized" else "Potential non-standardized values found",
    issue_table = issues
  )
}

# orchestration --------------------------------------------------------

#' Run selected checks
#'
#' Each check is wrapped in tryCatch so a single failing check never prevents
#' the remaining checks from running.  Severity is derived from the dispatch
#' registry defaults and can be overridden via the YAML config.
#'
#' @param adppk ADPPK data frame.
#' @param addose ADDOSE data frame.
#' @param selected Character vector of check ids.
#' @param cfg Optional check config list from `load_check_config()`.
#' @return Named list of check results.
#' @export
run_checks <- function(adppk, addose = data.frame(), selected = checks_registry()$id, cfg = NULL) {
  # P2-1/P2-2: data-driven dispatch table (replaces 27 hard-coded if statements)
  # P1-7: default_sev is the single source of truth for severity (YAML overrides below)
  dispatch <- list(
    required_vars              = list(fn = check_required_vars,              args = "adppk",        ts_dep = FALSE, default_sev = "model_blocker"),
    name_label_len             = list(fn = check_name_label_len,             args = "adppk",        ts_dep = FALSE, default_sev = "error"),
    pk_no_dose                 = list(fn = check_pk_no_dose,                 args = "adppk_addose", ts_dep = FALSE, default_sev = "model_blocker"),
    poppk_consistency          = list(fn = check_poppk_consistency,          args = "adppk",        ts_dep = FALSE, default_sev = "error"),
    char_num_mapping           = list(fn = check_char_num_mapping,           args = "adppk",        ts_dep = FALSE, default_sev = "error"),
    char_truncation            = list(fn = check_char_truncation,            args = "adppk",        ts_dep = FALSE, default_sev = "warn"),
    fixed_covariates           = list(fn = check_fixed_covariates,           args = "adppk",        ts_dep = FALSE, default_sev = "warn"),
    predose_time               = list(fn = check_predose_time,               args = "adppk",        ts_dep = TRUE,  default_sev = "model_blocker"),
    sampling_dev_10pct         = list(fn = check_sampling_deviation,         args = "adppk",        ts_dep = TRUE,  default_sev = "warn"),
    unexpected_values          = list(fn = check_unexpected_values,          args = "adppk",        ts_dep = FALSE, default_sev = "warn"),
    covariate_outliers         = list(fn = check_covariate_outliers,         args = "adppk",        ts_dep = FALSE, default_sev = "warn"),
    nominal_actual_deviation   = list(fn = check_nominal_actual_deviation,   args = "adppk",        ts_dep = TRUE,  default_sev = "warn"),
    nominal_actual_consistency = list(fn = check_nominal_actual_consistency, args = "adppk",        ts_dep = TRUE,  default_sev = "warn"),
    missing_by_evid            = list(fn = check_missing_by_evid,            args = "adppk",        ts_dep = FALSE, default_sev = "info"),
    duplicates                 = list(fn = check_duplicates,                 args = "adppk",        ts_dep = FALSE, default_sev = "model_blocker"),
    expected_ranges            = list(fn = check_expected_ranges,            args = "adppk",        ts_dep = FALSE, default_sev = "error"),
    bloq_middle                = list(fn = check_bloq_middle,                args = "adppk",        ts_dep = FALSE, default_sev = "warn"),
    high_predose               = list(fn = check_high_predose,               args = "adppk",        ts_dep = FALSE, default_sev = "warn"),
    amt_for_dose               = list(fn = check_amt_for_dose,               args = "adppk",        ts_dep = FALSE, default_sev = "model_blocker"),
    mdv_assignment             = list(fn = check_mdv_assignment,             args = "adppk",        ts_dep = FALSE, default_sev = "model_blocker"),
    evid4_once                 = list(fn = check_evid4_once,                 args = "adppk",        ts_dep = FALSE, default_sev = "warn"),
    time_sequential            = list(fn = check_time_sequential,            args = "adppk",        ts_dep = TRUE,  default_sev = "model_blocker"),
    event_ordering             = list(fn = check_event_ordering,             args = "adppk",        ts_dep = TRUE,  default_sev = "model_blocker"),
    obs_has_prior_dose         = list(fn = check_obs_has_prior_dose,         args = "adppk",        ts_dep = TRUE,  default_sev = "model_blocker"),
    time_anchor_consistency    = list(fn = check_time_anchor_consistency,    args = "adppk",        ts_dep = TRUE,  default_sev = "warn"),
    cross_study_alignment      = list(fn = check_cross_study_alignment,      args = "adppk",        ts_dep = FALSE, default_sev = "warn"),
    standardized_values        = list(fn = check_standardized_values,        args = "adppk",        ts_dep = FALSE, default_sev = "info")
  )

  ts_valid <- list(ok = TRUE, message = "ok")
  if (!is.null(cfg)) {
    selected <- intersect(selected, enabled_checks(cfg))
    ts_valid <- validate_time_semantics(cfg)
  }

  cfg_item <- function(id) {
    if (is.null(cfg) || is.null(cfg$checks) || is.null(cfg$checks[[id]])) return(NULL)
    cfg$checks[[id]]
  }

  out <- list()

  for (id in selected) {
    entry <- dispatch[[id]]
    if (is.null(entry)) next  # unknown id — skip silently

    # Skip time-dependent checks when time semantics are invalid
    if (isTRUE(entry$ts_dep) && !isTRUE(ts_valid$ok)) {
      out[[id]] <- .skip_result(id, paste("Skipped:", ts_valid$message))
      next
    }

    fn_args <- if (entry$args == "adppk_addose") {
      list(adppk = adppk, addose = addose)
    } else {
      list(adppk = adppk)
    }

    # P1-6: per-check error isolation — one bad check never kills all results
    out[[id]] <- tryCatch(
      run_check_with_cfg(entry$fn, fn_args, cfg_item(id)),
      error = function(e) {
        list(
          check_id    = id,
          passed      = FALSE,
          n_issue     = NA_integer_,
          message     = paste("Check error:", conditionMessage(e)),
          issue_table = data.frame(),
          status      = "error"
        )
      }
    )
  }

  # P1-7/P2-6: severity applied exactly once, here.
  # YAML config overrides the dispatch default; no duplicate sev_map vector.
  for (nm in names(out)) {
    entry   <- dispatch[[nm]]
    def_sev <- if (!is.null(entry)) entry$default_sev else "error"
    ci      <- cfg_item(nm)
    out[[nm]]$severity <- if (!is.null(ci) && !is.null(ci$severity)) ci$severity else def_sev
  }

  out
}

#' Available checks registry
#' @return data.frame of check ids and labels.
#' @export
checks_registry <- function() {
  data.frame(
    id = c(
      "required_vars", "name_label_len", "pk_no_dose", "poppk_consistency", "char_num_mapping",
      "char_truncation", "fixed_covariates", "predose_time", "sampling_dev_10pct", "unexpected_values",
      "covariate_outliers", "nominal_actual_deviation", "nominal_actual_consistency", "missing_by_evid", "duplicates",
      "expected_ranges", "bloq_middle", "high_predose", "amt_for_dose", "mdv_assignment",
      "evid4_once", "time_sequential", "event_ordering", "obs_has_prior_dose", "time_anchor_consistency",
      "cross_study_alignment", "standardized_values"
    ),
    label = c(
      "Required/conditional variables",
      "Variable names <=8 and labels <=40",
      "PK records without dosing records",
      "Subjects/records not in POPPK",
      "1:1 char-numeric mapping (RACE/RACEN)",
      "No character truncation patterns",
      "Fixed covariates unchanged within subject",
      "Predose sampling time <= 0",
      "Sampling deviation >10% vs nominal",
      "Unexpected dose/concentration values",
      "Covariate outliers",
      "Large nominal-actual absolute deviations",
      "Nominal/actual ordering consistency",
      "Missingness by EVID",
      "Duplicate records",
      "Expected ranges (ID/dose/time/age)",
      "BLOQ in middle of profile",
      "Abnormally high predose concentrations",
      "AMT present for dosing records",
      "MDV assignment vs DV",
      "EVID=4 once per period",
      "Actual times increase sequentially",
      "Dose before observation at same time",
      "Every observation has prior dose context",
      "First dose TIME anchor consistency (TIME=0)",
      "Cross-study variable alignment",
      "Standardized categorical/numeric values"
    ),
    stringsAsFactors = FALSE
  )
}
