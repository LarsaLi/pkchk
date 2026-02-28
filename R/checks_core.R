#' Check required ADPPK variables
#' @param adppk ADPPK data frame.
#' @return List with check result.
#' @export
check_required_vars <- function(adppk) {
  req <- c("STUDYID", "USUBJID", "SUBJID", "PARAMCD", "AVAL", "AVALU", "POPPKFL")
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
#' @param adppk ADPPK data frame.
#' @return List with check result.
#' @export
check_poppk_consistency <- function(adppk) {
  subj_bad <- unique(adppk$USUBJID[adppk$POPPKFL != "Y"])
  rec_bad <- adppk[adppk$POPPKFL != "Y", c("USUBJID", "PARAMCD", "AVAL", "POPPKFL")]
  issues <- if (nrow(rec_bad) == 0) data.frame() else rec_bad
  list(
    check_id = "poppk_consistency",
    passed = nrow(rec_bad) == 0,
    n_issue = nrow(rec_bad),
    message = if (nrow(rec_bad) == 0) "All records in POPPK" else paste0(length(subj_bad), " subject(s), ", nrow(rec_bad), " record(s) not in POPPK"),
    issue_table = issues
  )
}

#' Check char-numeric mapping consistency (RACE vs RACEN)
#' @param adppk ADPPK data frame.
#' @return List with check result.
#' @export
check_char_num_mapping <- function(adppk) {
  if (!all(c("RACE", "RACEN") %in% names(adppk))) {
    return(list(
      check_id = "char_num_mapping",
      passed = FALSE,
      n_issue = 1,
      message = "RACE/RACEN missing",
      issue_table = data.frame(reason = "RACE/RACEN missing", stringsAsFactors = FALSE)
    ))
  }

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

#' Run selected checks
#' @param adppk ADPPK data frame.
#' @param addose ADDOSE data frame.
#' @param selected character vector of check ids.
#' @return List of check results.
#' @export
run_checks <- function(adppk, addose = data.frame(), selected = c("required_vars", "name_label_len", "pk_no_dose", "poppk_consistency", "char_num_mapping")) {
  out <- list()
  if ("required_vars" %in% selected) out[["required_vars"]] <- check_required_vars(adppk)
  if ("name_label_len" %in% selected) out[["name_label_len"]] <- check_name_label_len(adppk)
  if ("pk_no_dose" %in% selected) out[["pk_no_dose"]] <- check_pk_no_dose(adppk, addose)
  if ("poppk_consistency" %in% selected) out[["poppk_consistency"]] <- check_poppk_consistency(adppk)
  if ("char_num_mapping" %in% selected) out[["char_num_mapping"]] <- check_char_num_mapping(adppk)
  out
}

#' Available checks registry
#' @return data.frame
#' @export
checks_registry <- function() {
  data.frame(
    id = c("required_vars", "name_label_len", "pk_no_dose", "poppk_consistency", "char_num_mapping"),
    label = c(
      "Required/conditional variables",
      "Variable name <=8 and label <=40",
      "PK records without dose",
      "Subjects/records not in POPPK",
      "Character-numeric mapping consistency"
    ),
    stringsAsFactors = FALSE
  )
}
