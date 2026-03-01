#' Internal: apply check function with optional config item
#' @param fn Check function
#' @param args Named list of args for function
#' @param cfg_item Optional config list for this check
#' @return Check result list
run_check_with_cfg <- function(fn, args, cfg_item = NULL) {
  # Pass threshold-like parameters only when function supports them.
  fn_formals <- names(formals(fn))
  extra <- list()
  if (!is.null(cfg_item) && is.list(cfg_item)) {
    for (nm in names(cfg_item)) {
      if (nm %in% c("enabled", "severity")) next
      if (nm %in% fn_formals) extra[[nm]] <- cfg_item[[nm]]
    }
  }

  out <- do.call(fn, c(args, extra))

  # Unified status field for downstream reporting
  msg <- if (!is.null(out$message)) tolower(out$message) else ""
  is_skip <- isTRUE(out$passed) && grepl("skip|skipped|single study only|no .*available|no .*records", msg)
  out$status <- if (is_skip) "skip" else if (isTRUE(out$passed)) "pass" else "fail"
  out
}
