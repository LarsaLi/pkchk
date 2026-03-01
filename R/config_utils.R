#' Load check configuration from YAML
#'
#' @param path Optional path to YAML file. If NULL, use built-in default config.
#' @return A list containing check config.
#' @export
load_check_config <- function(path = NULL) {
  if (is.null(path)) {
    path <- system.file("config", "checks_default.yml", package = "pkchk")
  }
  if (!nzchar(path) || !file.exists(path)) {
    stop("Config file not found: ", path)
  }
  yaml::read_yaml(path)
}

#' Get enabled check ids from config
#'
#' @param cfg Config list returned by `load_check_config()`
#' @return character vector of enabled check ids.
#' @export
enabled_checks <- function(cfg) {
  ids <- names(cfg$checks)
  on <- vapply(cfg$checks, function(x) isTRUE(x$enabled), logical(1))
  ids[on]
}

#' Extract per-check severity overrides from config
#'
#' @param cfg Config list returned by `load_check_config()`
#' @return named character vector.
#' @export
severity_overrides <- function(cfg) {
  ids <- names(cfg$checks)
  sev <- vapply(cfg$checks, function(x) if (is.null(x$severity)) "error" else as.character(x$severity), character(1))
  stats::setNames(sev, ids)
}
