test_that("default config can be loaded", {
  cfg <- load_check_config()
  expect_true(is.list(cfg))
  expect_true("checks" %in% names(cfg))
  ids <- enabled_checks(cfg)
  expect_true(length(ids) > 0)
})

test_that("run_checks honors config enabled list", {
  x <- generate_dummy_adppk(study_type = "SAD", n_subj = 10, seed = 7)
  cfg <- list(
    time_semantics = list(time_unit = "hour", time_anchor = "first_dose", period_anchor = "first_dose_in_period"),
    checks = list(required_vars = list(enabled = TRUE, severity = "error"),
                  duplicates = list(enabled = FALSE, severity = "error"))
  )
  out <- run_checks(x$adppk, x$addose, cfg = cfg)
  expect_true("required_vars" %in% names(out))
  expect_false("duplicates" %in% names(out))
})

test_that("rule_versions and time semantics validation", {
  cfg <- load_check_config()
  rv <- rule_versions(cfg)
  expect_true("required_vars" %in% names(rv))
  v <- validate_time_semantics(cfg)
  expect_true(v$ok)
})
