test_that("end-to-end workflow: dummy -> checks -> summary -> report", {
  x <- generate_dummy_pk_package(study_type = "MAD", n_subj = 10, period_n = 2, seed = 101)
  cfg <- load_check_config()
  out <- run_checks(x$adppk, x$addose, selected = checks_registry()$id, cfg = cfg)

  s <- checks_to_summary(out)
  expect_true(nrow(s) > 0)
  expect_true(all(c("check_id", "severity", "status", "n_issue") %in% names(s)))

  m <- model_readiness_score(s)
  expect_true(is.numeric(m$score))
  expect_true(m$status %in% c("READY", "REVIEW", "NOT_READY", "BLOCKED"))

  f <- tempfile(fileext = ".html")
  generate_check_report_html(x$adppk, out, f, cfg = cfg)
  expect_true(file.exists(f))
})
