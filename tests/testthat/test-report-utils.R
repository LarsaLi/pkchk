test_that("checks_to_summary returns severity column", {
  x <- generate_dummy_adppk(study_type = "SAD", n_subj = 10, seed = 8)
  out <- run_checks(x$adppk, x$addose, selected = c("required_vars", "duplicates"))
  s <- checks_to_summary(out)
  expect_true(all(c("check_id", "severity", "passed", "n_issue", "message") %in% names(s)))
  expect_equal(nrow(s), 2)
})

test_that("generate_check_report_html writes a file", {
  x <- generate_dummy_adppk(study_type = "SAD", n_subj = 10, seed = 9)
  out <- run_checks(x$adppk, x$addose, selected = c("required_vars"))
  f <- tempfile(fileext = ".html")
  generate_check_report_html(x$adppk, out, f)
  expect_true(file.exists(f))
  txt <- readLines(f, warn = FALSE)
  expect_true(any(grepl("pkchk checklist report", txt)))
})
