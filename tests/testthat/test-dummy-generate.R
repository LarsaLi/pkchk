test_that("generate_dummy_adppk returns expected structure", {
  x <- generate_dummy_adppk(study_type = "SAD", n_subj = 20, seed = 1)
  expect_true(is.list(x))
  expect_true(all(c("adppk", "addose") %in% names(x)))
  expect_true(is.data.frame(x$adppk))
  expect_true(is.data.frame(x$addose))
  expect_true(nrow(x$adppk) > 0)
  expect_true(length(unique(x$adppk$USUBJID)) == 20)
  expect_true(all(c("STUDYID", "USUBJID", "PARAMCD", "AVAL", "POPPKFL") %in% names(x$adppk)))
})


test_that("inject_missing_dose creates pk-no-dose findings", {
  x <- generate_dummy_adppk(study_type = "SAD", n_subj = 20, seed = 1, inject_missing_dose = TRUE)
  res <- check_pk_no_dose(x$adppk, x$addose)
  expect_false(res$passed)
  expect_true(res$n_issue >= 1)
})
