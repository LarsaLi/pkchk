test_that("generate_dummy_pk_package returns DM/EX/PC/ADPPK", {
  x <- generate_dummy_pk_package(study_type = "SAD", n_subj = 20, seed = 1)
  expect_true(is.list(x))
  expect_true(all(c("dm", "ex", "pc", "adppk", "addose") %in% names(x)))
  expect_true(is.data.frame(x$dm))
  expect_true(is.data.frame(x$ex))
  expect_true(is.data.frame(x$pc))
  expect_true(is.data.frame(x$adppk))
  expect_true(is.data.frame(x$addose))
  expect_true(nrow(x$adppk) > 0)
  expect_true(length(unique(x$adppk$USUBJID)) == 20)
  expect_true(all(c("STUDYID", "USUBJID", "PARAMCD", "AVAL", "POPPKFL", "EVID", "MDV", "DV", "AMT") %in% names(x$adppk)))
  expect_true(all(!is.na(x$adppk$ARM)))
  expect_true(all(c(0, 1) %in% unique(x$adppk$EVID)))
})


test_that("inject_missing_dose creates pk-no-dose findings", {
  x <- generate_dummy_adppk(study_type = "SAD", n_subj = 20, seed = 1, inject_missing_dose = TRUE)
  res <- check_pk_no_dose(x$adppk, x$addose)
  expect_false(res$passed)
  expect_true(res$n_issue >= 1)
})
