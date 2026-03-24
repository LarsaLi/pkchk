test_that("extdata split files are present and readable", {
  f_adppk <- system.file("extdata", "example_adppk_only.csv", package = "pkchk")
  f_addose <- system.file("extdata", "example_addose.csv", package = "pkchk")

  expect_true(nzchar(f_adppk))
  expect_true(nzchar(f_addose))

  adppk <- utils::read.csv(f_adppk, stringsAsFactors = FALSE)
  addose <- utils::read.csv(f_addose, stringsAsFactors = FALSE)

  expect_true(all(c("USUBJID", "PARAMCD", "AVAL", "ATPTN") %in% names(adppk)))
  expect_true(all(c("USUBJID", "DOSE", "ROUTE") %in% names(addose)))
  expect_true(nrow(adppk) > 0)
  expect_true(nrow(addose) > 0)
})
