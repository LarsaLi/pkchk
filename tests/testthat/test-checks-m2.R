test_that("duplicate check identifies duplicates", {
  x <- generate_dummy_adppk(study_type = "SAD", n_subj = 8, seed = 11)
  d <- x$adppk
  d2 <- rbind(d, d[1, , drop = FALSE])
  res <- check_duplicates(d2)
  expect_false(res$passed)
  expect_true(res$n_issue >= 1)
})

test_that("fixed covariate check catches within-subject changes", {
  x <- generate_dummy_adppk(study_type = "SAD", n_subj = 8, seed = 12)
  d <- x$adppk
  sid <- d$USUBJID[1]
  idx <- which(d$USUBJID == sid)[1:2]
  d$SEX[idx[2]] <- ifelse(d$SEX[idx[1]] == "M", "F", "M")
  res <- check_fixed_covariates(d)
  expect_false(res$passed)
  expect_true(res$n_issue >= 1)
})
