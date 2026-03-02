test_that("event_ordering passes for generated dummy", {
  x <- generate_dummy_pk_package(study_type = "SAD", n_subj = 6, period_n = 2, seed = 123)
  res <- check_event_ordering(x$adppk)
  expect_true(res$passed)
})

test_that("obs_has_prior_dose catches missing dose context", {
  x <- generate_dummy_pk_package(study_type = "SAD", n_subj = 6, period_n = 1, seed = 123)
  d <- x$adppk
  d <- d[!(d$USUBJID == d$USUBJID[1] & d$EVID == 1), , drop = FALSE]
  res <- check_obs_has_prior_dose(d)
  expect_false(res$passed)
  expect_true(res$n_issue >= 1)
})

test_that("time_anchor_consistency catches shifted dose times", {
  x <- generate_dummy_pk_package(study_type = "SAD", n_subj = 5, period_n = 1, seed = 123)
  d <- x$adppk
  idx <- which(d$USUBJID == d$USUBJID[1] & d$EVID == 1)
  d$TIME[idx] <- d$TIME[idx] + 1
  res <- check_time_anchor_consistency(d)
  expect_false(res$passed)
})
