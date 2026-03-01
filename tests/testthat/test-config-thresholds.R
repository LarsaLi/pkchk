test_that("sampling deviation threshold from cfg affects result", {
  x <- generate_dummy_adppk(study_type = "SAD", n_subj = 8, seed = 13)
  x$adppk$NTIME <- x$adppk$ATPTN
  x$adppk$TIME <- x$adppk$ATPTN
  # inject 15% deviation in a few records
  idx <- seq_len(min(4, nrow(x$adppk)))
  x$adppk$TIME[idx] <- x$adppk$NTIME[idx] * 1.15

  cfg_loose <- list(checks = list(sampling_dev_10pct = list(enabled = TRUE, severity = "warn", threshold_pct = 20)))
  cfg_strict <- list(checks = list(sampling_dev_10pct = list(enabled = TRUE, severity = "warn", threshold_pct = 5)))

  out_loose <- run_checks(x$adppk, x$addose, cfg = cfg_loose)
  out_strict <- run_checks(x$adppk, x$addose, cfg = cfg_strict)

  expect_true(out_loose$sampling_dev_10pct$passed)
  expect_false(out_strict$sampling_dev_10pct$passed)
})

test_that("missing_by_evid threshold from cfg affects result", {
  x <- generate_dummy_adppk(study_type = "SAD", n_subj = 8, seed = 14)
  x$adppk$EVID <- 0
  x$adppk$DV <- x$adppk$AVAL
  # make ~30% missing in AGE
  idx <- seq_len(floor(0.3 * nrow(x$adppk)))
  x$adppk$AGE[idx] <- NA

  cfg_80 <- list(checks = list(missing_by_evid = list(enabled = TRUE, severity = "info", high_missing_threshold_pct = 80)))
  cfg_20 <- list(checks = list(missing_by_evid = list(enabled = TRUE, severity = "info", high_missing_threshold_pct = 20)))

  out_80 <- run_checks(x$adppk, x$addose, cfg = cfg_80)
  out_20 <- run_checks(x$adppk, x$addose, cfg = cfg_20)

  expect_true(out_80$missing_by_evid$n_issue < out_20$missing_by_evid$n_issue)
  expect_false(out_20$missing_by_evid$passed)
})
