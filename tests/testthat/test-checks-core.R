test_that("required var check passes for valid dummy", {
  x <- generate_dummy_adppk(study_type = "MAD", n_subj = 15, seed = 2)
  res <- check_required_vars(x$adppk)
  expect_true(res$passed)
  expect_equal(res$n_issue, 0)
})

test_that("char-num mapping check catches mismatch", {
  x <- generate_dummy_adppk(study_type = "SAD", n_subj = 20, seed = 3, inject_char_num_mismatch = TRUE)
  res <- check_char_num_mapping(x$adppk)
  expect_false(res$passed)
  expect_true(res$n_issue > 0)
})

test_that("run_checks returns selected checks", {
  x <- generate_dummy_adppk(study_type = "SAD", n_subj = 10, seed = 4)
  out <- run_checks(x$adppk, x$addose, selected = c("required_vars", "name_label_len"))
  expect_true(all(c("required_vars", "name_label_len") %in% names(out)))
  expect_false("pk_no_dose" %in% names(out))
})
