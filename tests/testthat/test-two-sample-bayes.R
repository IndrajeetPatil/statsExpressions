test_that(desc = "bayesian (independent samples t-test)", code = {
  set.seed(123)
  df <- two_sample_test(
    type = "bayes",
    data = ToothGrowth,
    x = supp,
    y = len,
    paired = FALSE,
    conf.level = 0.99
  )

  expect_snapshot(df[["expression"]])
})

test_that(desc = "bayesian (paired t-test)", code = {
  dat_tidy <- dplyr::filter(bugs_long, condition %in% c("HDLF", "HDHF"))

  set.seed(123)
  df <- two_sample_test(
    type = "bayes",
    data = dat_tidy,
    x = "condition",
    y = desire,
    paired = TRUE,
    bf.prior = 0.8
  )

  expect_equal(df$bf10[[1]], 40.36079, tolerance = 0.001)
  expect_snapshot(df[["expression"]])
})

test_that(desc = "works with subject id", code = {
  expect_subject_id_invariance(
    two_sample_test,
    filter(data_with_subid, condition %in% c(1, 5)),
    type = "bayes"
  )
})
