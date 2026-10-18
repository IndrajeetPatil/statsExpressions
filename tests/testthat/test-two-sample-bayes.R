test_that("bayesian (independent samples t-test)", {
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

test_that("bayesian (paired t-test)", {
  set.seed(123)
  df <- two_sample_test(
    type = "bayes",
    data = bugs_long_two_groups,
    x = "condition",
    y = desire,
    paired = TRUE,
    bf.prior = 0.8
  )

  expect_equal(df$bf10[[1]], 40.36079, tolerance = 0.001)
  expect_snapshot(df[["expression"]])
})
