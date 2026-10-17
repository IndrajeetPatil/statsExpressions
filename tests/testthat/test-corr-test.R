# frequentist ------------------------------------------------------------

# `msleep` has `NA`s, `mtcars` doesn't
patrick::with_parameters_test_that(
  "corr_test works:",
  {
    # `exec()` inlines the column names, as if they were typed in the call
    df <- rlang::exec(
      corr_test,
      data = data,
      x = x,
      y = y,
      type = type,
      digits = digits,
      conf.level = conf.level
    )

    expect_snapshot(select(df, -expression))
    expect_snapshot(df[["expression"]])
  },
  .cases = dplyr::tibble(
    type = c("parametric", "p", "r", "r", "np", "np"),
    data = rep(list(msleep, mtcars), times = 3L),
    x = rep(c("brainwt", "wt"), times = 3L),
    y = c("sleep_rem", "mpg", "sleep_total", "mpg", "sleep_total", "mpg"),
    digits = c(3L, 3L, 4L, 2L, 4L, 4L),
    conf.level = c(0.90, 0.95, 0.50, 0.95, 0.50, 0.95),
    .test_name = paste(
      rep(c("parametric", "robust", "nonparametric"), each = 2L),
      rep(c("with NA", "without NA"), times = 3L),
      sep = ", "
    )
  )
)

# bayesian --------------------------------------

test_that("corr_test works - Bayesian", {
  # with NA
  set.seed(123)
  df1 <- corr_test(
    type = "bayes",
    data = msleep,
    y = brainwt,
    x = sleep_rem,
    bf.prior = 0.8,
    conf.level = 0.99
  )

  expect_equal(df1$bf10, 0.614, tolerance = 0.001)
  expect_snapshot(df1[["expression"]])

  # without NA
  set.seed(123)
  df2 <- corr_test(
    data = mtcars,
    x = wt,
    y = mpg,
    type = "bayes"
  )

  expect_snapshot(df2[["expression"]])
})
