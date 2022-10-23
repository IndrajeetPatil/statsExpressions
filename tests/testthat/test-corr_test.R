withr::local_options(list(tibble.width = Inf))

# parametric --------------------------------------------------------------

test_that(
  desc = "corr_test works - parametric",
  code = {
    # with NA
    set.seed(123)
    df1 <- corr_test(
      data = ggplot2::msleep,
      x = brainwt,
      y = sleep_rem,
      type = "parametric",
      k = 3,
      conf.level = 0.90
    )

    set.seed(123)
    expect_snapshot(select(df1, -expression))
    expect_snapshot(df1[["expression"]])

    # without NA
    set.seed(123)
    df2 <- corr_test(
      data = mtcars,
      x = wt,
      y = mpg,
      type = "p",
      k = 3L
    )

    set.seed(123)
    expect_snapshot(select(df2, -expression))
    expect_snapshot(df2[["expression"]])
  }
)

# robust ----------------------------------------------------------------

test_that(
  desc = "corr_test works - robust",
  code = {
    # with NA
    set.seed(123)
    df1 <- corr_test(
      data = ggplot2::msleep,
      x = brainwt,
      y = sleep_total,
      type = "r",
      k = 4,
      conf.level = 0.50
    )

    set.seed(123)
    expect_snapshot(select(df1, -expression))
    expect_snapshot(df1[["expression"]])

    # without NA
    set.seed(123)
    df2 <- corr_test(
      data = mtcars,
      x = wt,
      y = mpg,
      type = "r"
    )

    set.seed(123)
    expect_snapshot(select(df2, -expression))
    expect_snapshot(df2[["expression"]])
  }
)

# nonparametric ----------------------------------------------------------

test_that(
  desc = "corr_test works - nonparametric",
  code = {
    # with NA
    set.seed(123)
    df1 <- corr_test(
      data = ggplot2::msleep,
      x = brainwt,
      y = sleep_total,
      type = "np",
      k = 4L,
      conf.level = 0.50
    )

    set.seed(123)
    expect_snapshot(select(df1, -expression))
    expect_snapshot(df1[["expression"]])

    # without NA
    set.seed(123)
    df2 <- corr_test(
      data = mtcars,
      x = wt,
      y = mpg,
      type = "np",
      k = 4L
    )

    set.seed(123)
    expect_snapshot(select(df2, -expression))
    expect_snapshot(df2[["expression"]])
  }
)

# bayesian --------------------------------------

test_that(
  desc = "corr_test works - Bayesian",
  code = {
    # with NA
    set.seed(123)
    df1 <- corr_test(
      type = "bayes",
      data = ggplot2::msleep,
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
  }
)
