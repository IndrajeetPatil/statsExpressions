test_that(
  desc = "corr_test works - nonparametric",
  code = {
    options(tibble.width = Inf)

    # nonparametric ----------------------------------------------------------

    # `{statsExpressions}` output
    set.seed(123)
    df1 <- suppressWarnings(corr_test(
      data = sample_frac(movies_long, 0.05),
      x = rating,
      y = length,
      type = "nonparametric",
      k = 5,
      conf.level = 0.999
    ))

    # testing all details
    set.seed(123)
    expect_snapshot(select(df1, -expression))
    expect_snapshot(as.character(df1$expression[[1]]))

    # `{statsExpressions}` output
    set.seed(123)
    df2 <- corr_test(
      data = mtcars,
      x = wt,
      y = mpg,
      type = "np"
    )

    # testing all details
    set.seed(123)
    expect_snapshot(select(df2, -expression))
    expect_snapshot(as.character(df2$expression[[1]]))
  }
)

test_that(
  desc = "corr_test works - parametric",
  code = {
    # parametric --------------------------------------------------------------

    # `{statsExpressions}` output
    set.seed(123)
    df <- suppressWarnings(corr_test(
      data = ggplot2::msleep,
      x = brainwt,
      y = sleep_rem,
      type = "parametric",
      k = 3,
      conf.level = 0.90
    ))

    # testing all details
    set.seed(123)
    expect_snapshot(select(df, -expression))
    expect_snapshot(as.character(df$expression[[1]]))
  }
)

test_that(
  desc = "corr_test works - robust",
  code = {
    # robust ----------------------------------------------------------------

    # using function
    set.seed(123)
    df <- corr_test(
      data = ggplot2::msleep,
      x = brainwt,
      y = sleep_total,
      type = "r",
      k = 4,
      conf.level = 0.50
    )

    # testing all details
    set.seed(123)
    expect_snapshot(select(df, -expression))
    expect_snapshot(as.character(df$expression[[1]]))
  }
)

test_that(
  desc = "bayes factor (correlation test) - without NAs",
  code = {
    # bayes factor (correlation test) --------------------------------------

    skip_if(getRversion() < "4.0")

    # extracting results from where this function is implemented
    set.seed(123)
    df <- corr_test(
      type = "bayes",
      data = iris,
      y = Sepal.Length,
      x = Sepal.Width
    )

    # check bayes factor values
    expect_equal(df$bf10, 0.3445379, tolerance = 0.001)

    set.seed(123)
    subtitle1 <-
      corr_test(
        type = "bayes",
        data = iris,
        y = Sepal.Length,
        x = Sepal.Width,
        top.text = "huh is duh"
      )

    expect_snapshot(as.character(subtitle1$expression[[1]]))
  }
)

test_that(
  desc = "bayes factor (correlation test) - with NAs",
  code = {
    skip_if(getRversion() < "4.0")

    # extracting results from where this function is implemented
    set.seed(123)
    df <-
      corr_test(
        type = "bayes",
        data = ggplot2::msleep,
        y = brainwt,
        x = sleep_rem,
      )

    # check bayes factor values
    expect_equal(df$bf10, 0.6539296, tolerance = 0.001)

    set.seed(123)
    subtitle1 <- corr_test(
      type = "bayes",
      data = ggplot2::msleep,
      y = brainwt,
      x = sleep_rem,
      bf.prior = 0.8,
      conf.level = 0.99
    )

    expect_snapshot(as.character(subtitle1$expression[[1]]))
  }
)
