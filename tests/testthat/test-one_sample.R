test_that(
  desc = "one_sample_test parametric works",
  code = {
    options(tibble.width = Inf)
    skip_if(getRversion() < "4.0")

    # parametric -----------------------------------------------------------

    # Hedge's g and non-central
    set.seed(123)
    df1 <- one_sample_test(
      data = dplyr::sample_frac(movies_long, 0.05),
      x = length,
      test.value = 120,
      type = "p",
      k = 5
    )

    # Cohen's d and non-central
    set.seed(123)
    df2 <- suppressWarnings(
      one_sample_test(
        data = dplyr::sample_frac(movies_long, 0.05),
        x = length,
        test.value = 120,
        type = "p",
        effsize.type = "d",
        k = 4,
        conf.level = 0.90
      )
    )

    # testing all details
    set.seed(123)
    expect_snapshot(dplyr::select(df1, -expression))
    expect_snapshot(df1$expression[[1]])

    expect_snapshot(dplyr::select(df2, -expression))
    expect_snapshot(df2$expression[[1]])
  }
)

test_that(
  desc = "one_sample_test non-parametric works",
  code = {
    skip_if(getRversion() < "4.0")

    # non-parametric -----------------------------------------------------------

    # statsExpressions output
    set.seed(123)
    df1 <- suppressWarnings(one_sample_test(
      data = ToothGrowth,
      x = len,
      test.value = 20,
      type = "np",
      k = 4
    ))

    # statsExpressions output
    set.seed(123)
    df2 <- one_sample_test(
      data = ggplot2::msleep,
      x = names(ggplot2::msleep)[10],
      test.value = 0.25,
      type = "np",
      k = 4
    )

    # testing all details
    set.seed(123)
    expect_snapshot(dplyr::select(df1, -expression))
    expect_snapshot(df1$expression[[1]])

    expect_snapshot(dplyr::select(df2, -expression))
    expect_snapshot(df2$expression[[1]])
  }
)

test_that(
  desc = "one_sample_test robust works",
  code = {
    skip_if(getRversion() < "4.0")

    # robust -----------------------------------------------------------

    # statsExpressions output
    set.seed(123)
    df1 <- one_sample_test(
      data = anscombe,
      x = x1,
      test.value = 8,
      type = "r",
      k = 4,
      conf.level = 0.95 # TODO: change
    )

    # statsExpressions output
    set.seed(123)
    df2 <- one_sample_test(
      data = ggplot2::msleep,
      x = brainwt,
      test.value = 0.1,
      type = "r",
      k = 4,
      conf.level = 0.95 # TODO: change
    )

    # testing all details
    set.seed(123)
    expect_snapshot(dplyr::select(df1, -expression))
    expect_snapshot(df1$expression[[1]])

    expect_snapshot(dplyr::select(df2, -expression))
    expect_snapshot(df2$expression[[1]])
  }
)

test_that(
  desc = "one_sample_test bayes factor works",
  code = {
    skip_if(getRversion() < "4.0")

    # Bayesian -----------------------------------------------------------

    # extracting results from where this function is implemented
    set.seed(123)
    df_results <-
      one_sample_test(
        type = "bayes",
        data = iris,
        x = Petal.Length,
        y = NULL,
        test.value = 5.5,
        bf.prior = 0.99,
      )

    # check Bayes factor values
    expect_equal(df_results$bf10[[1]], 5.958171e+20, tolerance = 0.001)

    expect_snapshot(names(df_results))

    # extracting subtitle (without NA)
    set.seed(123)
    subtitle <- one_sample_test(
      type = "bayes",
      data = iris,
      x = Petal.Length,
      y = NULL,
      test.value = 5.5,
      bf.prior = 0.99,
      conf.level = 0.90
    )

    expect_snapshot(subtitle$expression[[1]])

    # extracting subtitle (with NA)
    set.seed(123)
    subtitle2 <- one_sample_test(
      type = "bayes",
      data = ggplot2::msleep,
      x = brainwt,
      y = NULL,
      test.value = 0.25,
      bf.prior = 0.9,
      k = 3,
      conf.method = "eti"
    )

    expect_snapshot(subtitle2$expression[[1]])
  }
)
