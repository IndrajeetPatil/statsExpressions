withr::local_options(list(tibble.width = Inf))

test_that(
  desc = "one_sample_test parametric works",
  code = {
    # parametric -------------------------------------------------

    # Hedge's g and non-central
    set.seed(123)
    df1 <- one_sample_test(
      data = sample_frac(movies_long, 0.05),
      x = length,
      test.value = 120,
      type = "p",
      k = 5
    )

    # Cohen's d and non-central
    set.seed(123)
    df2 <- suppressWarnings(
      one_sample_test(
        data = sample_frac(movies_long, 0.05),
        x = length,
        test.value = 120,
        type = "p",
        effsize.type = "d",
        k = 4,
        conf.level = 0.90
      )
    )

    set.seed(123)
    expect_snapshot(select(df1, -expression))
    expect_snapshot(df1[["expression"]])

    expect_snapshot(select(df2, -expression))
    expect_snapshot(df2[["expression"]])
  }
)

test_that(
  desc = "one_sample_test non-parametric works",
  code = {
    # non-parametric --------------------------------------------------

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
      data = msleep,
      x = names(msleep)[10],
      test.value = 0.25,
      type = "np",
      k = 4
    )

    set.seed(123)
    expect_snapshot(select(df1, -expression))
    expect_snapshot(df1[["expression"]])

    expect_snapshot(select(df2, -expression))
    expect_snapshot(df2[["expression"]])
  }
)

test_that(
  desc = "one_sample_test robust works",
  code = {
    # robust --------------------------------------------------

    # statsExpressions output
    set.seed(123)
    df1 <- one_sample_test(
      data = anscombe,
      x = x1,
      test.value = 8,
      type = "r",
      k = 4,
      conf.level = 0.90
    )

    # statsExpressions output
    set.seed(123)
    df2 <- one_sample_test(
      data = msleep,
      x = brainwt,
      test.value = 0.1,
      type = "r",
      k = 4,
      conf.level = 0.99
    )


    set.seed(123)
    expect_snapshot(select(df1, -expression))
    expect_snapshot(df1[["expression"]])

    expect_snapshot(select(df2, -expression))
    expect_snapshot(df2[["expression"]])
  }
)

test_that(
  desc = "one_sample_test bayesian works",
  code = {
    # Bayesian -----------------------------------------------

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
    df1 <- one_sample_test(
      type = "bayes",
      data = iris,
      x = Petal.Length,
      y = NULL,
      test.value = 5.5,
      bf.prior = 0.99,
      conf.level = 0.90
    )

    expect_snapshot(df1[["expression"]])

    # extracting subtitle (with NA)
    set.seed(123)
    df2 <- one_sample_test(
      type = "bayes",
      data = msleep,
      x = brainwt,
      y = NULL,
      test.value = 0.25,
      bf.prior = 0.9,
      k = 3,
      conf.method = "eti"
    )

    expect_snapshot(df2[["expression"]])
  }
)
