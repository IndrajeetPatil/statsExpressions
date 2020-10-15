
# parametric -----------------------------------------------------------

testthat::test_that(
  desc = "expr_t_onesample parametric works",
  code = {
    testthat::skip_if(getRversion() < "3.6")

    # Hedge's g and non-central
    set.seed(123)
    using_function1 <-
      statsExpressions::expr_t_onesample(
        data = dplyr::sample_frac(movies_long, 0.05),
        x = length,
        test.value = 120,
        type = "p",
        k = 5,
        messages = FALSE
      )

    set.seed(123)
    results1 <-
      ggplot2::expr(
        paste(
          italic("t")["Student"],
          "(",
          "78",
          ") = ",
          "-2.67496",
          ", ",
          italic("p"),
          " = ",
          "0.00910",
          ", ",
          widehat(italic("g"))["Hedge"],
          " = ",
          "-0.29805",
          ", CI"["95%"],
          " [",
          "-0.52379",
          ", ",
          "-0.07429",
          "]",
          ", ",
          italic("n")["obs"],
          " = ",
          79L
        )
      )

    # Cohen's d and non-central
    set.seed(123)
    using_function2 <-
      suppressWarnings(
        statsExpressions::expr_t_onesample(
          data = dplyr::sample_frac(movies_long, 0.05),
          x = "length",
          test.value = 120,
          type = "p",
          effsize.type = "d",
          k = 4,
          conf.level = 0.90,
          messages = FALSE
        )
      )

    set.seed(123)
    results2 <-
      ggplot2::expr(
        paste(
          italic("t")["Student"],
          "(",
          "78",
          ") = ",
          "-2.6750",
          ", ",
          italic("p"),
          " = ",
          "0.0091",
          ", ",
          widehat(italic("d"))["Cohen"],
          " = ",
          "-0.3010",
          ", CI"["90%"],
          " [",
          "-0.4924",
          ", ",
          "-0.1115",
          "]",
          ", ",
          italic("n")["obs"],
          " = ",
          79L
        )
      )

    # testing overall call
    testthat::expect_identical(using_function1, results1)
    testthat::expect_identical(using_function2, results2)
  }
)

# non-parametric -----------------------------------------------------------

testthat::test_that(
  desc = "expr_t_onesample non-parametric works",
  code = {
    testthat::skip_if(getRversion() < "3.6")

    # statsExpressions output
    set.seed(123)
    using_function <-
      statsExpressions::expr_t_onesample(
        data = ToothGrowth,
        x = len,
        test.value = 20,
        type = "np",
        k = 4,
        messages = TRUE
      )

    # expected output
    set.seed(123)
    results <-
      ggplot2::expr(
        paste(
          "log"["e"](italic("V")["Wilcoxon"]),
          " = ",
          "6.6247",
          ", ",
          italic("p"),
          " = ",
          "0.3227",
          ", ",
          widehat(italic("r")),
          " = ",
          "-0.1282",
          ", CI"["95%"],
          " [",
          "-0.3827",
          ", ",
          "0.1522",
          "]",
          ", ",
          italic("n")["obs"],
          " = ",
          60L
        )
      )

    # testing overall call
    testthat::expect_identical(using_function, results)

    # statsExpressions output
    set.seed(123)
    using_function2 <-
      statsExpressions::expr_t_onesample(
        data = ggplot2::msleep,
        x = names(ggplot2::msleep)[10],
        test.value = 0.25,
        type = "np",
        k = 4,
        messages = FALSE
      )

    results2 <-
      ggplot2::expr(
        paste(
          "log"["e"](italic("V")["Wilcoxon"]),
          " = ",
          "5.5683",
          ", ",
          italic("p"),
          " = ",
          "1.253e-05",
          ", ",
          widehat(italic("r")),
          " = ",
          "-0.5840",
          ", CI"["95%"],
          " [",
          "-0.8133",
          ", ",
          "-0.3768",
          "]",
          ", ",
          italic("n")["obs"],
          " = ",
          56L
        )
      )

    # testing overall call
    testthat::expect_identical(using_function2, results2)
  }
)


# robust -----------------------------------------------------------

testthat::test_that(
  desc = "expr_t_onesample robust works",
  code = {
    testthat::skip_if(getRversion() < "3.6")

    # statsExpressions output
    set.seed(123)
    using_function <-
      statsExpressions::expr_t_onesample(
        data = anscombe,
        x = "x1",
        test.value = 8,
        type = "r",
        k = 4,
        conf.level = 0.99,
        messages = TRUE
      )

    # expected output
    set.seed(123)
    results <-
      ggplot2::expr(
        paste(
          italic("M")["robust"],
          " = ",
          "9.0000",
          ", CI"["99%"],
          " [",
          "6.0128",
          ", ",
          "11.6299",
          "], ",
          italic("p"),
          " = ",
          "0.3000",
          ", ",
          italic("n")["obs"],
          " = ",
          11L
        )
      )

    # testing overall call
    testthat::expect_identical(using_function, results)
  }
)

# bayes factor -----------------------------------------------------------

testthat::test_that(
  desc = "expr_t_onesample bayes factor works",
  code = {
    testthat::skip_if(getRversion() < "3.6")

    # statsExpressions output
    set.seed(123)
    using_function <-
      statsExpressions::expr_t_onesample(
        data = anscombe,
        x = "x2",
        test.value = 8,
        type = "bf",
        messages = FALSE
      )

    # expected output
    set.seed(123)
    results <-
      tidyBF::bf_ttest(
        data = anscombe,
        x = "x2",
        test.value = 8,
        type = "bf",
        messages = FALSE,
        output = "h1"
      )$expr

    # testing overall call
    testthat::expect_identical(using_function, results)

    # statsExpressions output
    set.seed(123)
    using_function2 <-
      statsExpressions::expr_t_onesample(
        data = ggplot2::msleep,
        x = "brainwt",
        test.value = 0.25,
        type = "bf",
        k = 4,
        messages = FALSE
      )

    # expected result
    set.seed(123)
    results2 <-
      tidyBF::bf_ttest(
        data = ggplot2::msleep,
        x = "brainwt",
        test.value = 0.25,
        type = "bf",
        k = 4,
        messages = FALSE,
        output = "h1"
      )$expr

    # testing overall call
    testthat::expect_identical(using_function2, results2)
  }
)


# dataframe -----------------------------------------------------------

testthat::test_that(
  desc = "dataframe",
  code = {
    testthat::expect_is(
      statsExpressions::expr_t_onesample(
        data = dplyr::sample_frac(movies_long, 0.05),
        x = length,
        test.value = 120,
        output = "dataframe"
      ),
      "tbl_df"
    )
  }
)
