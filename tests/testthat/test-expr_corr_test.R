# nonparametric ----------------------------------------------------------

testthat::test_that(
  desc = "expr_corr_test works - nonparametric",
  code = {
    testthat::skip_if(getRversion() < "3.6")
    testthat::skip_on_cran()

    if (utils::packageVersion("correlation") > package_version("0.4.0")) {
      # `statsExpressions` output
      set.seed(123)
      using_function <-
        suppressWarnings(statsExpressions::expr_corr_test(
          data = dplyr::sample_frac(movies_long, 0.05),
          x = rating,
          y = "length",
          type = "nonparametric",
          k = 5,
          conf.level = 0.999
        ))

      # expected
      expected <-
        ggplot2::expr(
          paste(
            "log"["e"](italic("S")),
            " = ",
            "10.63231",
            ", ",
            italic("p"),
            " = ",
            "3.4438e-06",
            ", ",
            widehat(italic(rho))["Spearman"],
            " = ",
            "0.49546",
            ", CI"["99.9%"],
            " [",
            "0.15344",
            ", ",
            "0.73147",
            "]",
            ", ",
            italic("n")["pairs"],
            " = ",
            79L
          )
        )

      # testing overall call
      testthat::expect_identical(using_function, expected)

      # `statsExpressions` output
      set.seed(123)
      using_function2 <-
        statsExpressions::expr_corr_test(
          data = mtcars,
          x = names(mtcars)[6],
          y = mpg,
          type = "np"
        )

      expected2 <-
        ggplot2::expr(
          paste(
            "log"["e"](italic("S")),
            " = ",
            "9.24",
            ", ",
            italic("p"),
            " = ",
            "1.49e-11",
            ", ",
            widehat(italic(rho))["Spearman"],
            " = ",
            "-0.89",
            ", CI"["95%"],
            " [",
            "-0.94",
            ", ",
            "-0.77",
            "]",
            ", ",
            italic("n")["pairs"],
            " = ",
            32L
          )
        )

      testthat::expect_identical(using_function2, expected2)
    }
  }
)

# parametric --------------------------------------------------------------

testthat::test_that(
  desc = "expr_corr_test works - parametric",
  code = {
    testthat::skip_if(getRversion() < "3.6")
    testthat::skip_on_cran()

    if (utils::packageVersion("correlation") > package_version("0.4.0")) {
      # `statsExpressions` output
      set.seed(123)
      using_function <-
        suppressWarnings(statsExpressions::expr_corr_test(
          data = ggplot2::msleep,
          x = brainwt,
          y = sleep_rem,
          type = "parametric",
          k = 3,
          conf.level = 0.90
        ))

      # expected
      expected <-
        ggplot2::expr(
          paste(
            italic("t")["Student"],
            "(",
            "46",
            ") = ",
            "-1.539",
            ", ",
            italic("p"),
            " = ",
            "0.131",
            ", ",
            widehat(italic("r"))["Pearson"],
            " = ",
            "-0.221",
            ", CI"["90%"],
            " [",
            "-0.438",
            ", ",
            "0.020",
            "]",
            ", ",
            italic("n")["pairs"],
            " = ",
            48L
          )
        )

      # testing overall call
      testthat::expect_identical(using_function, expected)
    }
  }
)

# robust ----------------------------------------------------------------

testthat::test_that(
  desc = "expr_corr_test works - robust",
  code = {
    testthat::skip_if(getRversion() < "3.6")
    testthat::skip_on_cran()

    if (utils::packageVersion("correlation") > package_version("0.4.0")) {
      # using function
      set.seed(123)
      using_function <-
        statsExpressions::expr_corr_test(
          data = ggplot2::msleep,
          x = names(ggplot2::msleep)[10],
          y = "sleep_total",
          type = "r",
          k = 4,
          conf.level = .50
        )

      # expected
      expected <-
        ggplot2::expr(
          paste(
            italic("t")["Student"],
            "(",
            "54",
            ") = ",
            "-5.3621",
            ", ",
            italic("p"),
            " = ",
            "1.756e-06",
            ", ",
            widehat(italic(rho))["% bend"],
            " = ",
            "-0.5894",
            ", CI"["50%"],
            " [",
            "-0.6466",
            ", ",
            "-0.5257",
            "]",
            ", ",
            italic("n")["pairs"],
            " = ",
            56L
          )
        )

      # testing overall call
      testthat::expect_identical(using_function, expected)
    }
  }
)


# bayes ----------------------------------------------------------------

testthat::test_that(
  desc = "expr_corr_test works - bayes",
  code = {
    testthat::skip_if(getRversion() < "3.6")

    # using function
    set.seed(123)
    using_function <-
      statsExpressions::expr_corr_test(
        data = ggplot2::msleep,
        x = names(ggplot2::msleep)[10],
        y = sleep_rem,
        type = "bayes",
        k = 3
      )

    # expected
    set.seed(123)
    expected <-
      tidyBF::bf_corr_test(
        data = ggplot2::msleep,
        x = names(ggplot2::msleep)[10],
        y = sleep_rem,
        type = "bf",
        k = 3,
        output = "expression"
      )

    # testing overall call
    testthat::expect_identical(using_function, expected)
  }
)


# dataframe -----------------------------------------------------------

testthat::test_that(
  desc = "dataframe",
  code = {
    testthat::expect_s3_class(
      statsExpressions::expr_corr_test(
        data = mtcars,
        x = mpg,
        y = wt,
        output = "dataframe"
      ),
      "tbl_df"
    )
  }
)
