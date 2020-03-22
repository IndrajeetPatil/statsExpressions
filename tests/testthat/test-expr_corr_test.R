# nonparametric ----------------------------------------------------------

testthat::test_that(
  desc = "expr_corr_test works - nonparametric",
  code = {
    testthat::skip_if(getRversion() < "3.6")

    # ggstatsplot output
    set.seed(123)
    using_function <-
      suppressWarnings(statsExpressions::expr_corr_test(
        data = dplyr::sample_frac(movies_long, 0.05),
        x = rating,
        y = "length",
        type = "nonparametric",
        k = 5,
        conf.level = 0.999,
        nboot = 50,
        messages = TRUE
      ))

    # expected
    expected <-
      ggplot2::expr(
        paste(
          NULL,
          "log"["e"](italic("S")),
          " = ",
          "10.63231",
          ", ",
          italic("p"),
          " = ",
          "< 0.001",
          ", ",
          widehat(italic(rho))["Spearman"],
          " = ",
          "0.49546",
          ", CI"["99.9%"],
          " [",
          "0.26846",
          ", ",
          "0.73574",
          "]",
          ", ",
          italic("n")["pairs"],
          " = ",
          79L
        )
      )

    # testing overall call
    testthat::expect_identical(using_function, expected)
  }
)

# parametric --------------------------------------------------------------

testthat::test_that(
  desc = "expr_corr_test works - parametric",
  code = {
    testthat::skip_if(getRversion() < "3.6")

    # ggstatsplot output
    set.seed(123)
    using_function <-
      suppressWarnings(statsExpressions::expr_corr_test(
        data = ggplot2::msleep,
        x = brainwt,
        y = sleep_rem,
        type = "parametric",
        k = 3,
        conf.level = 0.90,
        conf.type = "bca",
        nboot = 25,
        messages = TRUE
      ))

    # expected
    expected <-
      ggplot2::expr(
        paste(
          NULL,
          italic("t"),
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
)

# robust ----------------------------------------------------------------

testthat::test_that(
  desc = "expr_corr_test works - robust",
  code = {
    testthat::skip_if(getRversion() < "3.6")

    # using function
    set.seed(123)
    using_function <-
      suppressWarnings(statsExpressions::expr_corr_test(
        data = ggplot2::msleep,
        x = "brainwt",
        y = "sleep_total",
        type = "r",
        k = 4,
        conf.level = .50,
        conf.type = "basic",
        nboot = 25,
        messages = TRUE
      ))

    # expected
    expected <-
      ggplot2::expr(
        paste(
          NULL,
          italic("t"),
          "(",
          "54",
          ") = ",
          "-5.0929",
          ", ",
          italic("p"),
          " = ",
          "< 0.001",
          ", ",
          widehat(italic(rho))["pb"],
          " = ",
          "-0.5696",
          ", CI"["50%"],
          " [",
          "-0.6289",
          ", ",
          "-0.5037",
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
)


# bayes ----------------------------------------------------------------

testthat::test_that(
  desc = "expr_corr_test works - bayes",
  code = {
    testthat::skip_if(getRversion() < "3.6")

    # using function
    set.seed(123)
    using_function <-
      suppressWarnings(statsExpressions::expr_corr_test(
        data = ggplot2::msleep,
        x = "brainwt",
        y = sleep_rem,
        type = "bf",
        k = 3,
        messages = FALSE
      ))

    # expected
    expected <-
      ggplot2::expr(atop(
        displaystyle(NULL),
        expr = paste(
          "In favor of alternative: ",
          "log"["e"],
          "(BF"["10"],
          ") = ",
          "-0.425",
          ", ",
          italic("r")["Cauchy"]^"JZS",
          " = ",
          "0.707"
        )
      ))

    # testing overall call
    testthat::expect_identical(using_function, expected)
  }
)
