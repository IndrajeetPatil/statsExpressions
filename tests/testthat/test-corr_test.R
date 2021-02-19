# nonparametric ----------------------------------------------------------

test_that(
  desc = "corr_test works - nonparametric",
  code = {
    if (utils::packageVersion("correlation") > package_version("0.4.0")) {
      # `statsExpressions` output
      set.seed(123)
      using_function <-
        suppressWarnings(corr_test(
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
            widehat(rho)["Spearman"],
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
            "79"
          )
        )

      # testing overall call
      expect_identical(using_function$expression[[1]], expected)

      # `statsExpressions` output
      set.seed(123)
      using_function2 <-
        corr_test(
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
            widehat(rho)["Spearman"],
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
            "32"
          )
        )

      expect_identical(using_function2$expression[[1]], expected2)
    }
  }
)

# parametric --------------------------------------------------------------

test_that(
  desc = "corr_test works - parametric",
  code = {
    if (utils::packageVersion("correlation") > package_version("0.4.0")) {
      # `statsExpressions` output
      set.seed(123)
      using_function <-
        suppressWarnings(corr_test(
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
            "48"
          )
        )

      # testing overall call
      expect_identical(using_function$expression[[1]], expected)
    }
  }
)

# robust ----------------------------------------------------------------

test_that(
  desc = "corr_test works - robust",
  code = {
    if (utils::packageVersion("correlation") > package_version("0.4.0")) {
      # using function
      set.seed(123)
      using_function <-
        corr_test(
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
            "-4.8286",
            ", ",
            italic("p"),
            " = ",
            "1.172e-05",
            ", ",
            widehat(italic("r"))["Winsorized"],
            " = ",
            "-0.5491",
            ", CI"["50%"],
            " [",
            "-0.6106",
            ", ",
            "-0.4812",
            "]",
            ", ",
            italic("n")["pairs"],
            " = ",
            "56"
          )
        )

      # testing overall call
      expect_identical(using_function$expression[[1]], expected)
    }
  }
)

# dataframe -----------------------------------------------------------

test_that(
  desc = "dataframe",
  code = {
    expect_s3_class(
      corr_test(
        data = mtcars,
        x = mpg,
        y = wt
      ),
      "tbl_df"
    )
  }
)


if (packageVersion("parameters") > "0.11.0") {

  # bayes factor (correlation test) --------------------------------------

  test_that(
    desc = "bayes factor (correlation test) - without NAs",
    code = {
      skip_if(getRversion() < "4.0")

      # extracting results from where this function is implemented
      set.seed(123)
      df <-
        corr_test(
          type = "bayes",
          data = iris,
          y = Sepal.Length,
          x = "Sepal.Width"
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
          top.text = "huh"
        )

      expect_identical(
        subtitle1$expression[[1]],
        ggplot2::expr(
          atop(displaystyle("huh"), expr = paste(
            "log"["e"] * "(BF"["01"] * ") = " * "1.07" * ", ",
            widehat(rho)["Pearson"]^"posterior" * " = " * "-0.12" * ", ",
            "CI"["95%"]^"HDI" * " [" * "-0.24" * ", " * "0.02" * "], ",
            italic("r")["beta"]^"JZS" * " = " * "1.41"
          ))
        )
      )
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
          y = names(ggplot2::msleep)[10],
          x = "sleep_rem",
        )

      # check bayes factor values
      expect_equal(df$bf10, 0.6539296, tolerance = 0.001)

      set.seed(123)
      subtitle1 <-
        corr_test(
          type = "bayes",
          data = ggplot2::msleep,
          y = brainwt,
          x = sleep_rem,
          bf.prior = 0.8,
          conf.level = 0.99,
          k = 3
        )

      expect_identical(
        subtitle1$expression[[1]],
        ggplot2::expr(
          paste(
            "log"["e"] * "(BF"["01"] * ") = " * "0.487" * ", ",
            widehat(rho)["Pearson"]^"posterior" * " = " * "-0.210" * ", ",
            "CI"["99%"]^"HDI" * " [" * "-0.410" * ", " * "0.026" * "], ",
            italic("r")["beta"]^"JZS" * " = " * "1.250"
          )
        )
      )
    }
  )
}
