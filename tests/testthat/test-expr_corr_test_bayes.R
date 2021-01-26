# bayes factor (correlation test) --------------------------------------

test_that(
  desc = "bayes factor (correlation test) - without NAs",
  code = {
    skip_if(getRversion() < "4.0")
    skip_on_cran()

    # extracting results from where this function is implemented
    set.seed(123)
    df <-
      expr_corr_test(
        type = "bayes",
        data = iris,
        y = Sepal.Length,
        x = "Sepal.Width",
        output = "dataframe"
      )

    # check bayes factor values
    expect_equal(df$bf10, 0.3445379, tolerance = 0.001)

    set.seed(123)
    subtitle1 <-
      expr_corr_test(
        type = "bayes",
        data = iris,
        y = Sepal.Length,
        x = Sepal.Width,
        output = "expression",
        top.text = "huh"
      )

    expect_identical(
      subtitle1,
      ggplot2::expr(
        atop(displaystyle("huh"), expr = paste(
          "log"["e"] * "(BF"["01"] * ") = " * "1.07" * ", ",
          widehat(rho)["median"]^"posterior" * " = " * "-0.12" * ", ",
          "CI"["95%"]^"HDI" * " [" * "-0.24" * ", " * "0.02" * "], ",
          italic("r")["Cauchy"]^"JZS" * " = " * "0.71"
        ))
      )
    )
  }
)

test_that(
  desc = "bayes factor (correlation test) - with NAs",
  code = {
    skip_if(getRversion() < "4.0")
    skip_on_cran()

    # extracting results from where this function is implemented
    set.seed(123)
    df <-
      expr_corr_test(
        type = "bayes",
        data = ggplot2::msleep,
        y = names(ggplot2::msleep)[10],
        x = "sleep_rem",
        output = "dataframe"
      )

    # check bayes factor values
    expect_equal(df$bf10, 0.6539296, tolerance = 0.001)

    set.seed(123)
    subtitle1 <-
      expr_corr_test(
        type = "bayes",
        data = ggplot2::msleep,
        y = brainwt,
        x = sleep_rem,
        output = "subtitle",
        bf.prior = 0.8,
        centrality = "mean",
        conf.level = 0.99,
        k = 3
      )

    expect_identical(
      subtitle1,
      ggplot2::expr(
        paste(
          "log"["e"] * "(BF"["01"] * ") = " * "0.487" * ", ",
          widehat(rho)["median"]^"posterior" * " = " * "-0.210" * ", ",
          "CI"["99%"]^"HDI" * " [" * "-0.410" * ", " * "0.026" * "], ",
          italic("r")["Cauchy"]^"JZS" * " = " * "0.800"
        )
      )
    )
  }
)
