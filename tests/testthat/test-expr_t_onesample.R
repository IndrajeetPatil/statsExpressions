
# parametric -----------------------------------------------------------

test_that(
  desc = "expr_t_onesample parametric works",
  code = {
    skip_if(getRversion() < "4.0")

    # Hedge's g and non-central
    set.seed(123)
    using_function1 <-
      expr_t_onesample(
        data = dplyr::sample_frac(movies_long, 0.05),
        x = length,
        test.value = 120,
        type = "p",
        k = 5
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
          "79"
        )
      )

    # Cohen's d and non-central
    set.seed(123)
    using_function2 <-
      suppressWarnings(
        expr_t_onesample(
          data = dplyr::sample_frac(movies_long, 0.05),
          x = "length",
          test.value = 120,
          type = "p",
          effsize.type = "d",
          k = 4,
          conf.level = 0.90
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
          "79"
        )
      )

    # testing overall call
    expect_identical(using_function1, results1)
    expect_identical(using_function2, results2)
  }
)

# non-parametric -----------------------------------------------------------

test_that(
  desc = "expr_t_onesample non-parametric works",
  code = {
    skip_if(getRversion() < "4.0")

    # statsExpressions output
    set.seed(123)
    using_function <-
      suppressWarnings(expr_t_onesample(
        data = ToothGrowth,
        x = len,
        test.value = 20,
        type = "np",
        k = 4
      ))

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
          widehat(italic("r"))["biserial"]^"rank",
          " = ",
          "-0.1486",
          ", CI"["95%"],
          " [",
          "-0.4584",
          ", ",
          "0.1206",
          "]",
          ", ",
          italic("n")["obs"],
          " = ",
          "60"
        )
      )

    # testing overall call
    expect_identical(using_function, results)

    # statsExpressions output
    set.seed(123)
    using_function2 <-
      expr_t_onesample(
        data = ggplot2::msleep,
        x = names(ggplot2::msleep)[10],
        test.value = 0.25,
        type = "np",
        k = 4
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
          widehat(italic("r"))["biserial"]^"rank",
          " = ",
          "-0.6717",
          ", CI"["95%"],
          " [",
          "-0.9052",
          ", ",
          "-0.4177",
          "]",
          ", ",
          italic("n")["obs"],
          " = ",
          "56"
        )
      )

    # testing overall call
    expect_identical(using_function2, results2)
  }
)


# robust -----------------------------------------------------------

test_that(
  desc = "expr_t_onesample robust works",
  code = {
    skip_if(getRversion() < "4.0")

    # statsExpressions output
    set.seed(123)
    using_function <-
      expr_t_onesample(
        data = anscombe,
        x = "x1",
        test.value = 8,
        type = "r",
        k = 4,
        conf.level = 0.99
      )

    # expected output
    set.seed(123)
    results <-
      ggplot2::expr(
        paste(
          italic("t")["bootstrapped"],
          " = ",
          "0.8748",
          ", ",
          italic("p"),
          " = ",
          "0.2500",
          ", ",
          widehat(mu)["trimmed"],
          " = ",
          "9.0000",
          ", CI"["99%"],
          " [",
          "4.4493",
          ", ",
          "13.5507",
          "]",
          ", ",
          italic("n")["obs"],
          " = ",
          "11"
        )
      )

    # testing overall call
    expect_identical(using_function, results)

    # statsExpressions output
    set.seed(123)
    using_function2 <-
      expr_t_onesample(
        data = ggplot2::msleep,
        x = brainwt,
        test.value = 0.1,
        type = "r",
        k = 4,
        conf.level = 0.90
      )

    # expected output
    set.seed(123)
    results2 <-
      ggplot2::expr(
        paste(
          italic("t")["bootstrapped"],
          " = ",
          "-1.4272",
          ", ",
          italic("p"),
          " = ",
          "0.2000",
          ", ",
          widehat(mu)["trimmed"],
          " = ",
          "0.0660",
          ", CI"["90%"],
          " [",
          "0.0201",
          ", ",
          "0.1119",
          "]",
          ", ",
          italic("n")["obs"],
          " = ",
          "56"
        )
      )

    # testing overall call
    expect_identical(using_function2, results2)
  }
)

# bayes factor -----------------------------------------------------------

test_that(
  desc = "expr_t_onesample bayes factor works",
  code = {
    skip_if(getRversion() < "4.0")

    # extracting results from where this function is implemented
    set.seed(123)
    df_results <-
      expr_t_onesample(
        type = "bayes",
        data = iris,
        x = Petal.Length,
        y = NULL,
        test.value = 5.5,
        bf.prior = 0.99,
        output = "dataframe"
      )

    # check Bayes factor values
    expect_equal(df_results$bf10[[1]], 5.958171e+20, tolerance = 0.001)

    # extracting subtitle (without NA)
    set.seed(123)
    subtitle <-
      expr_t_onesample(
        type = "bayes",
        data = iris,
        x = "Petal.Length",
        y = NULL,
        test.value = 5.5,
        bf.prior = 0.99,
        output = "expression",
        centrality = "mean",
        conf.level = 0.90
      )

    expect_type(subtitle, "language")

    expect_identical(
      subtitle,
      ggplot2::expr(
        paste(
          "log"["e"] * "(BF"["01"] * ") = " * "-47.84" * ", ",
          widehat(italic(delta))["median"]^"posterior" * " = " * "1.76" * ", ",
          "CI"["90%"]^"HDI" * " [" * "1.52" * ", " * "1.99" * "], ",
          italic("r")["Cauchy"]^"JZS" * " = " * "0.99"
        )
      )
    )

    # extracting subtitle (with NA)
    set.seed(123)
    subtitle2 <-
      expr_t_onesample(
        type = "bayes",
        data = ggplot2::msleep,
        x = brainwt,
        y = NULL,
        test.value = 0.25,
        bf.prior = 0.9,
        k = 3,
        output = "subtitle",
        conf.method = "eti"
      )

    expect_identical(
      subtitle2,
      ggplot2::expr(
        paste(
          "log"["e"] * "(BF"["01"] * ") = " * "2.125" * ", ",
          widehat(italic(delta))["median"]^"posterior" * " = " * "-0.018" * ", ",
          "CI"["95%"]^"HDI" * " [" * "-0.265" * ", " * "0.242" * "], ",
          italic("r")["Cauchy"]^"JZS" * " = " * "0.900"
        )
      )
    )
  }
)
