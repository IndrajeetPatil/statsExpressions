# between-subjects ----------------------------------------------------------

testthat::test_that(
  desc = "between-subjects - data with and without NAs",
  code = {
    testthat::skip_if(getRversion() < "3.6")

    # `statsExpressions` output
    set.seed(123)
    using_function1 <-
      statsExpressions::expr_anova_nonparametric(
        data = dplyr::sample_frac(statsExpressions::movies_long, 0.1),
        x = "genre",
        y = length,
        conf.type = "norm",
        paired = FALSE,
        k = 5,
        messages = TRUE
      )

    # expected output
    set.seed(123)
    results1 <-
      ggplot2::expr(
        paste(
          chi["Kruskal-Wallis"]^2,
          "(",
          "8",
          ") = ",
          "51.42672",
          ", ",
          italic("p"),
          " = ",
          "2.1714e-08",
          ", ",
          widehat(epsilon^2),
          " = ",
          "0.32756",
          ", CI"["95%"],
          " [",
          "0.16177",
          ", ",
          "0.42980",
          "]",
          ", ",
          italic("n")["obs"],
          " = ",
          158L
        )
      )

    # testing overall call
    testthat::expect_identical(using_function1, results1)

    # `statsExpressions` output
    set.seed(123)
    using_function2 <-
      suppressWarnings(statsExpressions::expr_anova_nonparametric(
        data = ggplot2::msleep,
        x = vore,
        y = sleep_cycle,
        k = 3,
        paired = FALSE,
        conf.level = 0.99,
        conf.type = "perc",
        messages = FALSE
      ))

    # expected output
    set.seed(123)
    results2 <-
      ggplot2::expr(
        paste(
          chi["Kruskal-Wallis"]^2,
          "(",
          "3",
          ") = ",
          "5.240",
          ", ",
          italic("p"),
          " = ",
          "0.155",
          ", ",
          widehat(epsilon^2),
          " = ",
          "0.175",
          ", CI"["99%"],
          " [",
          "0.003",
          ", ",
          "0.513",
          "]",
          ", ",
          italic("n")["obs"],
          " = ",
          31L
        )
      )

    # testing overall call
    testthat::expect_identical(using_function2, results2)
  }
)

# within-subjects -------------------------------------------------------

testthat::test_that(
  desc = "within-subjects - data with and without NAs",
  code = {
    testthat::skip_if(getRversion() < "3.6")

    # `statsExpressions` output
    set.seed(123)
    using_function1 <-
      statsExpressions::expr_anova_nonparametric(
        data = bugs_long,
        x = condition,
        y = "desire",
        k = 4L,
        conf.type = "norm",
        paired = TRUE,
        conf.level = 0.99,
        messages = FALSE
      )

    # expected output
    set.seed(123)
    results1 <-
      ggplot2::expr(
        paste(
          chi["Friedman"]^2,
          "(",
          "3",
          ") = ",
          "55.8338",
          ", ",
          italic("p"),
          " = ",
          "4.558e-12",
          ", ",
          widehat(italic("W"))["Kendall"],
          " = ",
          "0.6148",
          ", CI"["99%"],
          " [",
          "0.3369",
          ", ",
          "0.7022",
          "]",
          ", ",
          italic("n")["pairs"],
          " = ",
          88L
        )
      )

    # testing overall call
    testthat::expect_identical(using_function1, results1)

    # `statsExpressions` output
    set.seed(123)
    using_function2 <-
      statsExpressions::expr_anova_nonparametric(
        data = iris_long,
        x = condition,
        y = "value",
        k = 3,
        conf.type = "perc",
        paired = TRUE,
        conf.level = 0.90,
        messages = FALSE
      )

    # expected output
    set.seed(123)
    results2 <-
      ggplot2::expr(
        paste(
          chi["Friedman"]^2,
          "(",
          "3",
          ") = ",
          "410.000",
          ", ",
          italic("p"),
          " = ",
          "1.51e-88",
          ", ",
          widehat(italic("W"))["Kendall"],
          " = ",
          "0.486",
          ", CI"["90%"],
          " [",
          "0.345",
          ", ",
          "0.956",
          "]",
          ", ",
          italic("n")["pairs"],
          " = ",
          150L
        )
      )

    # testing overall call
    testthat::expect_identical(using_function2, results2)
  }
)


# dataframe -----------------------------------------------------------

testthat::test_that(
  desc = "dataframe",
  code = {
    testthat::expect_is(
      statsExpressions::expr_anova_nonparametric(
        data = mtcars,
        x = cyl,
        y = wt,
        output = "dataframe"
      ),
      "tbl_df"
    )
  }
)
