context("expr_anova_robust")

# between-subjects -------------------------------------------------------

testthat::test_that(
  desc = "expr_anova_robust works - between-subjects",
  code = {
    testthat::skip_if(getRversion() < "3.6")

    # ggstatsplot output
    set.seed(123)
    using_function1 <-
      statsExpressions::expr_anova_robust(
        data = mtcars,
        x = cyl,
        y = "wt",
        paired = FALSE,
        k = 5,
        tr = 0.00025,
        nboot = 2,
        messages = FALSE
      )

    # expected output
    results1 <-
      ggplot2::expr(
        paste(
          NULL,
          italic("F"),
          "(",
          "2",
          ",",
          "18.97383",
          ") = ",
          "20.24946",
          ", ",
          italic("p"),
          " = ",
          "2e-05",
          ", ",
          widehat(italic(xi)),
          " = ",
          "0.95933",
          ", CI"["95%"],
          " [",
          "0.84565",
          ", ",
          "1.00154",
          "]",
          ", ",
          italic("n")["obs"],
          " = ",
          32L
        )
      )

    # testing overall call
    testthat::expect_identical(using_function1, results1)

    # ggstatsplot output
    set.seed(123)
    using_function2 <-
      suppressWarnings(statsExpressions::expr_anova_robust(
        data = dplyr::filter(ggplot2::msleep, vore != "insecti"),
        x = vore,
        y = sleep_total,
        paired = FALSE,
        k = 4,
        nboot = 15,
        conf.level = 0.99,
        conf.type = "basic",
        messages = TRUE
      ))

    # expected output
    results2 <-
      ggplot2::expr(
        paste(
          NULL,
          italic("F"),
          "(",
          "2",
          ",",
          "35.1708",
          ") = ",
          "0.2695",
          ", ",
          italic("p"),
          " = ",
          "0.7653",
          ", ",
          widehat(italic(xi)),
          " = ",
          "0.1352",
          ", CI"["99%"],
          " [",
          "-0.0284",
          ", ",
          "0.2206",
          "]",
          ", ",
          italic("n")["obs"],
          " = ",
          71L
        )
      )

    # testing overall call
    testthat::expect_identical(using_function2, results2)
  }
)

# within-subjects -------------------------------------------------------

testthat::test_that(
  desc = "expr_anova_robust works - within-subjects",
  code = {
    testthat::skip_if(getRversion() < "3.6")

    # ggstatsplot output
    set.seed(123)
    using_function1 <-
      statsExpressions::expr_anova_robust(
        data = statsExpressions::bugs_long,
        x = "condition",
        y = desire,
        k = 4,
        tr = 0.2,
        paired = TRUE
      )

    # expected output
    results1 <-
      ggplot2::expr(
        paste(
          italic("F"),
          "(",
          "2.7303",
          ",",
          "144.7051",
          ") = ",
          "20.9752",
          ", ",
          italic("p"),
          " = ",
          "< 0.001",
          ", ",
          italic("n")["pairs"],
          " = ",
          88L
        )
      )

    # testing overall call
    testthat::expect_identical(using_function1, results1)

    # ggstatsplot output
    set.seed(123)
    using_function2 <-
      statsExpressions::expr_anova_robust(
        data = iris_long,
        x = condition,
        y = value,
        k = 3,
        paired = TRUE
      )

    # expected output
    set.seed(123)
    results2 <-
      ggplot2::expr(
        paste(
          italic("F"),
          "(",
          "1.091",
          ",",
          "97.096",
          ") = ",
          "367.791",
          ", ",
          italic("p"),
          " = ",
          "< 0.001",
          ", ",
          italic("n")["pairs"],
          " = ",
          150L
        )
      )

    # testing overall call
    testthat::expect_identical(using_function1, results1)
  }
)
