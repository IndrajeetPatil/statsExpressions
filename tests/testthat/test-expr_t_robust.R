# within-subjects ------------------------------------------------------------

testthat::test_that(
  desc = "expr_t_robust - within-subjects - without NAs",
  code = {
    testthat::skip_if(getRversion() < "3.6")

    # subtitle
    set.seed(123)
    using_function1 <-
      expr_t_robust(
        data = dplyr::filter(iris_long, condition %in% c("Sepal.Length", "Sepal.Width")),
        x = "condition",
        y = value,
        paired = TRUE,
        k = 4,
        messages = FALSE
      )

    # expected
    results1 <-
      ggplot2::expr(
        paste(
          NULL,
          italic("t")["Yuen"],
          "(",
          "119",
          ") = ",
          "31.9809",
          ", ",
          italic("p"),
          " = ",
          "0e+00",
          ", ",
          widehat(italic(delta))["R"],
          " = ",
          "2.6018",
          ", CI"["95%"],
          " [",
          "2.2905",
          ", ",
          "2.8700",
          "]",
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

testthat::test_that(
  desc = "expr_t_robust - within-subjects - with NAs",
  code = {
    testthat::skip_if(getRversion() < "3.6")

    # subtitle
    set.seed(123)
    using_function1 <-
      expr_t_robust(
        data = dplyr::filter(bugs_long, condition %in% c("HDHF", "HDLF")),
        x = "condition",
        y = desire,
        paired = TRUE,
        k = 3,
        messages = FALSE
      )

    # expected
    results1 <-
      ggplot2::expr(
        paste(
          NULL,
          italic("t")["Yuen"],
          "(",
          "71",
          ") = ",
          "-3.274",
          ", ",
          italic("p"),
          " = ",
          "0.002",
          ", ",
          widehat(italic(delta))["R"],
          " = ",
          "-0.418",
          ", CI"["95%"],
          " [",
          "-0.599",
          ", ",
          "-0.210",
          "]",
          ", ",
          italic("n")["pairs"],
          " = ",
          90L
        )
      )

    # testing overall call
    testthat::expect_identical(using_function1, results1)
  }
)


# between-subjects ------------------------------------------------------------

testthat::test_that(
  desc = "expr_t_robust - between-subjects - without NAs",
  code = {
    testthat::skip_if(getRversion() < "3.6")

    # subtitle
    set.seed(123)
    using_function1 <-
      expr_t_robust(
        data = mtcars,
        x = am,
        y = "wt",
        paired = FALSE,
        conf.level = 0.99,
        k = 3,
        messages = FALSE
      )

    # expected
    results1 <-
      ggplot2::expr(
        paste(
          NULL,
          italic("t")["Yuen"],
          "(",
          "24.816",
          ") = ",
          "5.255",
          ", ",
          italic("p"),
          " = ",
          "1.97e-05",
          ", ",
          widehat(italic(xi)),
          " = ",
          "0.818",
          ", CI"["99%"],
          " [",
          "0.727",
          ", ",
          "0.985",
          "]",
          ", ",
          italic("n")["obs"],
          " = ",
          32L
        )
      )

    # testing overall call
    testthat::expect_identical(using_function1, results1)
  }
)

testthat::test_that(
  desc = "expr_t_robust - between-subjects - with NAs",
  code = {
    testthat::skip_if(getRversion() < "3.6")

    # subtitle
    set.seed(123)
    using_function1 <-
      expr_t_robust(
        data = dplyr::filter(ggplot2::msleep, vore %in% c("carni", "herbi")),
        x = "vore",
        y = "brainwt",
        paired = FALSE,
        conf.level = 0.90,
        k = 4,
        messages = FALSE
      )

    # expected
    results1 <-
      ggplot2::expr(
        paste(
          NULL,
          italic("t")["Yuen"],
          "(",
          "20.9388",
          ") = ",
          "0.6891",
          ", ",
          italic("p"),
          " = ",
          "0.4983",
          ", ",
          widehat(italic(xi)),
          " = ",
          "0.2874",
          ", CI"["90%"],
          " [",
          "0.0000",
          ", ",
          "0.5875",
          "]",
          ", ",
          italic("n")["obs"],
          " = ",
          29L
        )
      )

    # testing overall call
    testthat::expect_identical(using_function1, results1)
  }
)
