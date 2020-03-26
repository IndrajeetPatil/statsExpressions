# within-subjects ------------------------------------------------------------

testthat::test_that(
  desc = "expr_t_robust - within-subjects - without NAs",
  code = {
    testthat::skip_if(getRversion() < "3.6")

    # subtitle
    set.seed(123)
    using_function1 <-
      expr_t_robust(
        data = dplyr::filter(
          statsExpressions::iris_long,
          condition %in% c("Sepal.Length", "Sepal.Width")
        ),
        x = "condition",
        y = value,
        paired = TRUE,
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
          "119",
          ") = ",
          "31.9809",
          ", ",
          italic("p"),
          " = ",
          "< 0.001",
          ", ",
          widehat(italic(xi)),
          " = ",
          "0.9265",
          ", CI"["90%"],
          " [",
          "0.9193",
          ", ",
          "0.9367",
          "]",
          ", ",
          italic("n")["pairs"],
          " = ",
          150L
        )
      )

    # testing overall call
    testthat::expect_identical(using_function1, results1)

    # checking if messages are okay
    set.seed(123)
    p_message <- capture.output(
      statsExpressions::expr_t_robust(
        data = dplyr::filter(
          statsExpressions::iris_long,
          condition %in% c("Sepal.Length", "Sepal.Width")
        ),
        x = condition,
        y = value,
        paired = TRUE,
        conf.level = 0.99,
        nboot = 20,
        k = 3,
        messages = TRUE
      )
    )

    # checking captured messages
    testthat::expect_match(p_message[1],
      "99% CI for effect size estimate was computed with 20",
      fixed = TRUE
    )
  }
)

testthat::test_that(
  desc = "expr_t_robust - within-subjects - with NAs",
  code = {
    testthat::skip_if(getRversion() < "3.6")

    # subtitle
    set.seed(123)
    using_function1 <- expr_t_robust(
      data = dplyr::filter(statsExpressions::bugs_long, condition %in% c("HDHF", "HDLF")),
      x = "condition",
      y = desire,
      paired = TRUE,
      conf.level = 0.99,
      k = 3,
      messages = FALSE
    )

    # expected
    results1 <- ggplot2::expr(
      paste(
        NULL,
        italic("t")["Yuen"],
        "(",
        "71",
        ") = ",
        "3.274",
        ", ",
        italic("p"),
        " = ",
        "0.002",
        ", ",
        widehat(italic(xi)),
        " = ",
        "0.273",
        ", CI"["99%"],
        " [",
        "0.066",
        ", ",
        "0.490",
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
          "< 0.001",
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
