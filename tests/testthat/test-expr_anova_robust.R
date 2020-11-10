
# between-subjects -------------------------------------------------------

testthat::test_that(
  desc = "expr_anova_robust works - between-subjects",
  code = {
    testthat::skip_if(getRversion() < "3.6")

    # `statsExpressions` output
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
          italic("F")["trimmed-means"],
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
          "0.85858",
          ", CI"["95%"],
          " [",
          "0.85268",
          ", ",
          "0.86448",
          "]",
          ", ",
          italic("n")["obs"],
          " = ",
          32L
        )
      )

    # testing overall call
    testthat::expect_identical(using_function1, results1)

    # `statsExpressions` output
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
          italic("F")["trimmed-means"],
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
          "0.1793",
          ", CI"["99%"],
          " [",
          "0.0711",
          ", ",
          "0.5717",
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

    # `statsExpressions` output
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
          italic("F")["trimmed-means"],
          "(",
          "2.7303",
          ",",
          "144.7051",
          ") = ",
          "20.9752",
          ", ",
          italic("p"),
          " = ",
          "1.146e-10",
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
          italic("F")["trimmed-means"],
          "(",
          "1.091",
          ",",
          "97.096",
          ") = ",
          "367.791",
          ", ",
          italic("p"),
          " = ",
          "0e+00",
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


# dataframe -----------------------------------------------------------

testthat::test_that(
  desc = "dataframe",
  code = {
    testthat::expect_s3_class(
      statsExpressions::expr_anova_robust(
        data = mtcars,
        x = cyl,
        y = wt,
        output = "dataframe"
      ),
      "tbl_df"
    )
  }
)

# works with subject id ------------------------------------------------------

testthat::test_that(
  desc = "works with subject id",
  code = {
    testthat::skip_if(getRversion() < "3.6")

    # data
    df <-
      structure(list(
        score = c(
          70, 82.5, 97.5, 100, 52.5, 62.5,
          92.5, 70, 90, 92.5, 90, 75, 60, 90, 85, 67.5, 90, 72.5, 45, 60,
          72.5, 80, 100, 100, 97.5, 95, 65, 87.5, 90, 62.5, 100, 100, 97.5,
          100, 97.5, 95, 82.5, 82.5, 40, 92.5, 85, 72.5, 35, 27.5, 82.5
        ), condition = structure(c(
          5L, 1L, 2L, 3L, 4L, 4L, 5L, 1L,
          2L, 3L, 2L, 3L, 3L, 4L, 2L, 1L, 5L, 5L, 4L, 1L, 1L, 4L, 3L, 5L,
          2L, 5L, 1L, 2L, 3L, 4L, 4L, 5L, 1L, 2L, 3L, 2L, 3L, 4L, 1L, 5L,
          3L, 2L, 5L, 4L, 1L
        ), .Label = c("1", "2", "3", "4", "5"), class = "factor"),
        id = structure(c(
          1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L,
          2L, 3L, 3L, 4L, 3L, 4L, 3L, 4L, 3L, 4L, 4L, 5L, 5L, 5L, 5L,
          5L, 6L, 6L, 6L, 6L, 6L, 7L, 7L, 7L, 7L, 7L, 8L, 8L, 8L, 8L,
          8L, 9L, 9L, 9L, 9L, 9L
        ), .Label = c(
          "1", "2", "3", "4", "5",
          "6", "7", "8", "9"
        ), class = "factor")
      ), row.names = c(
        NA,
        45L
      ), class = "data.frame")

    # incorrect
    set.seed(123)
    expr1 <-
      statsExpressions::expr_anova_robust(
        data = df,
        x = condition,
        y = score,
        subject.id = id,
        paired = TRUE
      )

    # correct
    set.seed(123)
    expr2 <-
      statsExpressions::expr_anova_robust(
        data = dplyr::arrange(df, id),
        x = condition,
        y = score,
        paired = TRUE
      )

    testthat::expect_equal(expr1, expr2)
  }
)
