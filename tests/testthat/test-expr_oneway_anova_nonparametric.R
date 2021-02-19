# between-subjects ----------------------------------------------------------

test_that(
  desc = "between-subjects - data with and without NAs",
  code = {


    # `statsExpressions` output
    set.seed(123)
    using_function1 <-
      expr_oneway_anova(
        type = "np",
        data = dplyr::sample_frac(movies_long, 0.1),
        x = "genre",
        y = length,
        paired = FALSE,
        k = 5
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
          widehat(epsilon)["ordinal"]^2,
          " = ",
          "0.32756",
          ", CI"["95%"],
          " [",
          "0.25737",
          ", ",
          "0.50585",
          "]",
          ", ",
          italic("n")["obs"],
          " = ",
          "158"
        )
      )

    # testing overall call
    expect_identical(using_function1, results1)

    # `statsExpressions` output
    set.seed(123)
    using_function2 <-
      suppressWarnings(expr_oneway_anova(
        type = "np",
        data = ggplot2::msleep,
        x = vore,
        y = sleep_cycle,
        k = 3,
        paired = FALSE,
        conf.level = 0.99
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
          widehat(epsilon)["ordinal"]^2,
          " = ",
          "0.175",
          ", CI"["99%"],
          " [",
          "0.053",
          ", ",
          "0.494",
          "]",
          ", ",
          italic("n")["obs"],
          " = ",
          "31"
        )
      )

    # testing overall call
    expect_identical(using_function2, results2)
  }
)

# within-subjects -------------------------------------------------------

test_that(
  desc = "within-subjects - data with and without NAs",
  code = {


    # `statsExpressions` output
    set.seed(123)
    using_function1 <-
      expr_oneway_anova(
        type = "np",
        data = bugs_long,
        x = condition,
        y = "desire",
        k = 4L,
        paired = TRUE,
        conf.level = 0.99
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
          "0.6021",
          ", CI"["99%"],
          " [",
          "0.6021",
          ", ",
          "0.9748",
          "]",
          ", ",
          italic("n")["pairs"],
          " = ",
          "88"
        )
      )

    # testing overall call
    expect_identical(using_function1, results1)

    # `statsExpressions` output
    set.seed(123)
    using_function2 <-
      expr_oneway_anova(
        type = "np",
        data = iris_long,
        x = condition,
        y = "value",
        k = 3,
        paired = TRUE,
        conf.level = 0.90
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
          "0.484",
          ", CI"["90%"],
          " [",
          "0.343",
          ", ",
          "0.969",
          "]",
          ", ",
          italic("n")["pairs"],
          " = ",
          "150"
        )
      )

    # testing overall call
    expect_identical(using_function2, results2)
  }
)


# dataframe -----------------------------------------------------------

test_that(
  desc = "dataframe",
  code = {
    expect_s3_class(
      expr_oneway_anova(
        type = "np",
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

test_that(
  desc = "works with subject id",
  code = {


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
      expr_oneway_anova(
        type = "np",
        data = df,
        x = condition,
        y = score,
        subject.id = id,
        paired = TRUE
      )

    # correct
    set.seed(123)
    expr2 <-
      expr_oneway_anova(
        type = "np",
        data = dplyr::arrange(df, id),
        x = condition,
        y = score,
        paired = TRUE
      )

    expect_equal(expr1, expr2)
  }
)
