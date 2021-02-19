# within-subjects ------------------------------------------------------------

test_that(
  desc = "t_robust - within-subjects - without NAs",
  code = {


    # subtitle
    set.seed(123)
    using_function1 <-
      t_twosample(
        type = "r",
        data = dplyr::filter(iris_long, condition %in% c("Sepal.Length", "Sepal.Width")),
        x = "condition",
        y = value,
        paired = TRUE,
        k = 4
      )

    # expected
    results1 <-
      ggplot2::expr(
        paste(
          italic("t")["Yuen"],
          "(",
          "89",
          ") = ",
          "28.7230",
          ", ",
          italic("p"),
          " = ",
          "0e+00",
          ", ",
          widehat(delta)["R"]^"AKP",
          " = ",
          "2.3582",
          ", CI"["95%"],
          " [",
          "1.9615",
          ", ",
          "2.6081",
          "]",
          ", ",
          italic("n")["pairs"],
          " = ",
          "150"
        )
      )

    # testing overall call
    expect_identical(using_function1$expression[[1]], results1)
  }
)

test_that(
  desc = "t_robust - within-subjects - with NAs",
  code = {


    # subtitle
    set.seed(123)
    using_function1 <-
      t_twosample(
        type = "r",
        data = dplyr::filter(bugs_long, condition %in% c("HDHF", "HDLF")),
        x = "condition",
        y = desire,
        paired = TRUE,
        k = 3
      )

    # expected
    results1 <-
      ggplot2::expr(
        paste(
          italic("t")["Yuen"],
          "(",
          "53",
          ") = ",
          "2.909",
          ", ",
          italic("p"),
          " = ",
          "0.005",
          ", ",
          widehat(delta)["R"]^"AKP",
          " = ",
          "0.410",
          ", CI"["95%"],
          " [",
          "0.238",
          ", ",
          "0.611",
          "]",
          ", ",
          italic("n")["pairs"],
          " = ",
          "90"
        )
      )

    # testing overall call
    expect_identical(using_function1$expression[[1]], results1)
  }
)


# between-subjects ------------------------------------------------------------

test_that(
  desc = "t_robust - between-subjects - without NAs",
  code = {


    # subtitle
    set.seed(123)
    using_function1 <-
      t_twosample(
        type = "r",
        data = mtcars,
        x = am,
        y = "wt",
        paired = FALSE,
        conf.level = 0.99,
        k = 3
      )

    # expected
    results1 <-
      ggplot2::expr(
        paste(
          italic("t")["Yuen"],
          "(",
          "13.584",
          ") = ",
          "5.840",
          ", ",
          italic("p"),
          " = ",
          "4.85e-05",
          ", ",
          widehat(xi),
          " = ",
          "0.915",
          ", CI"["99%"],
          " [",
          "0.702",
          ", ",
          "0.979",
          "]",
          ", ",
          italic("n")["obs"],
          " = ",
          "32"
        )
      )

    # testing overall call
    expect_identical(using_function1$expression[[1]], results1)
  }
)

test_that(
  desc = "t_robust - between-subjects - with NAs",
  code = {


    # subtitle
    set.seed(123)
    using_function1 <-
      t_twosample(
        type = "r",
        data = dplyr::filter(ggplot2::msleep, vore %in% c("carni", "herbi")),
        x = "vore",
        y = "brainwt",
        paired = FALSE,
        conf.level = 0.90,
        k = 4
      )

    # expected
    results1 <-
      ggplot2::expr(
        paste(
          italic("t")["Yuen"],
          "(",
          "13.8476",
          ") = ",
          "0.4521",
          ", ",
          italic("p"),
          " = ",
          "0.6582",
          ", ",
          widehat(xi),
          " = ",
          "0.3659",
          ", CI"["90%"],
          " [",
          "0.0000",
          ", ",
          "0.7768",
          "]",
          ", ",
          italic("n")["obs"],
          " = ",
          "29"
        )
      )

    # testing overall call
    expect_identical(using_function1$expression[[1]], results1)
  }
)


# dataframe -----------------------------------------------------------

test_that(
  desc = "dataframe",
  code = {
    expect_s3_class(
      statsExpressions::t_twosample(
        type = "r",
        data = dplyr::filter(movies_long, genre == "Action" | genre == "Drama"),
        x = "genre",
        y = rating,
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

    df <- dplyr::filter(df, condition %in% c(1, 5))

    # incorrect
    set.seed(123)
    expr1 <-
      statsExpressions::t_twosample(
        type = "r",
        data = df,
        x = condition,
        y = score,
        subject.id = id,
        paired = TRUE
      )

    # correct
    set.seed(123)
    expr2 <-
      statsExpressions::t_twosample(
        type = "r",
        data = dplyr::arrange(df, id),
        x = condition,
        y = score,
        paired = TRUE
      )

    expect_equal(expr1, expr2)
  }
)
