# parametric t-test (between-subjects without NAs) ---------------------------

test_that(
  desc = "parametric t-test works (between-subjects without NAs)",
  code = {
    skip_if(getRversion() < "3.6")

    # `statsExpressions` output
    set.seed(123)
    using_function1 <-
      suppressWarnings(
        expr_t_twosample(
          data = dplyr::filter(
            movies_long,
            genre == "Action" | genre == "Drama"
          ),
          x = genre,
          y = rating,
          effsize.type = "d",
          var.equal = TRUE,
          conf.level = 0.99,
          k = 5
        )
      )

    # expected output
    set.seed(123)
    results1 <-
      ggplot2::expr(
        paste(
          italic("t")["Student"],
          "(",
          "612",
          ") = ",
          "-10.52948",
          ", ",
          italic("p"),
          " = ",
          "6.0984e-24",
          ", ",
          widehat(italic("d"))["Cohen"],
          " = ",
          "-0.92473",
          ", CI"["99%"],
          " [",
          "-1.16064",
          ", ",
          "-0.68822",
          "]",
          ", ",
          italic("n")["obs"],
          " = ",
          "614"
        )
      )

    # testing overall call
    expect_equal(using_function1, results1)
  }
)

# parametric t-test (between-subjects with NAs) ------------------------------

test_that(
  desc = "parametric t-test works (between-subjects with NAs)",
  code = {
    skip_if(getRversion() < "3.6")

    # `statsExpressions` output
    set.seed(123)
    using_function1 <-
      suppressWarnings(
        expr_t_twosample(
          data = dplyr::filter(
            movies_long,
            genre == "Action" | genre == "Drama"
          ),
          x = genre,
          y = rating,
          effsize.type = "g",
          var.equal = FALSE,
          conf.level = 0.90,
          k = 3
        )
      )

    # expected output
    set.seed(123)
    results1 <-
      ggplot2::expr(
        paste(
          italic("t")["Welch"],
          "(",
          "271.302",
          ") = ",
          "-9.275",
          ", ",
          italic("p"),
          " = ",
          "5.8e-18",
          ", ",
          widehat(italic("g"))["Hedges"],
          " = ",
          "-0.924",
          ", CI"["90%"],
          " [",
          "-1.074",
          ", ",
          "-0.773",
          "]",
          ", ",
          italic("n")["obs"],
          " = ",
          "614"
        )
      )

    # testing overall call
    expect_equal(using_function1, results1)
  }
)

# parametric t-test (within-subjects without NAs) ---------------------------

test_that(
  desc = "parametric t-test works (within-subjects without NAs)",
  code = {
    skip_if(getRversion() < "3.6")

    # output from `statsExpressions` helper subtitle
    set.seed(123)
    subtitle <-
      suppressWarnings(expr_t_twosample(
        data = dplyr::filter(
          iris_long,
          condition %in% c("Sepal.Length", "Sepal.Width")
        ),
        x = condition,
        y = value,
        paired = TRUE,
        effsize.type = "g",
        k = 4,
        conf.level = 0.50
      ))

    # expected
    expected <-
      ggplot2::expr(
        paste(
          italic("t")["Student"],
          "(",
          "149",
          ") = ",
          "34.8152",
          ", ",
          italic("p"),
          " = ",
          "1.85e-73",
          ", ",
          widehat(italic("g"))["Hedges"],
          " = ",
          "2.8283",
          ", CI"["50%"],
          " [",
          "2.7086",
          ", ",
          "2.9560",
          "]",
          ", ",
          italic("n")["pairs"],
          " = ",
          "150"
        )
      )

    # testing overall call
    expect_identical(subtitle, expected)
  }
)


# parametric t-test (within-subjects with NAs) ---------------------------

test_that(
  desc = "parametric t-test works (within-subjects with NAs)",
  code = {
    skip_if(getRversion() < "3.6")

    # output from `statsExpressions` helper subtitle
    set.seed(123)
    subtitle <-
      expr_t_twosample(
        data = dplyr::filter(bugs_long, condition %in% c("HDHF", "HDLF")),
        x = condition,
        y = desire,
        paired = TRUE,
        effsize.type = "d",
        k = 3
      )

    # expected
    expected <-
      ggplot2::expr(
        paste(
          italic("t")["Student"],
          "(",
          "89",
          ") = ",
          "3.613",
          ", ",
          italic("p"),
          " = ",
          "5e-04",
          ", ",
          widehat(italic("d"))["Cohen"],
          " = ",
          "0.381",
          ", CI"["95%"],
          " [",
          "0.167",
          ", ",
          "0.597",
          "]",
          ", ",
          italic("n")["pairs"],
          " = ",
          "90"
        )
      )

    # testing overall call
    expect_identical(subtitle, expected)
  }
)

# works with subject id ------------------------------------------------------

test_that(
  desc = "works with subject id",
  code = {
    skip_if(getRversion() < "3.6")

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
      expr_t_twosample(
        data = df,
        x = condition,
        y = score,
        subject.id = id,
        paired = TRUE
      )

    # correct
    set.seed(123)
    expr2 <-
      expr_t_twosample(
        data = dplyr::arrange(df, id),
        x = condition,
        y = score,
        paired = TRUE
      )

    expect_equal(expr1, expr2)
  }
)
