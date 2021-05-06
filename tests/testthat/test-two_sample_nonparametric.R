test_that(
  desc = "t_nonparametric works - between-subjects design",
  code = {
    options(tibble.width = Inf)

    # between-subjects design -----------------------------------------------

    # output
    set.seed(123)
    df <-
      two_sample_test(
        type = "np",
        data = mtcars,
        x = am,
        y = wt,
        k = 3,
        conf.level = 0.90
      )

    # testing all details
    set.seed(123)
    expect_snapshot(dplyr::select(df, -expression))
    expect_snapshot(df$expression[[1]])
  }
)

test_that(
  desc = "t_nonparametric works - within-subjects design",
  code = {
    # within-subjects design -----------------------------------------------

    # data
    df_bird <-
      structure(list(Bird = c(
        "A", "B", "C", "D", "E", "F", "G", "H",
        "I", "J", "K", "L", "M", "N", "O", "P", "A", "B", "C", "D", "E",
        "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P"
      ), type = c(
        "Typical",
        "Typical", "Typical", "Typical", "Typical", "Typical", "Typical",
        "Typical", "Typical", "Typical", "Typical", "Typical", "Typical",
        "Typical", "Typical", "Typical", "Odd", "Odd", "Odd", "Odd",
        "Odd", "Odd", "Odd", "Odd", "Odd", "Odd", "Odd", "Odd", "Odd",
        "Odd", "Odd", "Odd"
      ), length = c(
        -0.255, -0.213, -0.19, -0.185,
        -0.045, -0.025, -0.015, 0.003, 0.015, 0.02, 0.023, 0.04, 0.04,
        0.05, 0.055, 0.058, -0.324, -0.185, -0.299, -0.144, -0.027, -0.039,
        -0.264, -0.077, -0.017, -0.169, -0.096, -0.33, -0.346, -0.191,
        -0.128, -0.182
      )), row.names = c(NA, -32L), class = c(
        "tbl_df",
        "tbl", "data.frame"
      ))

    # expect error
    expect_error(suppressWarnings(two_sample_test(
      type = "np",
      data = iris,
      x = Sepal.Length,
      y = Species
    )))

    # ggstatsplot output
    set.seed(123)
    df2 <-
      suppressWarnings(two_sample_test(
        type = "np",
        data = df_bird,
        x = type,
        y = length,
        k = 5,
        conf.level = 0.99,
        paired = TRUE
      ))

    # testing all details
    set.seed(123)
    expect_snapshot(dplyr::select(df2, -expression))
    expect_snapshot(df2$expression[[1]])
  }
)

test_that(
  desc = "works with subject id",
  code = {
    # works with subject id --------------------------------------------------

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
      two_sample_test(
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
      two_sample_test(
        type = "np",
        data = dplyr::arrange(df, id),
        x = condition,
        y = score,
        paired = TRUE
      )

    expect_equal(expr1, expr2)
  }
)
