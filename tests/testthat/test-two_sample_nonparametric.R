withr::local_options(list(tibble.width = Inf))

test_that(
  desc = "t_nonparametric works - between-subjects design",
  code = {
    # between-subjects design -----------------------------------------------

    # output
    set.seed(123)
    df <-
      two_sample_test(
        type = "np",
        data = mtcars,
        x = am,
        y = wt,
        k = 3L,
        conf.level = 0.90
      )


    set.seed(123)
    expect_snapshot(select(df, -expression))
    expect_snapshot(df[["expression"]])
  }
)

test_that(
  desc = "t_nonparametric works - within-subjects design",
  code = {
    # within-subjects design -----------------------------------------------

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
        k = 5L,
        conf.level = 0.99,
        paired = TRUE
      ))

    set.seed(123)
    expect_snapshot(select(df2, -expression))
    expect_snapshot(df2[["expression"]])
  }
)

test_that(
  desc = "works with subject id",
  code = {
    df <- filter(data_with_subid, condition %in% c(1, 5))


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


    set.seed(123)
    expr2 <-
      two_sample_test(
        type = "np",
        data = arrange(df, id),
        x = condition,
        y = score,
        paired = TRUE
      )

    expect_equal(expr1, expr2, ignore_attr = TRUE)
  }
)
