withr::local_options(list(tibble.width = Inf))
skip_if_not_installed("boot")

# between-subjects ----------------------------------------------

test_that(
  desc = "between-subjects",
  code = {
    # without NA
    set.seed(123)
    df1 <-
      oneway_anova(
        type = "np",
        data = sample_frac(movies_long, 0.1),
        x = genre,
        y = length,
        paired = FALSE
      )

    set.seed(123)
    expect_snapshot(select(df1, -expression))
    expect_snapshot(df1[["expression"]])

    # with NA
    set.seed(123)
    df2 <-
      suppressWarnings(oneway_anova(
        type = "np",
        data = msleep,
        x = vore,
        y = sleep_cycle,
        paired = FALSE,
        conf.level = 0.99
      ))

    set.seed(123)
    expect_snapshot(select(df2, -expression))
    expect_snapshot(df2[["expression"]])
  }
)

# wthin-subjects ----------------------------------------------

test_that(
  desc = "within-subjects",
  code = {
    #  with NAs
    set.seed(123)
    df1 <-
      oneway_anova(
        type = "np",
        data = bugs_long,
        x = condition,
        y = desire,
        paired = TRUE,
        conf.level = 0.99
      )

    set.seed(123)
    expect_snapshot(select(df1, -expression))
    expect_snapshot(df1[["expression"]])

    # without NAs
    set.seed(123)
    df2 <-
      oneway_anova(
        type = "np",
        data = iris_long,
        x = condition,
        y = value,
        paired = TRUE,
        conf.level = 0.90
      )

    set.seed(123)
    expect_snapshot(select(df2, -expression))
    expect_snapshot(df2[["expression"]])
  }
)

test_that(
  desc = "works with subject id",
  code = {
    # works with subject id -----------------------------------------------

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

    set.seed(123)
    expr1 <- oneway_anova(
      type = "np",
      data = df,
      x = condition,
      y = score,
      subject.id = id,
      paired = TRUE
    )

    set.seed(123)
    expr2 <- oneway_anova(
      type = "np",
      data = arrange(df, id),
      x = condition,
      y = score,
      paired = TRUE
    )

    expect_equal(expr1, expr2, ignore_attr = TRUE)
  }
)
