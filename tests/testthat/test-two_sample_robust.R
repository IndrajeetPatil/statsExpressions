# within-subjects ------------------------------------------------------------

test_that(
  desc = "t_robust - within-subjects - without NAs",
  code = {
    options(tibble.width = Inf)

    # subtitle
    set.seed(123)
    df1 <- two_sample_test(
      type = "r",
      data = filter(
        iris_long,
        condition %in% c("Sepal.Length", "Sepal.Width")
      ),
      x = condition,
      y = value,
      paired = TRUE,
      k = 4
    )


    set.seed(123)
    expect_snapshot(select(df1, -expression))
    expect_snapshot(df1$expression)
  }
)

test_that(
  desc = "t_robust - within-subjects - with NAs",
  code = {
    # subtitle
    set.seed(123)
    df1 <- two_sample_test(
      type = "r",
      data = filter(bugs_long, condition %in% c("HDHF", "HDLF")),
      x = condition,
      y = desire,
      paired = TRUE,
      k = 3L
    )


    set.seed(123)
    expect_snapshot(select(df1, -expression))
    expect_snapshot(df1$expression)
  }
)


test_that(
  desc = "t_robust - between-subjects - without NAs",
  code = {
    # between-subjects ------------------------------------------------------

    # subtitle
    set.seed(123)
    df1 <- two_sample_test(
      type = "r",
      data = mtcars,
      x = am,
      y = wt,
      paired = FALSE,
      conf.level = 0.99,
      k = 3
    )


    set.seed(123)
    expect_snapshot(select(df1, -expression))
    expect_snapshot(df1$expression)
  }
)

test_that(
  desc = "t_robust - between-subjects - with NAs",
  code = {
    # subtitle
    set.seed(123)
    df1 <- two_sample_test(
      type = "r",
      data = filter(ggplot2::msleep, vore %in% c("carni", "herbi")),
      x = vore,
      y = brainwt,
      paired = FALSE,
      conf.level = 0.90,
      k = 4
    )


    set.seed(123)
    expect_snapshot(select(df1, -expression))
    expect_snapshot(df1$expression)
  }
)

test_that(
  desc = "works with subject id",
  code = {
    # works with subject id --------------------------------------

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

    df <- filter(df, condition %in% c(1, 5))

    # incorrect
    set.seed(123)
    expr1 <- two_sample_test(
      type = "r",
      data = df,
      x = condition,
      y = score,
      subject.id = id,
      paired = TRUE
    )

    # correct
    set.seed(123)
    expr2 <- two_sample_test(
      type = "r",
      data = arrange(df, id),
      x = condition,
      y = score,
      paired = TRUE
    )

    expect_equal(expr1, expr2, ignore_attr = TRUE)
  }
)
