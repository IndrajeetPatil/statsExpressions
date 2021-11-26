test_that(
  desc = "bayes factor (between-subjects - anova)",
  code = {
    skip_if(getRversion() < "4.0")
    options(tibble.width = Inf)

    # bayes factor (between-subjects - anova) ------------------------------

    # extracting results from where this function is implemented
    set.seed(123)
    df1 <- suppressWarnings(oneway_anova(
      type = "bayes",
      data = ggplot2::msleep,
      x = vore,
      y = brainwt,
      bf.prior = 0.99,
      k = 3
    ))

    expect_snapshot(dplyr::select(df1, -expression), variant = .Platform$OS.type)
    expect_snapshot(as.character(df1$expression[[1]]), variant = .Platform$OS.type)

    # data where it works
    set.seed(123)
    df2 <- suppressWarnings(oneway_anova(
      type = "bayes",
      data = iris,
      x = Species,
      y = Sepal.Length,
      conf.level = 0.99,
      conf.method = "eti",
      k = 4
    ))

    expect_snapshot(dplyr::select(df2, -expression), variant = .Platform$OS.type)
    expect_snapshot(as.character(df2$expression[[1]]), variant = .Platform$OS.type)
  }
)

test_that(
  desc = "bayes factor (within-subjects - anova)",
  code = {
    # bayes factor (within-subjects - anova) ---------------------------------

    if (utils::packageVersion("BayesFactor") >= package_version("0.9.12-4.3")) {
      set.seed(123)
      df1 <- oneway_anova(
        type = "bayes",
        data = WRS2::WineTasting,
        x = Wine,
        y = Taste,
        paired = TRUE,
        bf.prior = 0.88
      )

      expect_snapshot(dplyr::select(df1, -expression), variant = .Platform$OS.type)
      expect_snapshot(as.character(df1$expression[[1]]), variant = .Platform$OS.type)

      # data with NA
      set.seed(123)
      df2 <- oneway_anova(
        type = "bayes",
        data = bugs_long,
        x = condition,
        y = desire,
        paired = TRUE
      )

      expect_snapshot(dplyr::select(df2, -expression), variant = .Platform$OS.type)
      expect_snapshot(as.character(df2$expression[[1]]), variant = .Platform$OS.type)

      # with subject.id ---------------------------------

      # data
      df <- structure(list(
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
      expr1 <- oneway_anova(
        type = "bayes",
        data = df,
        x = condition,
        y = score,
        subject.id = id,
        paired = TRUE
      )

      # correct
      set.seed(123)
      expr2 <- oneway_anova(
        type = "bayes",
        data = arrange(df, id),
        x = condition,
        y = score,
        paired = TRUE
      )

      expect_equal(expr2, expr1, ignore_attr = TRUE)
    }
  }
)
