# don't test data frames because the values vary across platforms, even with the
# same seed
#
# for the same reason, don't change `k` parameter

# to print all tibble columns in the snapshot; don't remove
withr::local_options(list(tibble.width = Inf))

# between-subjects ------------------------------

skip_if_not_installed("rstantools")

test_that(
  desc = "bayesian (between-subjects - anova)",
  code = {
    # with NA
    set.seed(123)
    df1 <- suppressWarnings(oneway_anova(
      type     = "bayes",
      data     = msleep,
      x        = vore,
      y        = brainwt
    ))

    expect_snapshot(dim(df1))
    expect_snapshot(df1[["expression"]][[1]])

    # without NA
    set.seed(123)
    df2 <- suppressWarnings(oneway_anova(
      type        = "bayes",
      data        = iris,
      x           = Species,
      y           = Sepal.Length
    ))

    expect_snapshot(dim(df2))
    expect_snapshot(df2[["expression"]][[1]])
  }
)

# within-subjects ------------------------------

test_that(
  desc = "bayesian (within-subjects - anova)",
  code = {
    set.seed(123)
    df1 <- oneway_anova(
      type     = "bayes",
      data     = WRS2::WineTasting,
      x        = Wine,
      y        = Taste,
      paired   = TRUE,
      bf.prior = 0.88
    )

    expect_snapshot(dim(df1))
    expect_snapshot(df1[["expression"]][[1]])

    # data with NA
    set.seed(123)
    df2 <- oneway_anova(
      type   = "bayes",
      data   = bugs_long,
      x      = condition,
      y      = desire,
      paired = TRUE
    )

    expect_snapshot(dim(df2))
    expect_snapshot(df2[["expression"]][[1]])

    # with subject.id ---------------------------------

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
      type       = "bayes",
      data       = df,
      x          = condition,
      y          = score,
      subject.id = id,
      paired     = TRUE
    )

    # correct
    set.seed(123)
    expr2 <- oneway_anova(
      type   = "bayes",
      data   = arrange(df, id),
      x      = condition,
      y      = score,
      paired = TRUE
    )

    expect_equal(expr2, expr1, ignore_attr = TRUE)
  }
)
