# don't test data frames because the values vary across platforms, even with the
# same seed; for the same reason, don't change number of digits

skip_if_not(getRversion() >= "4.4.0")

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

    set.seed(123)
    expr1 <- oneway_anova(
      type       = "bayes",
      data       = data_with_subid,
      x          = condition,
      y          = score,
      subject.id = id,
      paired     = TRUE
    )


    set.seed(123)
    expr2 <- oneway_anova(
      type   = "bayes",
      data   = arrange(data_with_subid, id),
      x      = condition,
      y      = score,
      paired = TRUE
    )

    expect_equal(expr2, expr1, ignore_attr = TRUE)
  }
)
