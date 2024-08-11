test_that(
  desc = "parametric anova subtitles work (without NAs)",
  code = {
    set.seed(123)
    df <- oneway_anova(
      data = mtcars,
      x = cyl,
      y = wt,
      effsize.type = "eta",
      digits = 5L,
      var.equal = FALSE
    )

    set.seed(123)
    expect_snapshot(select(df, -expression))
    expect_snapshot(df[["expression"]])


    set.seed(123)
    df1 <- oneway_anova(
      data = mtcars,
      x = cyl,
      y = wt,
      effsize.type = "eta",
      digits = 5L,
      var.equal = TRUE
    )

    set.seed(123)
    expect_snapshot(select(df1, -expression))
    expect_snapshot(df1[["expression"]])
  }
)

test_that(
  desc = "parametric anova subtitles with partial omega-squared",
  code = {
    set.seed(123)
    df1 <- oneway_anova(
      data = msleep,
      x = vore,
      y = brainwt,
      effsize.type = "unbiased",
      digits = 4L
    )

    set.seed(123)
    expect_snapshot(select(df1, -expression))
    expect_snapshot(df1[["expression"]])
  }
)

test_that(
  desc = "paired parametric anova subtitles work (without NAs)",
  code = {
    set.seed(123)
    df1 <- oneway_anova(
      data = iris_long,
      x = condition,
      y = value,
      paired = TRUE,
      digits = 3L,
      var.equal = FALSE, # shouldn't make a difference
      conf.level = 0.99
    )

    set.seed(123)
    expect_snapshot(select(df1, -expression))
    expect_snapshot(df1[["expression"]])
  }
)


test_that(
  desc = "works with subject id",
  code = {
    df <- data_with_subid

    set.seed(123)
    expr1 <- oneway_anova(
      data = df,
      x = condition,
      y = score,
      subject.id = id,
      paired = TRUE
    )

    set.seed(123)
    expr2 <- oneway_anova(
      data = arrange(df, id),
      x = condition,
      y = score,
      paired = TRUE
    )

    expect_equal(expr1, expr2, ignore_attr = TRUE)
  }
)
