test_that(
  desc = "expr_anova_robust works - between-subjects",
  code = {
    options(tibble.width = Inf)

    # between-subjects -------------------------------------------------------

    # `statsExpressions` output
    set.seed(123)
    df1 <- oneway_anova(
      type = "robust",
      data = mtcars,
      x = cyl,
      y = wt,
      paired = FALSE,
      k = 5,
      tr = 0.00025,
      nboot = 2
    )

    # `statsExpressions` output
    set.seed(123)
    df2 <- suppressWarnings(oneway_anova(
      type = "robust",
      data = filter(ggplot2::msleep, vore != "insecti"),
      x = vore,
      y = sleep_total,
      paired = FALSE,
      k = 4,
      nboot = 15,
      conf.level = 0.99
    ))

    # testing all details
    set.seed(123)
    expect_snapshot(select(df1, -expression))
    expect_snapshot(df1$expression[[1]])

    # testing all details
    set.seed(123)
    expect_snapshot(select(df2, -expression))
    expect_snapshot(df2$expression[[1]])
  }
)

test_that(
  desc = "expr_anova_robust works - within-subjects",
  code = {
    options(tibble.width = Inf)

    # within-subjects -------------------------------------------------------

    # `statsExpressions` output
    set.seed(123)
    df1 <- oneway_anova(
      type = "robust",
      data = bugs_long,
      x = condition,
      y = desire,
      k = 4L,
      paired = TRUE
    )

    # testing all details
    set.seed(123)
    expect_snapshot(select(df1, -expression))
    expect_snapshot(df1$expression[[1]])
  }
)
