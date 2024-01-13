test_that(
  desc = "expr_anova_robust works - between-subjects",
  code = {
    # between-subjects -------------------------------------------------------

    set.seed(123)
    df1 <- oneway_anova(
      type = "robust",
      data = mtcars,
      x = cyl,
      y = wt,
      paired = FALSE,
      k = 5L,
      tr = 0.00025,
      nboot = 2
    )

    set.seed(123)
    df2 <- suppressWarnings(oneway_anova(
      type = "robust",
      data = filter(msleep, vore != "insecti"),
      x = vore,
      y = sleep_total,
      paired = FALSE,
      k = 4L,
      nboot = 15,
      conf.level = 0.99
    ))

    set.seed(123)
    expect_snapshot(select(df1, -expression))
    expect_snapshot(df1[["expression"]])

    set.seed(123)
    expect_snapshot(select(df2, -expression))
    expect_snapshot(df2[["expression"]])
  }
)

test_that(
  desc = "expr_anova_robust works - within-subjects",
  code = {
    # within-subjects -------------------------------------------------------

    set.seed(123)
    df1 <- oneway_anova(
      type = "robust",
      data = bugs_long,
      x = condition,
      y = desire,
      k = 4L,
      paired = TRUE
    )

    set.seed(123)
    expect_snapshot(select(df1, -expression))
    expect_snapshot(df1[["expression"]])

    # edge case with few cases

    dat <- tibble(
      cases = c(1, 1, 2, 2, 3, 3, 4, 4),
      groups = c("b", "a", "b", "a", "b", "a", "b", "a"),
      values = c(2, 5, 3, 4, 4, 6, 5, 8)
    )

    set.seed(123)
    df2 <- oneway_anova(
      data = dat,
      x = groups,
      y = values,
      type = "r",
      conf.level = 0.99,
      paired = TRUE
    )

    set.seed(123)
    expect_snapshot(select(df2, -expression))
    expect_snapshot(df2[["expression"]])
  }
)
