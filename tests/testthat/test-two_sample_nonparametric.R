test_that(
  desc = "t_nonparametric works - between-subjects design",
  code = {
    # between-subjects design -----------------------------------------------

    set.seed(123)
    df <- two_sample_test(
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
  desc = "nonparametric works - within-subjects design",
  code = {
    # within-subjects design -----------------------------------------------

    set.seed(123)
    df <- suppressWarnings(two_sample_test(
      data = filter(bugs_long, condition %in% c("HDHF", "HDLF")),
      x = condition,
      y = desire,
      type = "np",
      k = 5L,
      conf.level = 0.99,
      paired = TRUE
    ))

    set.seed(123)
    expect_snapshot(select(df, -expression))
    expect_snapshot(df[["expression"]])
  }
)

test_that(
  desc = "works with subject id",
  code = {
    df <- filter(data_with_subid, condition %in% c(1, 5))

    set.seed(123)
    expr1 <- two_sample_test(
      type = "np",
      data = df,
      x = condition,
      y = score,
      subject.id = id,
      paired = TRUE
    )

    set.seed(123)
    expr2 <- two_sample_test(
      type = "np",
      data = arrange(df, id),
      x = condition,
      y = score,
      paired = TRUE
    )

    expect_equal(expr1, expr2, ignore_attr = TRUE)
  }
)
