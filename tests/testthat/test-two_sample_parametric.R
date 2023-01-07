test_that(
  desc = "parametric t-test works (between-subjects without NAs)",
  code = {
    set.seed(123)
    df1 <- suppressWarnings(
      two_sample_test(
        ToothGrowth,
        x = supp,
        y = len,
        effsize.type = "d",
        var.equal = TRUE,
        conf.level = 0.99,
        k = 5
      )
    )

    set.seed(123)
    expect_snapshot(select(df1, -expression))
    expect_snapshot(df1[["expression"]])
  }
)

test_that(
  desc = "parametric t-test works (between-subjects with NAs)",
  code = {
    set.seed(123)
    df1 <- suppressWarnings(
      two_sample_test(
        ToothGrowth,
        x = supp,
        y = len,
        effsize.type = "g",
        var.equal = FALSE,
        conf.level = 0.90,
        k = 3
      )
    )

    set.seed(123)
    expect_snapshot(select(df1, -expression))
    expect_snapshot(df1[["expression"]])
  }
)

test_that(
  desc = "parametric t-test works (within-subjects without NAs)",
  code = {
    set.seed(123)
    df1 <- suppressWarnings(two_sample_test(
      data = filter(iris_long, condition %in% c("Sepal.Length", "Sepal.Width")),
      x = condition,
      y = value,
      paired = TRUE,
      effsize.type = "g",
      k = 4L,
      conf.level = 0.50
    ))

    set.seed(123)
    expect_snapshot(select(df1, -expression))
    expect_snapshot(df1[["expression"]])
  }
)


test_that(
  desc = "parametric t-test works (within-subjects with NAs)",
  code = {
    set.seed(123)
    df1 <- two_sample_test(
      data = filter(bugs_long, condition %in% c("HDHF", "HDLF")),
      x = condition,
      y = desire,
      paired = TRUE,
      effsize.type = "d",
      k = 3
    )

    set.seed(123)
    expect_snapshot(select(df1, -expression))
    expect_snapshot(df1[["expression"]])
  }
)

test_that(
  desc = "works with subject id",
  code = {
    df <- filter(data_with_subid, condition %in% c(1, 5))

    set.seed(123)
    expr1 <- two_sample_test(
      data = df,
      x = condition,
      y = score,
      subject.id = id,
      paired = TRUE
    )

    set.seed(123)
    expr2 <- two_sample_test(
      data = arrange(df, id),
      x = condition,
      y = score,
      paired = TRUE
    )

    expect_equal(expr1, expr2, ignore_attr = TRUE)
  }
)
