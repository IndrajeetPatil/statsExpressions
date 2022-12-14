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
    set.seed(123)
    expr1 <- oneway_anova(
      type = "np",
      data = data_with_subid,
      x = condition,
      y = score,
      subject.id = id,
      paired = TRUE
    )

    set.seed(123)
    expr2 <- oneway_anova(
      type = "np",
      data = arrange(data_with_subid, id),
      x = condition,
      y = score,
      paired = TRUE
    )

    expect_equal(expr1, expr2, ignore_attr = TRUE)
  }
)
