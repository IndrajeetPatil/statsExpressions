# long_to_wide_converter works ---------------------------------------------

test_that(
  desc = "long_to_wide_converter works - spread true",
  code = {

    # data without NAs ------------------------------

    # within-subjects
    set.seed(123)
    df1 <- long_to_wide_converter(
      data = iris_long,
      x = condition,
      y = value
    )

    # between-subjects
    set.seed(123)
    df2 <- long_to_wide_converter(
      data = mtcars,
      x = am,
      y = wt,
      paired = FALSE
    )

    # data with NAs ------------------------------

    # within-subjects
    set.seed(123)
    df3 <- long_to_wide_converter(
      data = bugs_long,
      x = condition,
      y = desire,
      paired = TRUE
    )

    # between-subjects
    set.seed(123)
    df4 <- long_to_wide_converter(
      data = ggplot2::msleep,
      x = vore,
      y = brainwt,
      paired = FALSE
    )

    # checking datasets
    set.seed(123)
    expect_snapshot(list(df1, df2, df3, df4))
  }
)


test_that(
  desc = "long_to_wide_converter works - spread false",
  code = {

    # ----------------------- data without NAs ------------------------------

    # within-subjects
    set.seed(123)
    df1 <- long_to_wide_converter(
      data = iris_long,
      x = condition,
      y = value,
      spread = FALSE
    )

    # between-subjects
    set.seed(123)
    df2 <- long_to_wide_converter(
      data = mtcars,
      x = am,
      y = wt,
      paired = FALSE,
      spread = FALSE
    )

    # -------------------------- data with NAs ------------------------------

    # within-subjects
    set.seed(123)
    df3 <- long_to_wide_converter(
      data = bugs_long,
      x = condition,
      y = desire,
      paired = TRUE,
      spread = FALSE
    )

    # between-subjects
    set.seed(123)
    df4 <- long_to_wide_converter(
      data = ggplot2::msleep,
      x = vore,
      y = brainwt,
      paired = FALSE,
      spread = FALSE
    )

    # checking datasets
    set.seed(123)
    expect_snapshot(list(df1, df2, df3, df4))
  }
)

# with .rowid - without NA ---------------------------------------------

test_that(
  desc = "with .rowid - without NA",
  code = {
    df <- structure(list(
      score = c(90, 90, 72.5, 45),
      condition = structure(c(1L, 2L, 2L, 1L), .Label = c("4", "5"), class = "factor"),
      id = c(1L, 2L, 1L, 2L)
    ),
    row.names = c(NA, -4L),
    class = c("tbl_df", "tbl", "data.frame")
    )

    df1 <- arrange(df, id)

    expect_equal(
      long_to_wide_converter(df1, condition, score),
      long_to_wide_converter(df, condition, score, id)
    )

    expect_equal(
      long_to_wide_converter(df1, condition, score, spread = FALSE) %>%
        arrange(.rowid),
      long_to_wide_converter(df, condition, score, id, spread = FALSE) %>%
        arrange(.rowid)
    )
  }
)


# with .rowid - with NA ---------------------------------------------

test_that(
  desc = "with .rowid - with NA",
  code = {
    df <- bugs_long
    df1 <- arrange(bugs_long, subject)

    expect_equal(
      long_to_wide_converter(df1, condition, desire) %>%
        select(-.rowid),
      long_to_wide_converter(df, condition, desire, subject) %>%
        select(-.rowid)
    )

    expect_equal(
      long_to_wide_converter(df1, condition, desire, spread = FALSE) %>%
        select(-.rowid),
      long_to_wide_converter(df, condition, desire, subject, spread = FALSE) %>%
        select(-.rowid)
    )
  }
)
