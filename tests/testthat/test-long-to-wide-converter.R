withr::local_options(list(tibble.width = Inf))

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
      data = msleep,
      x = vore,
      y = brainwt,
      paired = FALSE
    )

    # checking datasets
    set.seed(123)
    expect_snapshot(purrr::walk(list(df1, df2, df3, df4), dplyr::glimpse))
    expect_snapshot(purrr::map(list(df1, df2, df3, df4), summary))
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
      data = msleep,
      x = vore,
      y = brainwt,
      paired = FALSE,
      spread = FALSE
    )

    set.seed(123)
    expect_snapshot(purrr::walk(list(df1, df2, df3, df4), dplyr::glimpse))
    expect_snapshot(purrr::map(list(df1, df2, df3, df4), summary))
  }
)

# with .rowid - without NA ---------------------------------------------

test_that(
  desc = "with .rowid - without NA",
  code = {
    df_original <- structure(
      list(
        score = c(90, 90, 72.5, 45),
        condition = structure(c(1L, 2L, 2L, 1L), .Label = c("4", "5"), class = "factor"),
        id = c(1L, 2L, 1L, 2L)
      ),
      row.names = c(NA, -4L),
      class = c("tbl_df", "tbl", "data.frame")
    )

    df_arranged <- arrange(df_original, id)

    expect_identical(
      long_to_wide_converter(df_arranged, condition, score),
      long_to_wide_converter(df_original, condition, score, id)
    )

    expect_identical(
      arrange(long_to_wide_converter(df_arranged, condition, score, spread = FALSE), .rowid),
      arrange(long_to_wide_converter(df_original, condition, score, id, spread = FALSE), .rowid)
    )
  }
)


# with .rowid - with NA ---------------------------------------------

test_that(
  desc = "with .rowid - with NA",
  code = {
    df_original <- bugs_long
    df_arranged <- arrange(bugs_long, subject)

    expect_identical(
      select(long_to_wide_converter(df_arranged, condition, desire), -.rowid),
      select(long_to_wide_converter(df_original, condition, desire, subject), -.rowid)
    )

    expect_identical(
      select(long_to_wide_converter(df_arranged, condition, desire, spread = FALSE), -.rowid),
      select(long_to_wide_converter(df_original, condition, desire, subject, spread = FALSE), -.rowid)
    )
  }
)
