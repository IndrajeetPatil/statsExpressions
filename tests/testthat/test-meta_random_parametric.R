test_that(
  desc = "meta_analysis works - parametric",
  code = {
    skip_if_not_installed("metafor")

    set.seed(123)
    df <- suppressWarnings(meta_analysis(data_meta))

    set.seed(123)
    expect_snapshot(select(df, -expression))
    expect_snapshot(df[["expression"]])

    expect_error(suppressWarnings(meta_analysis(mtcars)))
  }
)
