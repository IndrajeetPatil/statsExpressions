test_that(
  desc = "meta_analysis works - robust",
  code = {
    skip_if_not_installed("metaplus")

    set.seed(123)
    df <- meta_analysis(data_meta, type = "robust", random = "normal")

    set.seed(123)
    expect_snapshot(select(df, -expression))
    expect_snapshot(df[["expression"]])
  }
)
