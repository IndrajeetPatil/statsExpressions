test_that("meta_analysis works - robust", {
  skip_if_not_installed("metaplus")

  df <- meta_analysis(data_meta, type = "robust", random = "normal")

  expect_snapshot(select(df, -expression))
  expect_snapshot(df[["expression"]])
})
