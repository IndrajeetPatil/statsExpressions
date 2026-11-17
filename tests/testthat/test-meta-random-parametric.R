test_that("meta_analysis works - parametric", {
  skip_if_not_installed("metafor")

  df <- meta_analysis(data_meta)

  expect_snapshot(select(df, -expression))
  expect_snapshot(df[["expression"]])

  expect_snapshot(meta_analysis(mtcars), error = TRUE)
  expect_snapshot(meta_analysis(data_meta, type = "np"), error = TRUE)
})
