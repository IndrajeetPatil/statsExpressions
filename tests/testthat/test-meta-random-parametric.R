test_that(desc = "meta_analysis works - parametric", code = {
  skip_if_not_installed("metafor")

  df <- meta_analysis(data_meta)

  expect_snapshot(select(df, -expression))
  expect_snapshot(df[["expression"]])

  expect_snapshot(meta_analysis(mtcars), error = TRUE)
})
