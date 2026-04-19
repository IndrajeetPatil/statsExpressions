library(statsExpressions)

test_that("pairwise_comparisons works with custom threshold", {
  set.seed(123)
  df <- pairwise_comparisons(
    data               = mtcars,
    x                  = cyl,
    y                  = wt,
    type               = "parametric",
    p.adjust.threshold = 0.05
  )

  expect_s3_class(df, "statsExpressions")
  expect_true("significant" %in% names(df))
  expect_true(all(grepl("<=", as.character(df$expression[df$significant]))))
  expect_true(all(grepl(">", as.character(df$expression[!df$significant]))))
})
