test_that(desc = "parametric t-test works (between-subjects without NAs)", code = {
  set.seed(123)
  df1 <- suppressWarnings(
    two_sample_test(
      ToothGrowth,
      x = supp,
      y = len,
      effsize.type = "d",
      var.equal = TRUE,
      conf.level = 0.99,
      digits = 5
    )
  )

  expect_snapshot(select(df1, -expression))
  expect_snapshot(df1[["expression"]])
})

test_that(desc = "parametric t-test works (between-subjects with NAs)", code = {
  set.seed(123)
  df1 <- suppressWarnings(
    two_sample_test(
      ToothGrowth,
      x = supp,
      y = len,
      effsize.type = "g",
      var.equal = FALSE,
      conf.level = 0.90,
      digits = 3
    )
  )

  expect_snapshot(select(df1, -expression))
  expect_snapshot(df1[["expression"]])
})

test_that(desc = "parametric t-test works (within-subjects without NAs)", code = {
  set.seed(123)
  df1 <- suppressWarnings(two_sample_test(
    data = filter(iris_long, condition %in% c("Sepal.Length", "Sepal.Width")),
    x = condition,
    y = value,
    paired = TRUE,
    effsize.type = "g",
    digits = 4L,
    conf.level = 0.50
  ))

  expect_snapshot(select(df1, -expression))
  expect_snapshot(df1[["expression"]])
})

test_that(desc = "parametric t-test works (within-subjects with NAs)", code = {
  set.seed(123)
  df1 <- two_sample_test(
    data = filter(bugs_long, condition %in% c("HDHF", "HDLF")),
    x = condition,
    y = desire,
    paired = TRUE,
    effsize.type = "d",
    digits = 3
  )

  expect_snapshot(select(df1, -expression))
  expect_snapshot(df1[["expression"]])
})

test_that(desc = "works with subject id", code = {
  expect_subject_id_invariance(
    two_sample_test,
    filter(data_with_subid, condition %in% c(1, 5))
  )
})
