test_that("parametric anova subtitles work (without NAs)", {
  set.seed(123)
  df <- oneway_anova(
    data = mtcars,
    x = cyl,
    y = wt,
    effsize.type = "eta",
    digits = 5L,
    var.equal = FALSE
  )

  expect_snapshot(select(df, -expression))
  expect_snapshot(df[["expression"]])

  set.seed(123)
  df1 <- oneway_anova(
    data = mtcars,
    x = cyl,
    y = wt,
    effsize.type = "eta",
    digits = 5L,
    var.equal = TRUE
  )

  expect_snapshot(select(df1, -expression))
  expect_snapshot(df1[["expression"]])
})

test_that("parametric anova subtitles with partial omega-squared", {
  set.seed(123)
  df1 <- oneway_anova(
    data = msleep,
    x = vore,
    y = brainwt,
    effsize.type = "unbiased",
    digits = 4L
  )

  expect_snapshot(select(df1, -expression))
  expect_snapshot(df1[["expression"]])
})

test_that("paired parametric anova subtitles work (without NAs)", {
  set.seed(123)
  df1 <- oneway_anova(
    data = iris_long,
    x = condition,
    y = value,
    paired = TRUE,
    digits = 3L,
    var.equal = FALSE, # shouldn't make a difference
    conf.level = 0.99
  )

  expect_snapshot(select(df1, -expression))
  expect_snapshot(df1[["expression"]])
})

test_that("works with subject id", {
  expect_subject_id_invariance(oneway_anova, data_with_subid)
})
