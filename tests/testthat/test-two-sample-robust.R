# within-subjects ------------------------------------------------------------

test_that(desc = "t_robust - within-subjects - without NAs", code = {
  set.seed(123)
  df1 <- two_sample_test(
    type = "r",
    data = filter(
      iris_long,
      condition %in% c("Sepal.Length", "Sepal.Width")
    ),
    x = condition,
    y = value,
    paired = TRUE,
    digits = 4L
  )

  expect_snapshot(select(df1, -expression))
  expect_snapshot(df1[["expression"]])
})

test_that(desc = "t_robust - within-subjects - with NAs", code = {
  set.seed(123)
  df1 <- two_sample_test(
    type = "r",
    data = filter(bugs_long, condition %in% c("HDHF", "HDLF")),
    x = condition,
    y = desire,
    paired = TRUE,
    digits = 3L
  )

  expect_snapshot(select(df1, -expression))
  expect_snapshot(df1[["expression"]])
})


test_that(desc = "t_robust - between-subjects - without NAs", code = {
  # between-subjects ------------------------------------------------------

  set.seed(123)
  df1 <- two_sample_test(
    type = "r",
    data = mtcars,
    x = am,
    y = wt,
    paired = FALSE,
    conf.level = 0.99,
    digits = 3
  )

  expect_snapshot(select(df1, -expression))
  expect_snapshot(df1[["expression"]])
})

test_that(desc = "t_robust - between-subjects - with NAs", code = {
  set.seed(123)
  df1 <- two_sample_test(
    type = "r",
    data = filter(msleep, vore %in% c("carni", "herbi")),
    x = vore,
    y = brainwt,
    paired = FALSE,
    conf.level = 0.90,
    digits = 4L
  )

  expect_snapshot(select(df1, -expression))
  expect_snapshot(df1[["expression"]])
})

test_that(desc = "works with subject id", code = {
  expect_subject_id_invariance(
    two_sample_test,
    filter(data_with_subid, condition %in% c(1, 5)),
    type = "r"
  )
})
