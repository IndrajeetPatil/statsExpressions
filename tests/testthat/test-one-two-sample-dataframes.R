test_that("parametric t-tests and ANOVAs", {
  # one-sample t-test (with NAs) ---------

  df_1 <- purrr::pmap_dfr(
    .l = list(
      data = list(msleep),
      x = "brainwt",
      test.value = 0.25,
      effsize.type = c("d", "g", "d", "g"),
      alternative = c("two.sided", "less", "greater", "two.sided"),
      conf.level = c(0.89, 0.99, 0.90, 0.50)
    ),
    .f = one_sample_test
  )

  expect_snapshot(df_1)

  # parametric t-test (between-subjects without NAs) ---------

  df_2_between <- purrr::pmap_dfr(
    .l = list(
      data = list(mtcars),
      x = "am",
      y = "wt",
      effsize.type = c("d", "g", "d", "g"),
      var.equal = c(TRUE, FALSE, TRUE, FALSE),
      alternative = c("two.sided", "less", "greater", "two.sided"),
      conf.level = c(0.89, 0.99, 0.90, 0.50)
    ),
    .f = two_sample_test
  )

  expect_snapshot(df_2_between)

  # parametric t-test (within-subjects with NAs) ---------

  df_2_within <- purrr::pmap_dfr(
    .l = list(
      data = list(filter(bugs_long, condition %in% c("HDHF", "HDLF"))),
      x = "condition",
      y = "desire",
      paired = TRUE,
      effsize.type = c("d", "g", "d", "g"),
      var.equal = c(TRUE, FALSE, TRUE, FALSE),
      conf.level = c(0.89, 0.99, 0.90, 0.50)
    ),
    .f = two_sample_test
  )

  expect_snapshot(df_2_within)

  # parametric ANOVA (between-subjects with NAs) ---------

  df_3_between <- purrr::pmap_dfr(
    .l = list(
      data = list(msleep),
      x = "vore",
      y = "sleep_rem",
      effsize.type = c("eta", "omega", "eta", "omega"),
      var.equal = c(TRUE, FALSE, TRUE, FALSE),
      conf.level = c(0.89, 0.80, 0.90, 0.50)
    ),
    .f = oneway_anova
  )

  expect_snapshot(df_3_between)

  # parametric ANOVA (within-subjects with NAs) ---------

  df_3_within <- purrr::pmap_dfr(
    .l = list(
      data = list(bugs_long),
      x = "condition",
      y = "desire",
      paired = TRUE,
      effsize.type = c("eta", "omega"),
      conf.level = c(0.89, 0.90)
    ),
    .f = oneway_anova
  )

  expect_snapshot(df_3_within)
})
