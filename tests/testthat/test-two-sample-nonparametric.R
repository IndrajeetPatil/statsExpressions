test_that(desc = "t_nonparametric works - between-subjects design", code = {
  df <- two_sample_test(
    type = "np",
    data = mtcars,
    x = am,
    y = wt,
    digits = 3L,
    conf.level = 0.90
  )

  expect_snapshot(select(df, -expression))
  expect_snapshot(df[["expression"]])
})

test_that(desc = "nonparametric works - within-subjects design", code = {
  df <- suppressWarnings(two_sample_test(
    data = filter(bugs_long, condition %in% c("HDHF", "HDLF")),
    x = condition,
    y = desire,
    type = "np",
    digits = 5L,
    conf.level = 0.99,
    paired = TRUE
  ))

  snapshot_variant <- if (getRversion() >= "4.7.0") "r-4.7" else NULL
  expect_snapshot(select(df, -expression), variant = snapshot_variant)
  expect_snapshot(df[["expression"]], variant = snapshot_variant)
})

test_that(desc = "works with subject id", code = {
  expect_subject_id_invariance(
    two_sample_test,
    filter(data_with_subid, condition %in% c(1, 5)),
    type = "np"
  )
})
