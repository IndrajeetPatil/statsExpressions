test_that("t_nonparametric works - between-subjects design", {
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

test_that("nonparametric works - within-subjects design", {
  df <- suppressWarnings(two_sample_test(
    data = bugs_long_two_groups,
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
