patrick::with_parameters_test_that(
  "t_robust works:",
  {
    set.seed(123)
    # `exec()` inlines the column names, as if they were typed in the call
    df <- rlang::exec(
      two_sample_test,
      type = "r",
      data = data,
      x = x,
      y = y,
      paired = paired,
      conf.level = conf.level,
      digits = digits
    )

    expect_snapshot(select(df, -expression))
    expect_snapshot(df[["expression"]])
  },
  .cases = dplyr::tibble(
    .test_name = c(
      "within-subjects without NAs",
      "within-subjects with NAs",
      "between-subjects without NAs",
      "between-subjects with NAs"
    ),
    data = list(
      filter(iris_long, condition %in% c("Sepal.Length", "Sepal.Width")),
      filter(bugs_long, condition %in% c("HDHF", "HDLF")),
      mtcars,
      filter(msleep, vore %in% c("carni", "herbi"))
    ),
    x = c("condition", "condition", "am", "vore"),
    y = c("value", "desire", "wt", "brainwt"),
    paired = c(TRUE, TRUE, FALSE, FALSE),
    conf.level = c(0.95, 0.95, 0.99, 0.90),
    digits = c(4L, 3L, 3L, 4L)
  )
)

test_that("works with subject id", {
  expect_subject_id_invariance(
    two_sample_test,
    data_with_subid_two_groups,
    type = "r"
  )
})
