patrick::with_parameters_test_that(
  "parametric t-test works:",
  {
    # `exec()` inlines the column names, as if they were typed in the call
    df <- rlang::exec(
      two_sample_test,
      data = data,
      x = x,
      y = y,
      paired = paired,
      effsize.type = effsize.type,
      var.equal = var.equal,
      conf.level = conf.level,
      digits = digits
    )

    expect_snapshot(select(df, -expression))
    expect_snapshot(df[["expression"]])
  },
  .cases = dplyr::tibble(
    .test_name = c(
      "between-subjects without NAs",
      "between-subjects with NAs",
      "within-subjects without NAs",
      "within-subjects with NAs"
    ),
    data = list(
      ToothGrowth,
      filter(msleep, vore %in% c("carni", "herbi")),
      filter(iris_long, condition %in% c("Sepal.Length", "Sepal.Width")),
      filter(bugs_long, condition %in% c("HDHF", "HDLF"))
    ),
    x = c("supp", "vore", "condition", "condition"),
    y = c("len", "brainwt", "value", "desire"),
    paired = c(FALSE, FALSE, TRUE, TRUE),
    effsize.type = c("d", "g", "g", "d"),
    var.equal = c(TRUE, FALSE, FALSE, FALSE),
    conf.level = c(0.99, 0.90, 0.50, 0.95),
    digits = c(5L, 3L, 4L, 3L)
  )
)

test_that("works with subject id", {
  expect_subject_id_invariance(
    two_sample_test,
    data_with_subid_two_groups
  )
})
