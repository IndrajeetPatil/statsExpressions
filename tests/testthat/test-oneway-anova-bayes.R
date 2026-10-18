# don't test data frames because the values vary across platforms, even with the
# same seed; for the same reason, don't change number of digits

# column names cover the `if_all(matches("effect"))` filtering, which must keep
# every row when the effect/component columns are absent (between-subjects)
patrick::with_parameters_test_that(
  "Bayesian oneway_anova() works:",
  {
    set.seed(123)
    # `exec()` inlines the column names, as if they were typed in the call
    df <- rlang::exec(
      oneway_anova,
      type = "bayes",
      data = data,
      x = x,
      y = y,
      paired = paired,
      bf.prior = bf.prior
    )

    expect_snapshot(dim(df))
    expect_snapshot(colnames(df))
    expect_snapshot(df[["expression"]][[1]])
  },
  .cases = dplyr::tibble(
    .test_name = c(
      "between-subjects with NAs",
      "between-subjects without NAs",
      "within-subjects without NAs",
      "within-subjects with NAs"
    ),
    data = list(msleep, iris, WRS2::WineTasting, bugs_long),
    x = c("vore", "Species", "Wine", "condition"),
    y = c("brainwt", "Sepal.Length", "Taste", "desire"),
    paired = c(FALSE, FALSE, TRUE, TRUE),
    bf.prior = c(0.707, 0.707, 0.88, 0.707)
  )
)

test_that("tidy_model_parameters handles Bayesian ANOVA correctly", {
  set.seed(123)
  model <- BayesFactor::anovaBF(Sepal.Length ~ Species, data = iris)

  result <- tidy_model_parameters(model)

  expect_contains(colnames(result), c("estimate", "conf.low", "conf.high"))
  expect_identical(result$method[[1]], "Bayes factors for linear models")
  # without a component column (between-subjects), all rows are kept
  expect_snapshot(dim(result))
})
