# don't test data frames because the values vary across platforms, even with the
# same seed; for the same reason, don't change number of digits

# between-subjects ------------------------------

skip_if_not_installed("rstantools")

test_that(desc = "bayesian (between-subjects - anova)", code = {
  # with NA
  set.seed(123)
  df1 <- suppressWarnings(oneway_anova(
    type = "bayes",
    data = msleep,
    x = vore,
    y = brainwt
  ))

  expect_snapshot(dim(df1))
  expect_snapshot(df1[["expression"]][[1]])

  # without NA
  set.seed(123)
  df2 <- suppressWarnings(oneway_anova(
    type = "bayes",
    data = iris,
    x = Species,
    y = Sepal.Length
  ))

  expect_snapshot(dim(df2))
  expect_snapshot(df2[["expression"]][[1]])
})

# within-subjects ------------------------------

test_that(desc = "bayesian (within-subjects - anova)", code = {
  set.seed(123)
  df1 <- oneway_anova(
    type = "bayes",
    data = WRS2::WineTasting,
    x = Wine,
    y = Taste,
    paired = TRUE,
    bf.prior = 0.88
  )

  expect_snapshot(dim(df1))
  expect_snapshot(df1[["expression"]][[1]])

  # data with NA
  set.seed(123)
  df2 <- oneway_anova(
    type = "bayes",
    data = bugs_long,
    x = condition,
    y = desire,
    paired = TRUE
  )

  expect_snapshot(dim(df2))
  expect_snapshot(df2[["expression"]][[1]])
})

test_that(desc = "works with subject id", code = {
  expect_subject_id_invariance(oneway_anova, data_with_subid, type = "bayes")
})

# test Bayesian ANOVA helper behavior --------

test_that(desc = "Bayesian ANOVA with if_all() filtering and row replication", code = {
  # Test between-subjects design
  # This ensures the if_all(matches("effect")) filtering behaves correctly
  # when effect/component columns are absent (they appear in the within-subjects case)
  set.seed(123)
  df_between <- suppressWarnings(oneway_anova(
    type = "bayes",
    data = iris,
    x = Species,
    y = Sepal.Length
  ))

  # Verify structure is correct after filtering and binding
  expect_contains(colnames(df_between), c("estimate", "conf.low", "conf.high"))
  expect_snapshot(colnames(df_between))
  expect_snapshot(nrow(df_between))

  # Test within-subjects design
  # This tests both if_all filters and the row replication logic
  set.seed(123)
  df_within <- suppressWarnings(oneway_anova(
    type = "bayes",
    data = WRS2::WineTasting,
    x = Wine,
    y = Taste,
    paired = TRUE
  ))

  # Should have same columns and proper row count
  expect_contains(colnames(df_within), c("estimate", "conf.low", "conf.high"))
  expect_snapshot(colnames(df_within))
  expect_snapshot(nrow(df_within))
})

# test tidy_model_parameters with Bayesian ANOVA --------

test_that(desc = "tidy_model_parameters handles Bayesian ANOVA correctly", code = {
  # Test between-subjects design (no component column)
  set.seed(123)
  model_between <- BayesFactor::anovaBF(Sepal.Length ~ Species, data = iris)

  result_between <- tidy_model_parameters(model_between)

  expect_contains(
    colnames(result_between),
    c("estimate", "conf.low", "conf.high", "method")
  )
  expect_identical(
    result_between$method[[1]],
    "Bayes factors for linear models"
  )

  # Verify if_all behavior: when component column doesn't exist (between-subjects), keeps all rows
  expect_snapshot(dim(result_between))
})
