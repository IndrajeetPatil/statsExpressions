test_that("tidy_model_expressions works - F", {
  options(tibble.width = Inf)

  ## F-statistic --------------------------------

  if (packageVersion("parameters") >= "0.14.0.1") {
    set.seed(123)
    mod_f <- aov(yield ~ N * P + Error(block), npk)

    set.seed(123)
    expect_snapshot(tidy_model_expressions(
      tidy_model_parameters(mod_f,
        omega_squared = "partial",
        table_wide = TRUE
      ),
      statistic = "f"
    ))

    set.seed(123)
    expect_snapshot(tidy_model_expressions(
      tidy_model_parameters(mod_f,
        eta_squared = "partial",
        table_wide = TRUE
      ),
      statistic = "f",
      effsize.type = "eta"
    ))
  }
})
