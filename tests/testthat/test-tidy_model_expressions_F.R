test_that("tidy_model_expressions works - F", {
  options(tibble.width = Inf)

  ## F-statistic --------------------------------

  set.seed(123)
  mod_f <- aov(yield ~ N * P + Error(block), npk)

  set.seed(123)
  df1 <- tidy_model_expressions(
    tidy_model_parameters(mod_f,
      omega_squared = "partial",
      table_wide = TRUE
    ),
    statistic = "f"
  )

  expect_snapshot(dplyr::select(df1, -label))
  expect_snapshot(df1$label)

  set.seed(123)
  df2 <- tidy_model_expressions(
    tidy_model_parameters(mod_f,
      eta_squared = "partial",
      table_wide = TRUE
    ),
    statistic = "f",
    effsize.type = "eta"
  )

  expect_snapshot(dplyr::select(df2, -label))
  expect_snapshot(df2$label)
})
