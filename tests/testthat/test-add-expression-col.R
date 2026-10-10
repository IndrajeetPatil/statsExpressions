# Columns in `data` named like the function's arguments must not shadow them,
# so every case must give the same expression.

patrick::with_parameters_test_that(
  "add_expression_col() ignores a column named:",
  {
    data <- dplyr::tibble(
      statistic = 2.3456,
      df = 1,
      df.error = 10,
      p.value = 0.0312,
      estimate = 0.4,
      conf.level = 0.95,
      conf.low = 0.1,
      conf.high = 0.7,
      method = "Student's t-test",
      effectsize = "Cohen's d"
    )
    data[[column]] <- "x"

    df <- add_expression_col(data, n = 20L)

    expect_snapshot(df[["expression"]])
  },
  .cases = dplyr::tibble(
    column = c(
      "digits",
      "digits.df",
      "digits.df.error",
      "n",
      "n.text",
      "statistic.text",
      "effsize.text"
    ),
    .test_name = paste0("`", column, "`")
  )
)

test_that("add_expression_col() ignores a column named `digits` (Bayesian)", {
  bayes_df <- dplyr::tibble(
    bf10 = 3,
    estimate = 0.4,
    conf.level = 0.95,
    conf.low = 0.1,
    conf.high = 0.7,
    conf.method = "ETI",
    method = "Bayesian t-test",
    prior.scale = 0.707,
    effectsize = "Cohen's d",
    n.obs = 20L
  )

  expect_identical(
    add_expression_col(dplyr::mutate(bayes_df, digits = "x"))[["expression"]],
    add_expression_col(bayes_df)[["expression"]]
  )
})
