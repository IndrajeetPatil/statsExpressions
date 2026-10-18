patrick::with_parameters_test_that(
  "parametric oneway_anova() works:",
  {
    # `exec()` inlines the column names, as if they were typed in the call
    df <- rlang::exec(
      oneway_anova,
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
      "between-subjects without NAs, Welch",
      "between-subjects without NAs, Fisher",
      "between-subjects with NAs, partial omega-squared",
      # `var.equal` shouldn't make a difference
      "within-subjects without NAs"
    ),
    data = list(mtcars, mtcars, msleep, iris_long),
    x = c("cyl", "cyl", "vore", "condition"),
    y = c("wt", "wt", "brainwt", "value"),
    paired = c(FALSE, FALSE, FALSE, TRUE),
    effsize.type = c("eta", "eta", "unbiased", "omega"),
    var.equal = c(FALSE, TRUE, FALSE, FALSE),
    conf.level = c(0.95, 0.95, 0.95, 0.99),
    digits = c(5L, 5L, 4L, 3L)
  )
)
