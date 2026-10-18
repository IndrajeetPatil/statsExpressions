patrick::with_parameters_test_that(
  "robust oneway_anova() works:",
  {
    set.seed(123)
    # `exec()` inlines the column names, as if they were typed in the call
    df <- rlang::exec(
      oneway_anova,
      type = "robust",
      data = data,
      x = x,
      y = y,
      paired = paired,
      conf.level = conf.level,
      digits = digits,
      tr = tr,
      nboot = nboot
    )

    expect_snapshot(select(df, -expression))
    expect_snapshot(df[["expression"]])
  },
  .cases = dplyr::tibble(
    .test_name = c(
      "between-subjects without NAs",
      "between-subjects with NAs",
      "within-subjects with NAs",
      "within-subjects with few cases"
    ),
    data = list(
      mtcars,
      filter(msleep, vore != "insecti"),
      bugs_long,
      few_cases
    ),
    x = c("cyl", "vore", "condition", "groups"),
    y = c("wt", "sleep_total", "desire", "values"),
    paired = c(FALSE, FALSE, TRUE, TRUE),
    conf.level = c(0.95, 0.99, 0.95, 0.99),
    digits = c(5L, 4L, 4L, 2L),
    tr = c(0.00025, 0.2, 0.2, 0.2),
    nboot = c(2L, 15L, 100L, 100L)
  )
)
