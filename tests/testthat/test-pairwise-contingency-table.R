# `msleep` has `NA`s; `Titanic` is counts data
patrick::with_parameters_test_that(
  "pairwise_contingency_table() works:",
  {
    # `exec()` inlines the column names, as if they were typed in the call
    df <- rlang::exec(
      pairwise_contingency_table,
      data = data,
      x = x,
      y = y,
      counts = counts,
      p.adjust.method = p.adjust.method,
      digits = digits,
      conf.level = conf.level,
      alternative = alternative
    )

    expect_snapshot(select(df, -expression))
    expect_snapshot(df[["expression"]])
  },
  .cases = dplyr::tibble(
    .test_name = c(
      "basic",
      "with counts",
      "no adjustment",
      "data with NAs",
      "custom conf.level and alternative",
      "custom digits"
    ),
    data = list(mtcars, as.data.frame(Titanic), mtcars, msleep, mtcars, mtcars),
    x = c("cyl", "Class", "cyl", "vore", "cyl", "cyl"),
    y = c("am", "Survived", "am", "conservation", "am", "am"),
    counts = list(NULL, "Freq", NULL, NULL, NULL, NULL),
    p.adjust.method = c("bonferroni", "fdr", "none", "holm", "holm", "BH"),
    digits = c(2L, 2L, 2L, 2L, 2L, 4L),
    conf.level = c(0.95, 0.95, 0.95, 0.95, 0.90, 0.95),
    alternative = c(rep("two.sided", 4L), "greater", "two.sided")
  )
)

test_that("pairwise_contingency_table() adjusts p-values only when asked", {
  df <- pairwise_contingency_table(
    mtcars,
    cyl,
    am,
    p.adjust.method = "bonferroni"
  )
  df_none <- pairwise_contingency_table(
    mtcars,
    cyl,
    am,
    p.adjust.method = "none"
  )

  expect_true(all(df$p.value.adj >= df$p.value))
  expect_identical(df_none$p.value, df_none$p.value.adj)
})

test_that("pairwise_contingency_table works with `pair` as grouping name", {
  expected <- pairwise_contingency_table(mtcars, cyl, am)
  df <- pairwise_contingency_table(rename(mtcars, pair = cyl), pair, am)

  expect_identical(df[["expression"]], expected[["expression"]])
})
