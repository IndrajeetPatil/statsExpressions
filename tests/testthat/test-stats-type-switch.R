test_that("switch for stats type works", {
  expected <- c(
    p = "parametric",
    pearson = "parametric",
    `non-parametric` = "nonparametric",
    np = "nonparametric",
    r = "robust",
    bf = "bayes",
    xxx = "parametric"
  )

  expect_identical(extract_stats_type(names(expected)), unname(expected))
})
