test_that(
  desc = "switch for stats type works",
  code = {
    expect_identical(extract_stats_type("p"), "parametric")
    expect_identical(extract_stats_type("pearson"), "parametric")
    expect_identical(extract_stats_type("non-parametric"), "nonparametric")
    expect_identical(extract_stats_type("np"), "nonparametric")
    expect_identical(extract_stats_type("r"), "robust")
    expect_identical(extract_stats_type("bf"), "bayes")
    expect_identical(extract_stats_type("xxx"), "parametric")
  }
)
