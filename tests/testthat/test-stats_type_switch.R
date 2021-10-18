
# switch for stats type works ------------------------------------------

test_that(
  desc = "switch for stats type works",
  code = {
    expect_identical(stats_type_switch("p"), "parametric")
    expect_identical(stats_type_switch("pearson"), "parametric")
    expect_identical(stats_type_switch("non-parametric"), "nonparametric")
    expect_identical(stats_type_switch("np"), "nonparametric")
    expect_identical(stats_type_switch("r"), "robust")
    expect_identical(stats_type_switch("bf"), "bayes")
    expect_identical(stats_type_switch("xxx"), "parametric")
  }
)
