test_that(
  desc = "meta_analysis works - bayesian",
  code = {
    skip_if_not_installed("metaBMA")

    set.seed(123)
    df <- meta_analysis(
      type = "bayes",
      data = data_meta,
      digits = 3L,
      iter = 1000L,
      summarize = "integrate"
    )

    expect_type(df, "list")

    expect_snapshot(dplyr::select(df, -expression))
    expect_snapshot(df[["expression"]])
  }
)
