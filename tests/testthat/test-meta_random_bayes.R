# there is z in this file's name for a reason
# Bayesian meta-analysis often crashes R session on macOS
# this makes sure this test will be run after all others



test_that(
  desc = "meta_analysis works - bayesian",
  code = {
    skip_if_not_installed("metaBMA")
    skip_if_not_installed("metaplus")

    data(mag, package = "metaplus")
    dat <- mag %>% rename(estimate = yi, std.error = sei)

    set.seed(123)
    df <- suppressWarnings(meta_analysis(
      type = "bayes",
      data = dat,
      k = 3L,
      iter = 1000L,
      summarize = "integrate"
    ))

    expect_type(df, "list")

    expect_snapshot(dplyr::select(df, -expression))
    expect_snapshot(df[["expression"]])
  }
)
