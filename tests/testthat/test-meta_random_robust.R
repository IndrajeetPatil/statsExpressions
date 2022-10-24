withr::local_options(list(tibble.width = Inf))

test_that(
  desc = "meta_analysis works - robust",
  code = {
    skip_if_not_installed("metaplus")

    # renaming to what `{statsExpressions}` expects
    set.seed(123)
    data(mag, package = "metaplus")
    dat <-
      mag %>%
      rename(estimate = yi, std.error = sei) %>%
      sample_frac(0.4)

    # df
    set.seed(123)
    df <- meta_analysis(
      data = dat,
      type = "robust",
      random = "normal",
    )

    set.seed(123)
    expect_snapshot(select(df, -expression))
    expect_snapshot(df[["expression"]])
  }
)
