withr::local_options(list(tibble.width = Inf))

test_that(
  desc = "meta_analysis works - robust",
  code = {
    skip_if_not_installed("metaplus")

    data(mag, package = "metaplus")
    dat <- mag %>% rename(estimate = yi, std.error = sei)

    set.seed(123)
    df <- meta_analysis(
      data = dat,
      type = "robust",
      random = "normal"
    )

    set.seed(123)
    expect_snapshot(select(df, -expression))
    expect_snapshot(df[["expression"]])
  }
)
