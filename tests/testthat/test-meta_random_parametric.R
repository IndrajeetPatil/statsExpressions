withr::local_options(list(tibble.width = Inf))

test_that(
  desc = "meta_analysis works - parametric",
  code = {
    skip_if_not_installed("metafor")
    skip_if_not_installed("metaplus")

    data(mag, package = "metaplus")
    dat <- mag %>% rename(estimate = yi, std.error = sei)

    set.seed(123)
    df <- suppressWarnings(meta_analysis(dat))

    set.seed(123)
    expect_snapshot(select(df, -expression))
    expect_snapshot(df[["expression"]])

    expect_error(suppressWarnings(meta_analysis(mtcars)))
  }
)
