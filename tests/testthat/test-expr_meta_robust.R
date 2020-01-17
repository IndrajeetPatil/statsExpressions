context("expr_meta_robust")

# subtitle from meta-analysis -------------------------------------------

testthat::test_that(
  desc = "expr_meta_robust works",
  code = {
    testthat::skip_if(getRversion() < "3.6")

    # setup
    library(metaplus)

    # renaming to what `statsExpressions` expects
    df <- dplyr::rename(mag, estimate = yi, std.error = sei)

    # subtitle
    set.seed(123)
    results1 <-
      expr_meta_robust(
        data = df,
        random = "normal",
        k = 4,
        messages = TRUE
      )

    # test
    testthat::expect_identical(
      results1,
      ggplot2::expr(
        paste(
          "Summary effect: ",
          italic("z"),
          " = ",
          "NA",
          ", ",
          italic("p"),
          " = ",
          "5e-04",
          ", ",
          widehat(beta),
          " = ",
          "-0.7463",
          ", CI"["95%"],
          " [",
          "-1.2583",
          ", ",
          "-0.3428",
          "]",
          ", ",
          italic("n")["effects"],
          " = ",
          16L
        )
      )
    )
  }
)
