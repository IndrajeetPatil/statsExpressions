context("expr_meta_robust")

# subtitle from meta-analysis -------------------------------------------

testthat::test_that(
  desc = "expr_meta_robust works",
  code = {
    testthat::skip_if(getRversion() < "3.6")

    # setup
    library(metaplus)

    # renaming to what `statsExpressions` expects
    set.seed(123)
    df <-
      mag %>%
      dplyr::rename(., estimate = yi, std.error = sei) %>%
      dplyr::sample_frac(., 0.4)

    # subtitle
    set.seed(123)
    results1 <-
      suppressWarnings(expr_meta_bayes(
        data = df,
        k = 4,
        messages = FALSE
      ))

    # test
    testthat::expect_identical(
      results1,
      ggplot2::expr(atop(
        displaystyle(NULL),
        expr = paste(
          "In favor of alternative: ",
          "log"["e"],
          "(BF"["10"],
          ") = ",
          "0.9700",
          ", ",
          italic("d")["mean"]^"posterior",
          " = ",
          "-0.3070",
          ", CI"["95%"],
          " [",
          "-0.7024",
          ", ",
          "0.0926",
          "]"
        )
      ))
    )
  }
)
