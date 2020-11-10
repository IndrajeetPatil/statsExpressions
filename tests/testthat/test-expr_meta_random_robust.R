# subtitle from meta-analysis -------------------------------------------

testthat::test_that(
  desc = "expr_meta_random works",
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
      expr_meta_random(
        data = df,
        random = "normal",
        type = "robust",
        k = 4
      )

    # df
    set.seed(123)
    df_res <-
      expr_meta_random(
        data = df,
        type = "robust",
        random = "normal",
        output = "dataframe"
      )

    # output
    testthat::expect_s3_class(df_res, "tbl_df")

    # test
    testthat::expect_identical(
      results1,
      ggplot2::expr(
        paste(
          italic("z"),
          " = ",
          "-1.8844",
          ", ",
          italic("p"),
          " = ",
          "0.0230",
          ", ",
          widehat(beta)["summary"]^"meta",
          " = ",
          "-0.6930",
          ", CI"["95%"],
          " [",
          "-1.5596",
          ", ",
          "-0.1180",
          "]",
          ", ",
          italic("n")["effects"],
          " = ",
          6L
        )
      )
    )
  }
)
