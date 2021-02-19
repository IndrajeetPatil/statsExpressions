if (require("metaplus")) {

  # subtitle from meta-analysis -------------------------------------------

  test_that(
    desc = "expr_meta_random works",
    code = {
      skip_if(getRversion() < "4.0")

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
        )

      # output
      expect_s3_class(df_res, "tbl_df")

      # test
      expect_identical(
        results1$expression[[1]],
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
            "6"
          )
        )
      )
    }
  )
}
