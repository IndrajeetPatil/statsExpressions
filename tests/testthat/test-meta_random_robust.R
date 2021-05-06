if (require("metaplus")) {

  # subtitle from meta-analysis -------------------------------------------

  test_that(
    desc = "meta_analysis works",
    code = {
      options(tibble.width = Inf)
      skip_if(getRversion() < "4.0")

      # renaming to what `statsExpressions` expects
      set.seed(123)
      dat <-
        mag %>%
        dplyr::rename(estimate = yi, std.error = sei) %>%
        dplyr::sample_frac(0.4)

      # df
      set.seed(123)
      df <-
        meta_analysis(
          data = dat,
          type = "robust",
          random = "normal",
        )

      # testing all details
      set.seed(123)
      expect_snapshot(dplyr::select(df, -expression))
      expect_snapshot(df$expression[[1]])
    }
  )
}
