if (require("metafor")) {
  test_that(
    desc = "meta_analysis works - parametric",
    code = {
      options(tibble.width = Inf)
      skip_if(getRversion() < "4.0")

      # dataframe
      df_eg <-
        structure(
          list(
            estimate = c(0.111, 0.245, 0.8, 1.1, 0.03),
            std.error = c(0.05, 0.111, 0.001, 0.2, 0.01)
          ),
          row.names = c(NA, -5L),
          class = c("tbl_df", "tbl", "data.frame")
        )

      # dataframe output
      set.seed(123)
      df <- meta_analysis(df_eg)

      # testing all details
      set.seed(123)
      expect_snapshot(dplyr::select(df, -expression))
      expect_snapshot(df$expression[[1]])

      # error
      expect_error(meta_analysis(mtcars))
    }
  )
}
