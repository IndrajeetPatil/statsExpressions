# there is z in this file's name for a reason
# Bayesian meta-analysis often crashes R session on macOS
# this makes sure this test will be run after all others

withr::local_options(list(tibble.width = Inf))

test_that(
  desc = "meta_analysis works - bayesian",
  code = {
    skip_if_not_installed("metaBMA")

    # setup
    set.seed(123)
    df1 <- structure(
      .Data = list(
        term = c("1", "2", "3", "4", "5"),
        estimate = c(
          0.382047603321706,
          0.780783111514665,
          0.425607573765058,
          0.558365541235078,
          0.956473848429961
        ),
        std.error = c(
          0.0465576338644502,
          0.0330218199731529,
          0.0362834986178494,
          0.0480571500648261,
          0.062215818388157
        )
      ),
      row.names = c(NA, -5L),
      class = c("tbl_df", "tbl", "data.frame")
    )

    # getting bayesian in favor of null hypothesis
    set.seed(123)
    df <- suppressWarnings(meta_analysis(
      type = "bayes",
      data = df1,
      k = 3,
      iter = 1000,
      summarize = "integrate"
    ))

    expect_type(df, "list")

    expect_snapshot(dplyr::select(df, -expression))
    expect_snapshot(df[["expression"]])
  }
)
