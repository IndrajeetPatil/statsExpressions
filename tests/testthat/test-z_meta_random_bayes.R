# there is z in this file's name for a reason
# Bayesian meta-analysis often crashes R session on macOS
# this makes sure this test will be run after all others

test_that(
  desc = "meta_analysis works - bayesian",
  code = {
    skip_if_not_installed("metaBMA")
    skip_if(getRversion() < "4.0")

    # setup
    set.seed(123)

    # creating a dataframe
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

    # getting bayes factor in favor of null hypothesis
    set.seed(123)
    subtitle1 <- suppressWarnings(meta_analysis(
      type = "bayes",
      data = df1,
      k = 3,
      iter = 1000,
      summarize = "integrate"
    ))

    set.seed(123)
    df <- suppressWarnings(meta_analysis(
      type = "bayes",
      data = df1,
      k = 3,
      iter = 1000,
      summarize = "integrate",
      top.text = "ayyo"
    ))

    expect_type(df, "list")
    expect_identical(class(df), c("tbl_df", "tbl", "data.frame"))

    expect_snapshot(as.character(subtitle1$expression[[1]]))
  }
)
