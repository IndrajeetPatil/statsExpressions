context("expr_meta_bayes")

# subtitle from meta-analysis -------------------------------------------

testthat::test_that(
  desc = "expr_meta_bayes works",
  code = {
    testthat::skip_if(getRversion() < "3.6")

    # setup
    set.seed(123)
    library(metaBMA)

    # creating a dataframe
    df1 <-
      structure(
        .Data = list(
          estimate = c(
            0.382047603321706,
            0.780783111514665,
            0.425607573765058,
            0.558365541235078,
            0.956473848429961,
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
            0.062215818388157,
            0.0465576338644502,
            0.0330218199731529,
            0.0362834986178494,
            0.0480571500648261,
            0.062215818388157
          )
        ),
        row.names = c(NA, -10L),
        class = c("tbl_df", "tbl", "data.frame")
      )

    # subtitle
    set.seed(123)
    results1 <-
      capture.output(suppressWarnings(expr_meta_bayes(
        data = df1,
        k = 3,
        messages = TRUE,
        iter = 1000
      )))

    # test
    testthat::expect_identical(results1[8], "  random_H0         1  0.000234")
    testthat::expect_identical(results1[9], "  random_H1      4269  1.000000")
  }
)
