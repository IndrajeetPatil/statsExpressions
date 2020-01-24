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

    # subtitle
    set.seed(123)
    results1 <-
      suppressWarnings(expr_meta_bayes(
        data = df1,
        k = 3,
        messages = TRUE,
        iter = 1000,
        summarize = "integrate"
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
          "3.341",
          ", ",
          italic("d")["mean"]^"posterior",
          " = ",
          "0.514",
          ", CI"["95%"],
          " [",
          "0.220",
          ", ",
          "0.761",
          "]"
        )
      ))
    )
  }
)
