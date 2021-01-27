# subtitle from meta-analysis -------------------------------------------

test_that(
  desc = "expr_meta_random works",
  code = {
    skip_if(getRversion() < "4.0")
    skip_on_cran()

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

    # getting bayes factor in favor of null hypothesis
    set.seed(123)
    subtitle1 <-
      suppressWarnings(expr_meta_random(
        type = "bayes",
        data = df1,
        k = 3,
        iter = 1000,
        summarize = "integrate",
        output = "expression"
      ))

    set.seed(123)
    df <-
      suppressWarnings(expr_meta_random(
        type = "bayes",
        data = df1,
        k = 3,
        iter = 1000,
        summarize = "integrate",
        output = "dataframe",
        top.text = "ayyo"
      ))

    expect_type(df, "list")
    expect_identical(class(df), c("tbl_df", "tbl", "data.frame"))

    expect_identical(
      subtitle1,
      ggplot2::expr(
        paste(
          "log"["e"] * "(BF"["01"] * ") = " * "-3.587" * ", ",
          widehat(italic(delta))["mean"]^"posterior" * " = " * "0.596" * ", ",
          "CI"["95%"]^"HDI" * " [" * "0.321" * ", " * "0.854" * "], ",
          italic("r")["Cauchy"]^"JZS" * " = " * "0.707"
        )
      )
    )
  }
)
