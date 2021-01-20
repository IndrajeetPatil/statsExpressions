# subtitle from meta-analysis -------------------------------------------

test_that(
  desc = "expr_meta_random works",
  code = {
    skip_if(getRversion() < "3.6")

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

    # subtitle output
    set.seed(123)
    using_function1 <-
      expr_meta_random(
        data = df_eg,
        k = 4,
        output = "subtitle"
      )

    # dataframe output
    set.seed(123)
    df_res <-
      expr_meta_random(
        data = df_eg,
        output = "dataframe"
      )

    # output
    expect_s3_class(df_res, "tbl_df")

    # expected subtitle output
    set.seed(123)
    results1 <-
      ggplot2::expr(
        paste(
          italic("z"),
          " = ",
          "2.1697",
          ", ",
          italic("p"),
          " = ",
          "0.0300",
          ", ",
          widehat(beta)["summary"]^"meta",
          " = ",
          "0.4377",
          ", CI"["95%"],
          " [",
          "0.0423",
          ", ",
          "0.8331",
          "]",
          ", ",
          italic("n")["effects"],
          " = ",
          "5"
        )
      )

    # testing overall call
    expect_identical(using_function1, results1)

    # error
    expect_error(expr_meta_random(mtcars))
  }
)
