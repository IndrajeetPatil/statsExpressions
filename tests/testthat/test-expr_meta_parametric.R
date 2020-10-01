# subtitle from meta-analysis -------------------------------------------

testthat::test_that(
  desc = "expr_meta_parametric works",
  code = {
    testthat::skip_if(getRversion() < "3.6")

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
      expr_meta_parametric(
        data = df_eg,
        k = 4,
        messages = FALSE,
        output = "subtitle"
      )

    # caption output
    set.seed(123)
    using_function2 <-
      expr_meta_parametric(
        data = df_eg,
        k = 2,
        caption = "this is caption",
        messages = FALSE,
        output = "caption"
      )

    # expected subtitle output
    set.seed(123)
    results1 <-
      ggplot2::expr(
        paste(
          "Summary effect: ",
          italic("z"),
          " = ",
          "2.1697",
          ", ",
          italic("p"),
          " = ",
          "0.0300",
          ", ",
          widehat(beta),
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
          5L
        )
      )

    # expected subtitle output
    set.seed(123)
    results2 <-
      ggplot2::expr(atop(
        displaystyle("this is caption"),
        expr = paste(
          "Heterogeneity: ",
          italic("Q"),
          "(",
          "4",
          ") = ",
          "6083",
          ", ",
          italic("p"),
          " = ",
          "0e+00",
          ", ",
          tau["REML"]^2,
          " = ",
          "0.19",
          ", ",
          "I"^2,
          " = ",
          "99.90%"
        )
      ))

    # testing overall call
    testthat::expect_identical(using_function1, results1)
    testthat::expect_identical(using_function2, results2)

    # error
    testthat::expect_error(expr_meta_parametric(mtcars))
  }
)
