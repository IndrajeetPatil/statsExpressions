# subtitle from meta-analysis -------------------------------------------

testthat::test_that(
  desc = "expr_meta_parametric works",
  code = {
    testthat::skip_if(getRversion() < "3.6")

    # dataframe
    df_eg <-
      tibble::tribble(
        ~estimate, ~std.error,
        0.111, 0.05,
        0.245, 0.111,
        0.8, 0.001,
        1.1, 0.2,
        0.03, 0.01
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
          "< 0.001",
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

    # tidy dataframe
    set.seed(123)
    tidy_df <-
      expr_meta_parametric(
        data = df_eg,
        messages = FALSE,
        output = "tidy"
      )

    glance_df <-
      expr_meta_parametric(
        data = df_eg,
        messages = FALSE,
        output = "glance"
      )

    # checking if the tidy output is expected
    testthat::expect_equal(tidy_df$estimate, 0.4376927, tolerance = 0.0001)
    testthat::expect_equal(tidy_df$conf.low, 0.04231262, tolerance = 0.0001)
    testthat::expect_equal(tidy_df$conf.high, 0.8330728, tolerance = 0.0001)

    # checking if the glance output is expected
    testthat::expect_equal(dim(glance_df), c(1L, 12L))
    testthat::expect_equal(glance_df$tau2, 0.193525, tolerance = 0.0001)
    testthat::expect_equal(glance_df$se.tau2, 0.1437787, tolerance = 0.0001)
    testthat::expect_equal(glance_df$QE, 6082.595, tolerance = 0.01)
    testthat::expect_equal(glance_df$k, 5L)
    testthat::expect_equal(glance_df$p, 1L)
    testthat::expect_equal(glance_df$m, 1L)

    # error
    testthat::expect_error(expr_meta_parametric(mtcars))
  }
)
