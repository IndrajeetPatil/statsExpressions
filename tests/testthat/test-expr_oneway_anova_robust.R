
# between-subjects -------------------------------------------------------

test_that(
  desc = "expr_anova_robust works - between-subjects",
  code = {
    skip_if(getRversion() < "3.6")

    # `statsExpressions` output
    set.seed(123)
    using_function1 <-
      expr_oneway_anova(
        type = "robust",
        data = mtcars,
        x = cyl,
        y = "wt",
        paired = FALSE,
        k = 5,
        tr = 0.00025,
        nboot = 2
      )

    # expected output
    results1 <-
      ggplot2::expr(
        paste(
          italic("F")["trimmed-means"],
          "(",
          "2",
          ",",
          "18.97383",
          ") = ",
          "20.24946",
          ", ",
          italic("p"),
          " = ",
          "2e-05",
          ", ",
          widehat(xi),
          " = ",
          "0.85858",
          ", CI"["95%"],
          " [",
          "0.85268",
          ", ",
          "0.86448",
          "]",
          ", ",
          italic("n")["obs"],
          " = ",
          "32"
        )
      )

    # testing overall call
    expect_identical(using_function1, results1)

    # `statsExpressions` output
    set.seed(123)
    using_function2 <-
      suppressWarnings(expr_oneway_anova(
        type = "robust",
        data = dplyr::filter(ggplot2::msleep, vore != "insecti"),
        x = vore,
        y = sleep_total,
        paired = FALSE,
        k = 4,
        nboot = 15,
        conf.level = 0.99
      ))

    # expected output
    results2 <-
      ggplot2::expr(
        paste(
          italic("F")["trimmed-means"],
          "(",
          "2",
          ",",
          "35.1708",
          ") = ",
          "0.2695",
          ", ",
          italic("p"),
          " = ",
          "0.7653",
          ", ",
          widehat(xi),
          " = ",
          "0.1793",
          ", CI"["99%"],
          " [",
          "0.0711",
          ", ",
          "0.5717",
          "]",
          ", ",
          italic("n")["obs"],
          " = ",
          "71"
        )
      )

    # testing overall call
    expect_identical(using_function2, results2)
  }
)

# within-subjects -------------------------------------------------------

test_that(
  desc = "expr_anova_robust works - within-subjects",
  code = {
    skip_if(getRversion() < "3.6")

    # `statsExpressions` output
    set.seed(123)
    using_function1 <-
      expr_oneway_anova(
        type = "robust",
        data = bugs_long,
        x = "condition",
        y = desire,
        k = 4,
        tr = 0.2,
        paired = TRUE
      )

    # expected output
    results1 <-
      ggplot2::expr(
        paste(
          italic("F")["trimmed-means"],
          "(",
          "2.7303",
          ",",
          "144.7051",
          ") = ",
          "20.9752",
          ", ",
          italic("p"),
          " = ",
          "1.146e-10",
          ", ",
          widehat(delta)["R-avg"]^"AKP",
          " = ",
          "0.6635",
          ", CI"["95%"],
          " [",
          "0.4660",
          ", ",
          "0.9707",
          "]",
          ", ",
          italic("n")["pairs"],
          " = ",
          "88"
        )
      )

    # testing overall call
    expect_identical(using_function1, results1)
  }
)


# dataframe -----------------------------------------------------------

test_that(
  desc = "dataframe",
  code = {
    expect_s3_class(
      expr_oneway_anova(
        type = "robust",
        data = mtcars,
        x = cyl,
        y = wt,
        output = "dataframe"
      ),
      "tbl_df"
    )
  }
)
