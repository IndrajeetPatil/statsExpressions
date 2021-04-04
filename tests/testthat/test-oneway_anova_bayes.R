# bayes factor (between-subjects - anova) ---------------------------------

test_that(
  desc = "bayes factor (between-subjects - anova)",
  code = {
    skip_if(getRversion() < "4.0")
    skip_on_cran()

    # extracting results from where this function is implemented
    set.seed(123)
    df_results <-
      oneway_anova(
        type = "bayes",
        data = ggplot2::msleep,
        x = "vore",
        y = brainwt,
        bf.prior = 0.99
      )

    # extracting expr
    set.seed(123)
    results <-
      oneway_anova(
        type = "bayes",
        data = ggplot2::msleep,
        x = vore,
        y = "brainwt",
        bf.prior = 0.88,
        k = 2 # don't change; tests fail on Ubuntu otherwise
      )

    # check bayes factor values
    expect_equal(df_results$bf10[[1]], 0.1177186, tolerance = 0.001)

    # call
    expect_identical(
      results$expression[[1]],
      ggplot2::expr(
        paste(
          "log"["e"] * "(BF"["01"] * ") = " * "1.92" * ", ",
          widehat(italic(R^"2"))["Bayesian"]^"posterior" * " = " * "0.00" * ", ",
          "CI"["95%"]^"HDI" * " [" * "0.00" * ", " * "0.08" * "], ",
          italic("r")["Cauchy"]^"JZS" * " = " * "0.88"
        )
      )
    )

    # data where it works
    set.seed(123)
    results2 <-
      oneway_anova(
        type = "bayes",
        data = iris,
        x = Species,
        y = Sepal.Length,
        conf.level = 0.99,
        conf.method = "eti",
        k = 2 # don't change; tests fail on Ubuntu otherwise
      )

    expect_identical(
      results2$expression[[1]],
      ggplot2::expr(
        paste(
          "log"["e"] * "(BF"["01"] * ") = " * "-65.10" * ", ",
          widehat(italic(R^"2"))["Bayesian"]^"posterior" * " = " * "0.61" * ", ",
          "CI"["99%"]^"HDI" * " [" * "0.51" * ", " * "0.68" * "], ",
          italic("r")["Cauchy"]^"JZS" * " = " * "0.71"
        )
      )
    )
  }
)

# bayes factor (within-subjects - anova) ---------------------------------

test_that(
  desc = "bayes factor (within-subjects - anova)",
  code = {
    skip_on_cran()

    if (utils::packageVersion("BayesFactor") >= package_version("0.9.12-4.3")) {

      # dataframe
      dat <-
        structure(list(Taste = c(
          5.4, 5.5, 5.55, 5.85, 5.7, 5.75, 5.2,
          5.6, 5.5, 5.55, 5.5, 5.4, 5.9, 5.85, 5.7, 5.45, 5.55, 5.6, 5.4,
          5.4, 5.35, 5.45, 5.5, 5.35, 5.25, 5.15, 5, 5.85, 5.8, 5.7, 5.25,
          5.2, 5.1, 5.65, 5.55, 5.45, 5.6, 5.35, 5.45, 5.05, 5, 4.95, 5.5,
          5.5, 5.4, 5.45, 5.55, 5.5, 5.55, 5.55, 5.35, 5.45, 5.5, 5.55,
          5.5, 5.45, 5.25, 5.65, 5.6, 5.4, 5.7, 5.65, 5.55, 6.3, 6.3, 6.25
        ), Wine = structure(c(
          1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L,
          2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L,
          3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L,
          1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L,
          2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L
        ), .Label = c(
          "Wine A", "Wine B",
          "Wine C"
        ), class = "factor"), Taster = structure(c(
          1L, 1L, 1L,
          2L, 2L, 2L, 3L, 3L, 3L, 4L, 4L, 4L, 5L, 5L, 5L, 6L, 6L, 6L, 7L,
          7L, 7L, 8L, 8L, 8L, 9L, 9L, 9L, 10L, 10L, 10L, 11L, 11L, 11L,
          12L, 12L, 12L, 13L, 13L, 13L, 14L, 14L, 14L, 15L, 15L, 15L, 16L,
          16L, 16L, 17L, 17L, 17L, 18L, 18L, 18L, 19L, 19L, 19L, 20L, 20L,
          20L, 21L, 21L, 21L, 22L, 22L, 22L
        ), .Label = c(
          "1", "2", "3",
          "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15",
          "16", "17", "18", "19", "20", "21", "22"
        ), class = "factor")), row.names = c(
          NA,
          -66L
        ), class = "data.frame")

      # extracting results from where this function is implemented
      set.seed(123)
      df_results <-
        oneway_anova(
          type = "bayes",
          data = dat,
          x = Wine,
          y = "Taste",
          paired = TRUE,
          bf.prior = 0.99
        )

      # check bayes factor values
      expect_equal(df_results$bf10[[1]], 6.364917, tolerance = 0.001)

      # extracting expression
      set.seed(123)
      results <-
        oneway_anova(
          type = "bayes",
          data = dat,
          x = "Wine",
          y = Taste,
          k = 2, # don't change; tests fail on Ubuntu otherwise
          paired = TRUE,
          bf.prior = 0.88
        )

      # data with NA
      set.seed(123)
      results_na <-
        oneway_anova(
          type = "bayes",
          data = bugs_long,
          x = condition,
          y = "desire",
          paired = TRUE
        )

      # testing expression
      expect_type(results$expression[[1]], "language")
      expect_type(results_na$expression[[1]], "language")

      # checking expressions
      expect_identical(
        results$expression[[1]],
        ggplot2::expr(
          paste(
            "log"["e"] * "(BF"["01"] * ") = " * "-1.96" * ", ",
            widehat(italic(R^"2"))["Bayesian"]^"posterior" * " = " * "0.89" * ", ",
            "CI"["95%"]^"HDI" * " [" * "0.85" * ", " * "0.92" * "], ",
            italic("r")["Cauchy"]^"JZS" * " = " * "0.88"
          )
        )
      )

      expect_identical(
        results_na$expression[[1]],
        ggplot2::expr(
          paste(
            "log"["e"] * "(BF"["01"] * ") = " * "-21.04" * ", ",
            widehat(italic(R^"2"))["Bayesian"]^"posterior" * " = " * "0.53" * ", ",
            "CI"["95%"]^"HDI" * " [" * "0.46" * ", " * "0.59" * "], ",
            italic("r")["Cauchy"]^"JZS" * " = " * "0.71"
          )
        )
      )

      # with subject.id ---------------------------------

      # data
      df <-
        structure(list(
          score = c(
            70, 82.5, 97.5, 100, 52.5, 62.5,
            92.5, 70, 90, 92.5, 90, 75, 60, 90, 85, 67.5, 90, 72.5, 45, 60,
            72.5, 80, 100, 100, 97.5, 95, 65, 87.5, 90, 62.5, 100, 100, 97.5,
            100, 97.5, 95, 82.5, 82.5, 40, 92.5, 85, 72.5, 35, 27.5, 82.5
          ), condition = structure(c(
            5L, 1L, 2L, 3L, 4L, 4L, 5L, 1L,
            2L, 3L, 2L, 3L, 3L, 4L, 2L, 1L, 5L, 5L, 4L, 1L, 1L, 4L, 3L, 5L,
            2L, 5L, 1L, 2L, 3L, 4L, 4L, 5L, 1L, 2L, 3L, 2L, 3L, 4L, 1L, 5L,
            3L, 2L, 5L, 4L, 1L
          ), .Label = c("1", "2", "3", "4", "5"), class = "factor"),
          id = structure(c(
            1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L,
            2L, 3L, 3L, 4L, 3L, 4L, 3L, 4L, 3L, 4L, 4L, 5L, 5L, 5L, 5L,
            5L, 6L, 6L, 6L, 6L, 6L, 7L, 7L, 7L, 7L, 7L, 8L, 8L, 8L, 8L,
            8L, 9L, 9L, 9L, 9L, 9L
          ), .Label = c(
            "1", "2", "3", "4", "5",
            "6", "7", "8", "9"
          ), class = "factor")
        ), row.names = c(
          NA,
          45L
        ), class = "data.frame")

      # incorrect
      set.seed(123)
      expr1 <-
        oneway_anova(
          type = "bayes",
          data = df,
          x = condition,
          y = score,
          subject.id = id,
          paired = TRUE
        )

      # correct
      set.seed(123)
      expr2 <-
        oneway_anova(
          type = "bayes",
          data = dplyr::arrange(df, id),
          x = condition,
          y = score,
          paired = TRUE
        )

      expect_equal(expr2, expr1)
    }
  }
)
