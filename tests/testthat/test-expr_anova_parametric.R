# parametric anova subtitles (without NAs) -----------------------------------

testthat::test_that(
  desc = "parametric anova subtitles work (without NAs)",
  code = {
    testthat::skip_if(getRversion() < "3.6")

    # statsExpression output
    set.seed(123)
    using_function1 <-
      statsExpressions::expr_anova_parametric(
        data = dplyr::sample_frac(statsExpressions::movies_long, 0.1),
        x = genre,
        y = rating,
        effsize.type = "partial_eta",
        k = 5,
        var.equal = FALSE,
        messages = FALSE
      )

    set.seed(123)
    using_function2 <-
      statsExpressions::expr_anova_parametric(
        data = dplyr::sample_frac(statsExpressions::movies_long, 0.1),
        x = genre,
        y = rating,
        effsize.type = "partial_eta",
        conf.level = 0.90,
        k = 5,
        var.equal = FALSE,
        messages = FALSE
      )

    # expected output
    results1 <-
      ggplot2::expr(
        paste(
          italic("F")["Welch"],
          "(",
          "8",
          ",",
          "46.23423",
          ") = ",
          "4.03133",
          ", ",
          italic("p"),
          " = ",
          "0.00107",
          ", ",
          widehat(eta["p"]^2),
          " = ",
          "0.15181",
          ", CI"["95%"],
          " [",
          "0.02743",
          ", ",
          "0.22672",
          "]",
          ", ",
          italic("n")["obs"],
          " = ",
          158L
        )
      )

    results2 <-
      ggplot2::expr(
        paste(
          italic("F")["Welch"],
          "(",
          "8",
          ",",
          "46.23423",
          ") = ",
          "4.03133",
          ", ",
          italic("p"),
          " = ",
          "0.00107",
          ", ",
          widehat(eta["p"]^2),
          " = ",
          "0.15181",
          ", CI"["90%"],
          " [",
          "0.03902",
          ", ",
          "0.20834",
          "]",
          ", ",
          italic("n")["obs"],
          " = ",
          158L
        )
      )

    # testing overall call
    testthat::expect_identical(using_function1, results1)
    testthat::expect_identical(using_function2, results2)
  }
)

# parametric anova subtitles (with NAs) --------------------------------------

testthat::test_that(
  desc = "parametric anova subtitles work (with NAs)",
  code = {
    testthat::skip_if(getRversion() < "3.6")

    # output from statsExpression helper subtitle
    set.seed(123)
    using_function1 <-
      suppressWarnings(
        statsExpressions::expr_anova_parametric(
          data = ggplot2::msleep,
          x = vore,
          y = sleep_total,
          k = 3,
          effsize.type = "biased",
          partial = FALSE,
          conf.level = 0.95,
          messages = FALSE
        )
      )

    # expected output
    results1 <-
      ggplot2::expr(
        paste(
          italic("F")["Welch"],
          "(",
          "3",
          ",",
          "16.586",
          ") = ",
          "1.405",
          ", ",
          italic("p"),
          " = ",
          "0.277",
          ", ",
          widehat(eta^2),
          " = ",
          "0.085",
          ", CI"["95%"],
          " [",
          "0.000",
          ", ",
          "0.203",
          "]",
          ", ",
          italic("n")["obs"],
          " = ",
          76L
        )
      )

    # testing overall call
    testthat::expect_identical(using_function1, results1)
  }
)

# parametric anova subtitles (partial omega) ----------------------------------

testthat::test_that(
  desc = "parametric anova subtitles with partial omega-squared",
  code = {
    testthat::skip_if(getRversion() < "3.6")

    # statsExpression output
    set.seed(123)
    using_function1 <-
      statsExpressions::expr_anova_parametric(
        data = ggplot2::msleep,
        x = vore,
        y = "sleep_rem",
        effsize.type = "partial_omega",
        k = 4,
        messages = FALSE
      )

    # expected output
    results1 <-
      ggplot2::expr(
        paste(
          italic("F")["Welch"],
          "(",
          "3",
          ",",
          "11.1010",
          ") = ",
          "2.6325",
          ", ",
          italic("p"),
          " = ",
          "0.1017",
          ", ",
          widehat(omega["p"]^2),
          " = ",
          "0.1438",
          ", CI"["95%"],
          " [",
          "0.0000",
          ", ",
          "0.2999",
          "]",
          ", ",
          italic("n")["obs"],
          " = ",
          56L
        )
      )

    # testing overall call
    testthat::expect_identical(using_function1, results1)
  }
)

# parametric anova subtitles (partial eta and NAs) --------------------------

testthat::test_that(
  desc = "parametric anova subtitles with partial eta-squared and data with NAs",
  code = {
    testthat::skip_if(getRversion() < "3.6")

    # statsExpression output
    set.seed(123)
    using_function1 <-
      statsExpressions::expr_anova_parametric(
        data = ggplot2::msleep,
        x = vore,
        y = sleep_rem,
        var.equal = TRUE,
        effsize.type = "partial_eta",
        k = 4,
        messages = FALSE
      )

    # expected output
    results1 <-
      ggplot2::expr(
        paste(
          italic("F")["Fisher"],
          "(",
          "3",
          ",",
          "52",
          ") = ",
          "4.1361",
          ", ",
          italic("p"),
          " = ",
          "0.0105",
          ", ",
          widehat(eta["p"]^2),
          " = ",
          "0.1926",
          ", CI"["95%"],
          " [",
          "0.0139",
          ", ",
          "0.3555",
          "]",
          ", ",
          italic("n")["obs"],
          " = ",
          56L
        )
      )

    # testing overall call
    testthat::expect_identical(using_function1, results1)
  }
)

# checking non-partial variants ----------------------------------

testthat::test_that(
  desc = "parametric anova subtitles with partial eta-squared and data with NAs",
  code = {
    testthat::skip_if(getRversion() < "3.6")

    # statsExpression output
    # eta
    set.seed(123)
    using_function1 <-
      statsExpressions::expr_anova_parametric(
        data = ggplot2::msleep,
        x = vore,
        y = sleep_rem,
        effsize.type = "biased",
        conf.level = 0.95,
        partial = FALSE,
        k = 4,
        messages = FALSE
      )

    # omega
    set.seed(123)
    using_function2 <-
      statsExpressions::expr_anova_parametric(
        data = ggplot2::msleep,
        x = vore,
        y = sleep_rem,
        effsize.type = "unbiased",
        partial = FALSE,
        k = 4,
        conf.level = 0.99,
        messages = FALSE
      )

    # expected output
    # eta
    results1 <-
      ggplot2::expr(
        paste(
          italic("F")["Welch"],
          "(",
          "3",
          ",",
          "11.1010",
          ") = ",
          "2.6325",
          ", ",
          italic("p"),
          " = ",
          "0.1017",
          ", ",
          widehat(eta^2),
          " = ",
          "0.1926",
          ", CI"["95%"],
          " [",
          "0.0139",
          ", ",
          "0.3555",
          "]",
          ", ",
          italic("n")["obs"],
          " = ",
          56L
        )
      )

    # omega
    results2 <-
      ggplot2::expr(
        paste(
          italic("F")["Welch"],
          "(",
          "3",
          ",",
          "11.1010",
          ") = ",
          "2.6325",
          ", ",
          italic("p"),
          " = ",
          "0.1017",
          ", ",
          widehat(omega^2),
          " = ",
          "0.1438",
          ", CI"["99%"],
          " [",
          "0.0000",
          ", ",
          "0.3577",
          "]",
          ", ",
          italic("n")["obs"],
          " = ",
          56L
        )
      )

    # testing overall call
    # eta
    testthat::expect_identical(using_function1, results1)

    # omega
    testthat::expect_identical(using_function2, results2)
  }
)

# parametric repeated anova subtitles (basic) ---------------------------------

testthat::test_that(
  desc = "parametric anova subtitles work (without NAs)",
  code = {
    testthat::skip_if(getRversion() < "3.6")

    # statsExpression output
    set.seed(123)
    using_function1 <-
      statsExpressions::expr_anova_parametric(
        data = statsExpressions::iris_long,
        x = "condition",
        y = value,
        paired = TRUE,
        k = 3,
        conf.level = 0.99,
        messages = TRUE
      )

    # expected output
    results1 <-
      ggplot2::expr(
        paste(
          italic("F")["Fisher"],
          "(",
          "1.149",
          ",",
          "171.217",
          ") = ",
          "776.318",
          ", ",
          italic("p"),
          " = ",
          "1.32e-69",
          ", ",
          widehat(omega["p"]^2),
          " = ",
          "0.707",
          ", CI"["99%"],
          " [",
          "0.653",
          ", ",
          "0.750",
          "]",
          ", ",
          italic("n")["pairs"],
          " = ",
          150L
        )
      )

    # testing overall call
    testthat::expect_identical(using_function1, results1)
  }
)

# parametric repeated anova subtitles ---------------------------------

testthat::test_that(
  desc = "parametric anova subtitles work (with NAs)",
  code = {
    testthat::skip_if(getRversion() < "3.6")

    # statsExpression output
    set.seed(123)
    using_function1 <-
      statsExpressions::expr_anova_parametric(
        data = WRS2::WineTasting,
        x = Wine,
        y = Taste,
        paired = TRUE,
        effsize.type = "biased",
        partial = TRUE,
        sphericity.correction = FALSE,
        k = 4,
        conf.level = 0.99,
        messages = FALSE
      )

    # expected output
    results1 <-
      ggplot2::expr(
        paste(
          italic("F")["Fisher"],
          "(",
          "2",
          ",",
          "42",
          ") = ",
          "6.2883",
          ", ",
          italic("p"),
          " = ",
          "0.0041",
          ", ",
          widehat(eta["p"]^2),
          " = ",
          "0.2304",
          ", CI"["99%"],
          " [",
          "0.0021",
          ", ",
          "0.4770",
          "]",
          ", ",
          italic("n")["pairs"],
          " = ",
          22L
        )
      )

    # testing overall call
    testthat::expect_identical(using_function1, results1)

    # statsExpression output
    set.seed(123)
    using_function2 <-
      statsExpressions::expr_anova_parametric(
        data = WRS2::WineTasting,
        x = Wine,
        y = Taste,
        paired = TRUE,
        effsize.type = "xxx",
        partial = FALSE,
        k = 5,
        conf.level = 0.90,
        messages = FALSE
      )

    # expected output
    results2 <-
      ggplot2::expr(
        paste(
          italic("F")["Fisher"],
          "(",
          "1.54700",
          ",",
          "32.48706",
          ") = ",
          "6.28831",
          ", ",
          italic("p"),
          " = ",
          "0.00844",
          ", ",
          widehat(omega^2),
          " = ",
          "0.01633",
          ", CI"["90%"],
          " [",
          "0.00000",
          ", ",
          "0.09119",
          "]",
          ", ",
          italic("n")["pairs"],
          " = ",
          22L
        )
      )

    # testing overall call
    testthat::expect_identical(using_function2, results2)
  }
)

# parametric repeated anova subtitles (catch bad data) -----------------------

testthat::test_that(
  desc = "parametric anova subtitles work (catch bad data)",
  code = {
    testthat::skip_if(getRversion() < "3.6")
    # statsExpression output

    # fake a data entry mistake
    iris_long[5, 3] <- "Sepal.Width"

    # sample size should be different
    set.seed(123)
    testthat::expect_error(statsExpressions::expr_anova_parametric(
      data = iris_long,
      x = condition,
      y = value,
      paired = TRUE,
      effsize.type = "eta",
      partial = FALSE,
      conf.level = 0.50,
      messages = FALSE
    ))
  }
)

# checking warning message when too few obs --------------------------------

testthat::test_that(
  desc = "checking warning message when too few obs",
  code = {
    testthat::skip_on_cran()
    testthat::skip_if(getRversion() < "3.6")
    set.seed(123)

    # dataframe
    df <- structure(list(
      x = c(
        30, 40, 50, 60, 70, 80, 90, 30, 40, 50,
        60, 70, 80, 90, 30, 40, 50, 60, 70, 80, 90, 30, 40, 50, 60, 70,
        80, 90, 30, 40, 50, 60, 70, 80, 90
      ),
      Participant = c(
        "FH2", "FH2",
        "FH2", "FH2", "FH2", "FH2", "FH2", "ZW", "ZW", "ZW", "ZW", "ZW",
        "ZW", "ZW", "KS", "KS", "KS", "KS", "KS", "KS", "KS", "CL", "CL",
        "CL", "CL", "CL", "CL", "CL", "AG", "AG", "AG", "AG", "AG", "AG",
        "AG"
      ),
      Method = c(
        2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
        2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2
      ),
      y = c(
        2571.25, 2688.003333, 2779.363333, 2832.046667,
        3050.72, 3255.553333, 3327.173667, 1766.296667, 2107.890333,
        2391.7, 2569.24, 2680.22, 2807.59, 2807.953333, 2078.734,
        2414.366667, 2583.27, 2923.253333, 3085.96, 3094.003333,
        3121.49, 2824.990667, 2716.429667, 2844.323333, 3124.713333,
        3252.863333, 3424.24, 3674.463333, 2401.996667, 2719.046667,
        2712.99, 2951.965667, 3046.526667, 3100.902667, 3195.331333
      )
    ),
    class = c("spec_tbl_df", "tbl_df", "tbl", "data.frame"),
    row.names = c(NA, -35L),
    spec = structure(list(
      cols = list(
        x = structure(list(), class = c("collector_double", "collector")),
        Participant = structure(list(), class = c(
          "collector_character",
          "collector"
        )),
        Method = structure(list(), class = c(
          "collector_double",
          "collector"
        )),
        y = structure(list(), class = c(
          "collector_double",
          "collector"
        ))
      ),
      default = structure(list(), class = c(
        "collector_guess",
        "collector"
      )), skip = 1
    ),
    class = "col_spec"
    )
    )

    # capture the message
    set.seed(123)
    p_sub <-
      suppressWarnings(statsExpressions::expr_anova_parametric(
        data = df,
        x = x,
        y = y,
        paired = TRUE,
        effsize.type = "eta",
        partial = FALSE,
        messages = FALSE
      ))

    # check that
    testthat::expect_identical(
      p_sub,
      ggplot2::expr(paste(
        italic("F")["Fisher"],
        "(",
        "6",
        ",",
        "24",
        ") = ",
        "43.14",
        ", ",
        italic("p"),
        " = ",
        "1.08e-11",
        ", ",
        widehat(eta^2),
        " = ",
        "0.62",
        ", CI"["95%"],
        " [",
        "0.26",
        ", ",
        "0.75",
        "]",
        ", ",
        italic("n")["pairs"],
        " = ",
        5L
      ))
    )
  }
)


# dataframe -----------------------------------------------------------

testthat::test_that(
  desc = "dataframe",
  code = {
    testthat::expect_is(
      statsExpressions::expr_anova_parametric(
        data = mtcars,
        x = cyl,
        y = wt,
        output = "dataframe"
      ),
      "tbl_df"
    )
  }
)
