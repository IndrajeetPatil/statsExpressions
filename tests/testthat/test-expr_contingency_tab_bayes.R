# bayes factor (proportion test) --------------------------------------

test_that(
  desc = "bayes factor (proportion test)",
  code = {
    skip_if(getRversion() < "3.6")
    skip_on_cran()

    # extracting results from where this function is implemented
    set.seed(123)
    df <-
      expr_contingency_tab(
        type = "bayes",
        data = mtcars,
        x = am,
        output = "dataframe"
      )

    # check bayes factor values
    expect_equal(df$bf10, 0.2465787, tolerance = 0.001)
    expect_equal(df$log_e_bf10, -1.400074, tolerance = 0.001)

    # expr
    set.seed(123)
    expr_text <-
      expr_contingency_tab(
        type = "bayes",
        data = mtcars,
        x = "cyl",
        output = "expression",
        prior.concentration = 10,
        top.text = "duh"
      )

    expect_identical(
      expr_text,
      ggplot2::expr(
        atop(displaystyle("duh"),
          expr =
            paste(
              "log"["e"] * "(BF"["01"] * ") = " * "0.55" * ", ",
              italic("a")["Gunel-Dickey"] * " = " * "10.00"
            )
        )
      )
    )
  }
)

# bayes factor (contingency tab) --------------------------------------

test_that(
  desc = "bayes factor (contingency tab)",
  code = {
    skip_if(getRversion() < "3.6")
    skip_on_cran()

    # extracting results from where this function is implemented
    set.seed(123)
    df_results <-
      expr_contingency_tab(
        type = "bayes",
        data = mtcars,
        x = "am",
        y = cyl,
        sampling.plan = "jointMulti",
        fixed.margin = "rows",
        output = "dataframe"
      )

    # objects
    expect_identical(class(df_results), c("tbl_df", "tbl", "data.frame"))

    # check bayes factor values
    expect_equal(df_results$bf10[[1]], 28.07349, tolerance = 0.001)
    expect_equal(df_results$log_e_bf10[[1]], 3.334826, tolerance = 0.001)

    # expr
    set.seed(123)
    expr_text1 <-
      expr_contingency_tab(
        type = "bayes",
        data = mtcars,
        x = colnames(mtcars)[9],
        y = "cyl",
        sampling.plan = "jointMulti",
        fixed.margin = "rows",
        conf.level = 0.89,
        k = 3L,
        output = "expression"
      )

    # with counts
    set.seed(123)
    expr_text2 <-
      expr_contingency_tab(
        type = "bayes",
        data = as.data.frame(Titanic),
        x = "Survived",
        y = colnames(as.data.frame(Titanic))[2],
        counts = "Freq",
        sampling.plan = "jointMulti",
        fixed.margin = "rows",
        k = 3L,
        output = "expression",
        conf.level = 0.99,
        centrality = "mean"
      )

    # with counts
    set.seed(123)
    expr_text3 <-
      expr_contingency_tab(
        type = "bayes",
        data = as.data.frame(Titanic),
        x = Survived,
        y = Sex,
        counts = "Freq",
        k = 3L,
        output = "expression",
        prior.concentration = 1.5
      )

    # expr text
    expect_identical(
      expr_text1,
      ggplot2::expr(
        paste(
          "log"["e"] * "(BF"["01"] * ") = " * "-3.335" * ", ",
          widehat(italic("V"))["median"]^"posterior" * " = " * "0.479" * ", ",
          "CI"["89%"]^"HDI" * " [" * "0.285" * ", " * "0.692" * "], ",
          italic("a")["Gunel-Dickey"] * " = " * "1.000"
        )
      )
    )

    expect_type(expr_text2, "language")
    expect_type(expr_text3, "language")

    expect_identical(
      expr_text2,
      ggplot2::expr(
        paste(
          "log"["e"] * "(BF"["01"] * ") = " * "-214.255" * ", ",
          widehat(italic("V"))["median"]^"posterior" * " = " * "0.455" * ", ",
          "CI"["99%"]^"HDI" * " [" * "0.402" * ", " * "0.508" * "], ",
          italic("a")["Gunel-Dickey"] * " = " * "1.000"
        )
      )
    )

    expect_identical(
      expr_text3,
      ggplot2::expr(
        paste(
          "log"["e"] * "(BF"["01"] * ") = " * "-213.873" * ", ",
          widehat(italic("V"))["median"]^"posterior" * " = " * "0.454" * ", ",
          "CI"["95%"]^"HDI" * " [" * "0.417" * ", " * "0.495" * "], ",
          italic("a")["Gunel-Dickey"] * " = " * "1.500"
        )
      )
    )
  }
)

# check edge cases --------------------------------------------

test_that(
  desc = "check edge cases",
  code = {
    df <- data.frame(x = c("a"))

    expect_null(expr_contingency_tab(type = "bayes", df, x))
  }
)
