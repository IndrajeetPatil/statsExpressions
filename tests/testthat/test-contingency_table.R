# contingency tab - data without NAs -----------------------------------------

test_that(
  desc = "contingency_table works - data without NAs",
  code = {

    # `statsExpressions` output
    set.seed(123)
    using_function1 <-
      suppressWarnings(contingency_table(
        data = mtcars,
        x = "am",
        y = cyl,
        k = 5,
        conf.level = 0.99
      ))

    # expected output
    set.seed(123)
    results1 <-
      ggplot2::expr(
        paste(
          chi["Pearson"]^2,
          "(",
          "2",
          ") = ",
          "8.74073",
          ", ",
          italic("p"),
          " = ",
          "0.01265",
          ", ",
          widehat(italic("V"))["Cramer"],
          " = ",
          "0.46431",
          ", CI"["99%"],
          " [",
          "0.00000",
          ", ",
          "0.88767",
          "]",
          ", ",
          italic("n")["obs"],
          " = ",
          "32"
        )
      )

    # testing overall call
    expect_identical(using_function1$expression[[1]], results1)

    # with counts
    set.seed(123)
    using_function2 <-
      contingency_table(
        data = as.data.frame(Titanic),
        x = names(as.data.frame(Titanic))[2],
        y = Survived,
        counts = "Freq"
      )

    results2 <-
      ggplot2::expr(
        paste(
          chi["Pearson"]^2,
          "(",
          "1",
          ") = ",
          "456.87",
          ", ",
          italic("p"),
          " = ",
          "2.3e-101",
          ", ",
          widehat(italic("V"))["Cramer"],
          " = ",
          "0.46",
          ", CI"["95%"],
          " [",
          "0.41",
          ", ",
          "0.50",
          "]",
          ", ",
          italic("n")["obs"],
          " = ",
          "2,201"
        )
      )

    # testing overall call
    expect_identical(using_function2$expression[[1]], results2)
  }
)

# contingency tab - data with NAs -----------------------------------------

test_that(
  desc = "contingency_table works - data with NAs",
  code = {


    # introduce NAs
    set.seed(123)
    using_function1 <-
      suppressWarnings(contingency_table(
        data = ggplot2::msleep,
        x = vore,
        y = "conservation",
        conf.level = 0.990
      ))

    # expected output
    set.seed(123)
    results1 <-
      ggplot2::expr(
        paste(
          chi["Pearson"]^2,
          "(",
          "15",
          ") = ",
          "15.75",
          ", ",
          italic("p"),
          " = ",
          "0.399",
          ", ",
          widehat(italic("V"))["Cramer"],
          " = ",
          "0.06",
          ", CI"["99%"],
          " [",
          "0.00",
          ", ",
          "0.00",
          "]",
          ", ",
          italic("n")["obs"],
          " = ",
          "52"
        )
      )

    # testing overall call
    expect_identical(using_function1$expression[[1]], results1)
  }
)

# paired data without NAs and counts data -------------------------------------

test_that(
  desc = "paired contingency_table works - counts data without NAs",
  code = {


    # create data structure
    paired_data <-
      structure(
        list(
          response_before =
            structure(
              c(1L, 2L, 1L, 2L),
              .Label = c("no", "yes"),
              class = "factor"
            ),
          response_after = structure(
            c(1L, 1L, 2L, 2L),
            .Label = c("no", "yes"),
            class = "factor"
          ),
          Freq = c(65L, 25L, 5L, 5L)
        ),
        class = "data.frame",
        row.names = c(NA, -4L)
      )

    # `statsExpressions` output
    set.seed(123)
    using_function1 <-
      suppressWarnings(
        contingency_table(
          data = paired_data,
          x = "response_before",
          y = response_after,
          paired = TRUE,
          counts = "Freq",
          k = 5
        )
      )

    # expected output
    set.seed(123)
    results1 <-
      ggplot2::expr(
        paste(
          chi["McNemar"]^2,
          "(",
          "1",
          ") = ",
          "13.33333",
          ", ",
          italic("p"),
          " = ",
          "2.6073e-04",
          ", ",
          widehat(italic("g"))["Cohen"],
          " = ",
          "0.33333",
          ", CI"["95%"],
          " [",
          "0.16436",
          ", ",
          "0.42663",
          "]",
          ", ",
          italic("n")["pairs"],
          " = ",
          "100"
        )
      )

    # testing overall call
    expect_identical(using_function1$expression[[1]], results1)
  }
)

# paired data with NAs  ---------------------------------------------

test_that(
  desc = "paired contingency_table works - with NAs",
  code = {

    # create data structure
    paired_data <-
      structure(
        list(
          response_before =
            structure(
              c(1L, 2L, 1L, 2L),
              .Label = c("no", "yes"),
              class = "factor"
            ),
          response_after = structure(
            c(1L, 1L, 2L, 2L),
            .Label = c("no", "yes"),
            class = "factor"
          ),
          Freq = c(65L, 25L, 5L, 5L)
        ),
        class = "data.frame",
        row.names = c(NA, -4L)
      )

    # expanding the dataframe
    paired_data %<>% tidyr::uncount(weights = Freq)

    # introduce NAs
    # check that 2-by-2 doesn't produce continuity correction
    set.seed(123)
    paired_data[1, 1] <- NA
    paired_data[12, 1] <- NA
    paired_data[22, 1] <- NA
    paired_data[24, 1] <- NA
    paired_data[65, 1] <- NA

    # `statsExpressions` output
    set.seed(123)
    using_function1 <-
      suppressWarnings(
        contingency_table(
          data = paired_data,
          x = response_before,
          y = "response_after",
          paired = TRUE,
          k = 3,
          conf.level = 0.90
        )
      )

    # expected output
    set.seed(123)
    results1 <-
      ggplot2::expr(
        paste(
          chi["McNemar"]^2,
          "(",
          "1",
          ") = ",
          "13.333",
          ", ",
          italic("p"),
          " = ",
          "2.61e-04",
          ", ",
          widehat(italic("g"))["Cohen"],
          " = ",
          "0.333",
          ", CI"["90%"],
          " [",
          "0.195",
          ", ",
          "0.416",
          "]",
          ", ",
          italic("n")["pairs"],
          " = ",
          "95"
        )
      )

    # testing overall call
    expect_identical(using_function1$expression[[1]], results1)
  }
)

# one-sample test (without counts) -----------------------------------------

test_that(
  desc = "Goodness of Fit contingency_table works without counts",
  code = {


    # `statsExpressions` output
    set.seed(123)
    using_function1 <-
      suppressWarnings(contingency_table(
        data = mtcars,
        x = "am",
        conf.level = 0.99,
        k = 5
      ))

    # expected output
    set.seed(123)
    results1 <-
      ggplot2::expr(
        paste(
          chi["gof"]^2,
          "(",
          "1",
          ") = ",
          "1.12500",
          ", ",
          italic("p"),
          " = ",
          "0.28884",
          ", ",
          widehat(italic("V"))["Cramer"],
          " = ",
          "0.05472",
          ", CI"["99%"],
          " [",
          "0.00000",
          ", ",
          "0.49920",
          "]",
          ", ",
          italic("n")["obs"],
          " = ",
          "32"
        )
      )

    # testing overall call
    expect_identical(using_function1$expression[[1]], results1)

    # with counts
    set.seed(123)
    using_function2 <-
      contingency_table(
        data = as.data.frame(Titanic),
        x = Sex,
        counts = "Freq"
      )

    results2 <-
      ggplot2::expr(
        paste(
          chi["gof"]^2,
          "(",
          "1",
          ") = ",
          "722.45",
          ", ",
          italic("p"),
          " = ",
          "3.92e-159",
          ", ",
          widehat(italic("V"))["Cramer"],
          " = ",
          "0.57",
          ", CI"["95%"],
          " [",
          "0.47",
          ", ",
          "0.62",
          "]",
          ", ",
          italic("n")["obs"],
          " = ",
          "2,201"
        )
      )

    # testing overall call
    expect_identical(using_function2$expression[[1]], results2)
  }
)

# checking subtitle (with counts) -----------------------------------------

test_that(
  desc = "Goodness of Fit contingency_table works with counts",
  code = {


    # `statsExpressions` output
    set.seed(123)
    using_function1 <-
      contingency_table(
        data = as.data.frame(Titanic),
        x = Sex,
        counts = "Freq",
        k = 3
      )

    # expected output
    set.seed(123)
    results1 <-
      ggplot2::expr(
        paste(
          chi["gof"]^2,
          "(",
          "1",
          ") = ",
          "722.454",
          ", ",
          italic("p"),
          " = ",
          "3.92e-159",
          ", ",
          widehat(italic("V"))["Cramer"],
          " = ",
          "0.573",
          ", CI"["95%"],
          " [",
          "0.465",
          ", ",
          "0.615",
          "]",
          ", ",
          italic("n")["obs"],
          " = ",
          "2,201"
        )
      )

    # testing overall call
    expect_identical(using_function1$expression[[1]], results1)
  }
)

# dataframe with NA  and with ratio ----------------------------------------

test_that(
  desc = "works with dataframes with NAs and with ratio",
  code = {


    # from function
    set.seed(123)
    using_function <-
      contingency_table(
        data = ggplot2::msleep,
        x = vore,
        ratio = c(0.2, 0.2, 0.3, 0.3)
      )

    # expected
    expected <-
      ggplot2::expr(
        paste(
          chi["gof"]^2,
          "(",
          "3",
          ") = ",
          "33.76",
          ", ",
          italic("p"),
          " = ",
          "2.23e-07",
          ", ",
          widehat(italic("V"))["Cramer"],
          " = ",
          "0.37",
          ", CI"["95%"],
          " [",
          "0.22",
          ", ",
          "0.49",
          "]",
          ", ",
          italic("n")["obs"],
          " = ",
          "76"
        )
      )

    # testing if these are equivalent
    expect_identical(using_function$expression[[1]], expected)
  }
)

# checking edge cases --------------------------------------------------------

test_that(
  desc = "works even in edge cases",
  code = {


    # too few observations
    df <- data.frame(
      x = c("a", "b", "b", "c", "c", "c"),
      y = c("a", "a", "a", "a", "b", "b")
    )

    # subtitle
    set.seed(123)
    sub <- suppressWarnings(
      contingency_table(df, x, y, messages = FALSE)
    )

    # test
    expect_identical(
      sub$expression[[1]],
      ggplot2::expr(
        paste(
          chi["Pearson"]^2,
          "(",
          "2",
          ") = ",
          "3.00",
          ", ",
          italic("p"),
          " = ",
          "0.223",
          ", ",
          widehat(italic("V"))["Cramer"],
          " = ",
          "0.35",
          ", CI"["95%"],
          " [",
          "0.00",
          ", ",
          "0.95",
          "]",
          ", ",
          italic("n")["obs"],
          " = ",
          "6"
        )
      )
    )

    # another dataset
    df1 <- dplyr::filter(mtcars, am == "0")

    set.seed(123)
    sub1 <- contingency_table(df1, am, cyl)

    # test
    expect_identical(
      sub1$expression[[1]],
      ggplot2::expr(
        paste(
          chi["gof"]^2,
          "(",
          "2",
          ") = ",
          "7.68",
          ", ",
          italic("p"),
          " = ",
          "0.021",
          ", ",
          widehat(italic("V"))["Cramer"],
          " = ",
          "0.41",
          ", CI"["95%"],
          " [",
          "0.00",
          ", ",
          "0.67",
          "]",
          ", ",
          italic("n")["obs"],
          " = ",
          "19"
        )
      )
    )
  }
)


# dataframe - parametric -----------------------------------------------------------

test_that(
  desc = "dataframe - parametric",
  code = {
    expect_s3_class(
      contingency_table(
        data = as.data.frame(HairEyeColor),
        x = Eye,
        counts = Freq,
      ),
      "tbl_df"
    )
  }
)


if (packageVersion("parameters") > "0.11.0") {

  # bayes factor (proportion test) --------------------------------------

  test_that(
    desc = "bayes factor (proportion test)",
    code = {

      # extracting results from where this function is implemented
      set.seed(123)
      df <-
        contingency_table(
          type = "bayes",
          data = mtcars,
          x = am
        )

      # check bayes factor values
      expect_equal(df$bf10, 0.2465787, tolerance = 0.001)

      # expr
      set.seed(123)
      expr_text <-
        contingency_table(
          type = "bayes",
          data = mtcars,
          x = "cyl",
          prior.concentration = 10,
          top.text = "duh"
        )

      expect_identical(
        expr_text$expression[[1]],
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


      # extracting results from where this function is implemented
      set.seed(123)
      df_results <-
        contingency_table(
          type = "bayes",
          data = mtcars,
          x = "am",
          y = cyl,
          sampling.plan = "jointMulti",
          fixed.margin = "rows"
        )

      # objects
      expect_identical(class(df_results), c("tbl_df", "tbl", "data.frame"))

      # check bayes factor values
      expect_equal(df_results$bf10[[1]], 28.07349, tolerance = 0.001)

      # expr
      set.seed(123)
      expr_text1 <-
        contingency_table(
          type = "bayes",
          data = mtcars,
          x = colnames(mtcars)[9],
          y = "cyl",
          sampling.plan = "jointMulti",
          fixed.margin = "rows",
          conf.level = 0.89,
          k = 3L
        )

      # with counts
      set.seed(123)
      expr_text2 <-
        contingency_table(
          type = "bayes",
          data = as.data.frame(Titanic),
          x = "Survived",
          y = colnames(as.data.frame(Titanic))[2],
          counts = "Freq",
          sampling.plan = "jointMulti",
          fixed.margin = "rows",
          conf.level = 0.99,
          k = 3L
        )

      # with counts
      set.seed(123)
      expr_text3 <-
        contingency_table(
          type = "bayes",
          data = as.data.frame(Titanic),
          x = Survived,
          y = Sex,
          counts = "Freq",
          k = 3L,
          prior.concentration = 1.5
        )

      # expr text
      expect_identical(
        expr_text1$expression[[1]],
        ggplot2::expr(
          paste(
            "log"["e"] * "(BF"["01"] * ") = " * "-3.335" * ", ",
            widehat(italic("V"))["Cramer"]^"posterior" * " = " * "0.479" * ", ",
            "CI"["89%"]^"HDI" * " [" * "0.285" * ", " * "0.692" * "], ",
            italic("a")["Gunel-Dickey"] * " = " * "1.000"
          )
        )
      )

      expect_type(expr_text2$expression[[1]], "language")
      expect_type(expr_text3$expression[[1]], "language")

      expect_identical(
        expr_text2$expression[[1]],
        ggplot2::expr(
          paste(
            "log"["e"] * "(BF"["01"] * ") = " * "-214.255" * ", ",
            widehat(italic("V"))["Cramer"]^"posterior" * " = " * "0.455" * ", ",
            "CI"["99%"]^"HDI" * " [" * "0.402" * ", " * "0.508" * "], ",
            italic("a")["Gunel-Dickey"] * " = " * "1.000"
          )
        )
      )

      expect_identical(
        expr_text3$expression[[1]],
        ggplot2::expr(
          paste(
            "log"["e"] * "(BF"["01"] * ") = " * "-213.873" * ", ",
            widehat(italic("V"))["Cramer"]^"posterior" * " = " * "0.454" * ", ",
            "CI"["95%"]^"HDI" * " [" * "0.417" * ", " * "0.495" * "], ",
            italic("a")["Gunel-Dickey"] * " = " * "1.500"
          )
        )
      )
    }
  )

  # check edge cases - bayes --------------------------------------------

  test_that(
    desc = "check edge cases - bayes",
    code = {
      df <- data.frame(x = c("a"))

      expect_null(contingency_table(type = "bayes", df, x))
    }
  )
}
