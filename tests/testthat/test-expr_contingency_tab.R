# contingency tab - data without NAs -----------------------------------------

test_that(
  desc = "expr_contingency_tab works - data without NAs",
  code = {
    skip_if(getRversion() < "3.6")

    # `statsExpressions` output
    set.seed(123)
    using_function1 <-
      suppressWarnings(statsExpressions::expr_contingency_tab(
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
    expect_identical(using_function1, results1)

    # with counts
    set.seed(123)
    using_function2 <-
      statsExpressions::expr_contingency_tab(
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
    expect_identical(using_function2, results2)
  }
)

# contingency tab - data with NAs -----------------------------------------

test_that(
  desc = "expr_contingency_tab works - data with NAs",
  code = {
    skip_if(getRversion() < "3.6")

    # introduce NAs
    set.seed(123)
    using_function1 <-
      suppressWarnings(statsExpressions::expr_contingency_tab(
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
    expect_identical(using_function1, results1)
  }
)

# paired data without NAs and counts data -------------------------------------

test_that(
  desc = "paired expr_contingency_tab works - counts data without NAs",
  code = {
    skip_if(getRversion() < "3.6")

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
        statsExpressions::expr_contingency_tab(
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
    expect_identical(using_function1, results1)
  }
)

# paired data with NAs  ---------------------------------------------

test_that(
  desc = "paired expr_contingency_tab works - with NAs",
  code = {
    skip_if(getRversion() < "3.6")

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
    paired_data %<>% tidyr::uncount(data = ., weights = Freq)

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
        statsExpressions::expr_contingency_tab(
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
    expect_identical(using_function1, results1)
  }
)

# paired data 4-by-4  ---------------------------------------------

test_that(
  desc = "paired data 4-by-4",
  code = {
    skip_if(getRversion() < "3.6")
    set.seed(123)

    # making data
    Input <- ("
    Before        Pastafarian2   Discordiant2   Dudist2   Jedi2
    Pastafarian   7              0              23         0
    Discordiant   0              7               0        33
    Dudist        3              0               7         1
    Jedi          0              1               0         7
    ")

    # matrix
    matrix_df <- as.matrix(read.table(textConnection(Input),
      header = TRUE,
      row.names = 1
    ))

    # cleaning the factor levels
    df <- as.data.frame(as.table(matrix_df)) %>%
      dplyr::mutate(.data = ., Var2 = dplyr::case_when(
        Var2 == "Pastafarian2" ~ "Pastafarian",
        Var2 == "Discordiant2" ~ "Discordiant",
        Var2 == "Dudist2" ~ "Dudist",
        Var2 == "Jedi2" ~ "Jedi"
      ))

    # `statsExpressions` output
    set.seed(123)
    subtitle1 <-
      suppressWarnings(statsExpressions::expr_contingency_tab(
        data = df,
        x = Var1,
        y = Var2,
        counts = "Freq",
        paired = TRUE,
        k = 4,
        conf.level = 0.99,
        conf.type = "basic",
        nboot = 50
      ))

    # expected output
    set.seed(123)
    results1 <-
      ggplot2::expr(
        paste(
          chi["McNemar"]^2,
          "(",
          "6",
          ") = ",
          "NaN",
          ", ",
          italic("p"),
          " = ",
          "NaN",
          ", ",
          widehat(italic("g"))["Cohen"],
          " = ",
          "0.2955",
          ", CI"["99%"],
          " [",
          "0.1659",
          ", ",
          "0.3835",
          "]",
          ", ",
          italic("n")["pairs"],
          " = ",
          "89"
        )
      )

    # testing overall call
    expect_identical(subtitle1, results1)

    # edge case
    dfEx <-
      data.frame(
        cat1 = rep(c("A", "B"), 10),
        cat2 = c(rep("C", 10), rep("D", 10))
      )

    # subtitle
    set.seed(123)
    subtitle2 <-
      statsExpressions::expr_contingency_tab(
        data = dfEx,
        x = cat1,
        y = cat2,
        paired = TRUE
      )

    results2 <-
      ggplot2::expr(
        paste(
          chi["McNemar"]^2,
          "(",
          "1",
          ") = ",
          "0.00",
          ", ",
          italic("p"),
          " = ",
          "1.000",
          ", ",
          widehat(italic("g"))["Cohen"],
          " = ",
          "0.00",
          ", CI"["95%"],
          " [",
          "-0.26",
          ", ",
          "0.26",
          "]",
          ", ",
          italic("n")["pairs"],
          " = ",
          "20"
        )
      )

    # testing overall call
    expect_identical(subtitle2, results2)
  }
)

# one-sample test (without counts) -----------------------------------------

test_that(
  desc = "Goodness of Fit expr_contingency_tab works without counts",
  code = {
    skip_if(getRversion() < "3.6")

    # `statsExpressions` output
    set.seed(123)
    using_function1 <-
      suppressWarnings(statsExpressions::expr_contingency_tab(
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
    expect_identical(using_function1, results1)

    # with counts
    set.seed(123)
    using_function2 <-
      statsExpressions::expr_contingency_tab(
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
    expect_identical(using_function2, results2)
  }
)

# checking subtitle (with counts) -----------------------------------------

test_that(
  desc = "Goodness of Fit expr_contingency_tab works with counts",
  code = {
    skip_if(getRversion() < "3.6")

    # `statsExpressions` output
    set.seed(123)
    using_function1 <-
      statsExpressions::expr_contingency_tab(
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
    expect_identical(using_function1, results1)
  }
)

# dataframe with NA  and with ratio ----------------------------------------

test_that(
  desc = "works with dataframes with NAs and with ratio",
  code = {
    skip_if(getRversion() < "3.6")

    # from function
    set.seed(123)
    using_function <-
      statsExpressions::expr_contingency_tab(
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
          "0.27",
          ", CI"["95%"],
          " [",
          "0.11",
          ", ",
          "0.38",
          "]",
          ", ",
          italic("n")["obs"],
          " = ",
          "76"
        )
      )

    # testing if these are equivalent
    expect_identical(using_function, expected)
  }
)

# checking edge cases --------------------------------------------------------

test_that(
  desc = "works even in edge cases",
  code = {
    skip_if(getRversion() < "3.6")

    # too few observations
    df <- data.frame(
      x = c("a", "b", "b", "c", "c", "c"),
      y = c("a", "a", "a", "a", "b", "b")
    )

    # subtitle
    set.seed(123)
    sub <- suppressWarnings(
      statsExpressions::expr_contingency_tab(df, x, y, messages = FALSE)
    )

    # test
    expect_identical(
      sub,
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
    sub1 <- expr_contingency_tab(df1, am, cyl)

    # test
    expect_identical(
      sub1,
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


# dataframe -----------------------------------------------------------

test_that(
  desc = "dataframe",
  code = {
    expect_s3_class(
      statsExpressions::expr_contingency_tab(
        data = as.data.frame(HairEyeColor),
        x = Eye,
        counts = Freq,
        output = "dataframe"
      ),
      "tbl_df"
    )
  }
)
