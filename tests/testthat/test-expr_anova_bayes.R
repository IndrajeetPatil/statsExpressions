# expr_anova_bayes works (between-subjects) ----------------------------

testthat::test_that(
  desc = "expr_anova_bayes works (between-subjects)",
  code = {
    testthat::skip_if(getRversion() < "3.6")

    # ggstatsplot output
    set.seed(123)
    using_function1 <-
      statsExpressions::expr_anova_bayes(
        data = statsExpressions::movies_long,
        x = genre,
        y = rating,
        k = 5,
        bf.prior = 0.8
      )

    # expected output
    results1 <-
      tidyBF::bf_oneway_anova(
        data = statsExpressions::movies_long,
        x = genre,
        y = rating,
        k = 5,
        bf.prior = 0.8,
        output = "h1"
      )

    # testing overall call
    testthat::expect_identical(using_function1, results1)
  }
)

# expr_anova_bayes works (within-subjects) ----------------------------

testthat::test_that(
  desc = "expr_anova_bayes works (within-subjects)",
  code = {
    testthat::skip_if(getRversion() < "3.6")

    # ggstatsplot output
    set.seed(123)
    using_function1 <-
      statsExpressions::expr_anova_bayes(
        data = WRS2::WineTasting,
        x = Wine,
        y = Taste,
        paired = TRUE,
        k = 3
      )

    # expected output
    set.seed(123)
    results1 <-
      tidyBF::bf_oneway_anova(
        data = WRS2::WineTasting,
        x = Wine,
        y = Taste,
        paired = TRUE,
        k = 3,
        output = "h1"
      )

    # testing overall call
    testthat::expect_identical(using_function1, results1)
  }
)

# expr_anova_bayes works (within-subjects) - with NA ----------------------

testthat::test_that(
  desc = "expr_anova_bayes works (within-subjects) - with NA",
  code = {
    testthat::skip_if(getRversion() < "3.6")

    # ggstatsplot output
    set.seed(123)
    using_function1 <-
      statsExpressions::expr_anova_bayes(
        data = statsExpressions::bugs_long,
        x = condition,
        y = desire,
        paired = TRUE,
        k = 3
      )

    # expected output
    set.seed(123)
    results1 <-
      tidyBF::bf_oneway_anova(
        data = statsExpressions::bugs_long,
        x = condition,
        y = desire,
        paired = TRUE,
        k = 3,
        output = "h1"
      )

    # testing overall call
    testthat::expect_identical(using_function1, results1)
  }
)


# dataframe -----------------------------------------------------------

testthat::test_that(
  desc = "dataframe",
  code = {
    testthat::expect_is(
      statsExpressions::expr_anova_bayes(
        data = mtcars,
        x = cyl,
        y = wt,
        output = "dataframe"
      ),
      "tbl_df"
    )
  }
)

# works with subject id ------------------------------------------------------

testthat::test_that(
  desc = "works with subject id",
  code = {
    testthat::skip_if(getRversion() < "3.6")

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
      statsExpressions::expr_anova_bayes(
        data = df,
        x = condition,
        y = score,
        subject.id = id,
        paired = TRUE
      )

    # correct
    set.seed(123)
    expr2 <-
      statsExpressions::expr_anova_bayes(
        data = dplyr::arrange(df, id),
        x = condition,
        y = score,
        paired = TRUE
      )

    testthat::expect_equal(expr1, expr2)
  }
)
