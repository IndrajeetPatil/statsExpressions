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
