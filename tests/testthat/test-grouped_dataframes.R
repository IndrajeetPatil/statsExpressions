test_that(
  desc = "results don't differ for grouped versus ungrouped",
  code = {
    # create a grouped dataframe and forget to ungroup it
    df <- group_by(mtcars, am)

    # correlation ---------------------------------------------------

    expect_equal(
      corr_test(mtcars, wt, mpg),
      corr_test(df, wt, mpg),
      ignore_attr = TRUE
    )

    # t-test -------------------------------------------------------

    expect_equal(
      two_sample_test(mtcars, vs, mpg),
      two_sample_test(df, vs, mpg),
      ignore_attr = TRUE
    )

    expect_equal(
      one_sample_test(mtcars, wt, test.value = 3),
      one_sample_test(df, wt, test.value = 3),
      ignore_attr = TRUE
    )

    # anova ---------------------------------------------------------

    expect_equal(
      oneway_anova(mtcars, cyl, wt),
      oneway_anova(df, cyl, wt),
      ignore_attr = TRUE
    )

    # contingency tabs -----------------------------------------------

    expect_equal(
      contingency_table(mtcars, cyl),
      contingency_table(df, cyl),
      ignore_attr = TRUE
    )
  }
)
