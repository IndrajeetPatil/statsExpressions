
# loops - correlation ---------------------------------------------------------

test_that(
  desc = "expr_corr_test works in loop",
  code = {
    col.name <- colnames(mtcars)

    set.seed(123)
    ls <-
      capture.output(for (i in 4:5) {
        print(expr_corr_test(
          data = mtcars,
          x = disp,
          y = col.name[i],
          type = "bayes", output = "dataframe"
        ))
      })

    expect_true(length(ls) >= 6L)
  }
)

# loops - contingency tabs ---------------------------------------------------

test_that(
  desc = "expr_contingency_tab works in loop",
  code = {
    df <- dplyr::select(mtcars, am, cyl, vs)
    col.name <- colnames(df)

    set.seed(123)
    ls <-
      capture.output(for (i in 1:3) {
        print(expr_contingency_tab(
          data = mtcars,
          x = col.name[i],
          type = "bayes", output = "dataframe"
        ))
      })

    expect_true(length(ls) >= 10L)
  }
)

# loops - expr_ttest ---------------------------------------------------

test_that(
  desc = "expr_ttest works in loop",
  code = {

    # working with loops
    df <- dplyr::select(mtcars, am, wt, mpg)
    col.name <- colnames(df)

    set.seed(123)
    ls1 <-
      capture.output(for (i in 2:length(col.name)) {
        print(expr_t_twosample(
          data = mtcars,
          x = am,
          y = !!col.name[i],
          type = "bayes", output = "dataframe"
        ))
      })

    expect_true(length(ls1) >= 8L)

    set.seed(123)
    ls2 <-
      capture.output(for (i in 2:length(col.name)) {
        print(expr_t_onesample(
          data = mtcars,
          x = col.name[i],
          test.value = 3,
          type = "bayes", output = "dataframe"
        ))
      })

    expect_true(length(ls2) >= 8L)
  }
)

# loops - expr_anova ---------------------------------------------------

test_that(
  desc = "expr_anova works in loop",
  code = {
    # skip_on_cran()

    # working with loops
    df <- dplyr::select(mtcars, cyl, wt, mpg)
    col.name <- colnames(df)

    set.seed(123)
    ls <-
      capture.output(for (i in 2:length(col.name)) {
        print(expr_oneway_anova(
          data = mtcars,
          x = cyl,
          y = !!col.name[i],
          type = "bayes", output = "dataframe"
        ))
      })

    expect_true(length(ls) >= 22L)
  }
)
