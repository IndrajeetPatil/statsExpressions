test_that(
  desc = "corr_test works in loop",
  code = {
    col.name <- colnames(mtcars)

    set.seed(123)
    ls <- capture.output(for (i in 4:5) {
      print(corr_test(
        data = mtcars,
        x = disp,
        y = col.name[i],
        type = "bayes"
      ))
    })

    expect_true(length(ls) >= 6L)
  }
)

test_that(
  desc = "contingency_table works in loop",
  code = {
    df <- dplyr::select(mtcars, am, cyl, vs)
    col.name <- colnames(df)

    set.seed(123)
    ls <- capture.output(for (i in 1:3) {
      print(contingency_table(
        data = mtcars,
        x = col.name[i],
        type = "bayes"
      ))
    })

    expect_true(length(ls) >= 10L)
  }
)

test_that(
  desc = "expr_ttest works in loop",
  code = {
    # working with loops
    df <- dplyr::select(mtcars, am, wt, mpg)
    col.name <- colnames(df)

    set.seed(123)
    ls1 <-
      capture.output(for (i in 2:length(col.name)) {
        print(two_sample_test(
          data = mtcars,
          x = am,
          y = !!col.name[i],
          type = "bayes"
        ))
      })

    expect_true(length(ls1) >= 8L)

    set.seed(123)
    ls2 <- capture.output(for (i in 2:length(col.name)) {
      print(one_sample_test(
        data = mtcars,
        x = col.name[i],
        test.value = 3,
        type = "bayes"
      ))
    })

    expect_true(length(ls2) >= 8L)
  }
)

test_that(
  desc = "expr_anova works in loop",
  code = {
    # working with loops
    df <- dplyr::select(mtcars, cyl, wt, mpg)
    col.name <- colnames(df)

    set.seed(123)
    ls <- capture.output(for (i in 2:length(col.name)) {
      print(oneway_anova(
        data = mtcars,
        x = cyl,
        y = !!col.name[i],
        type = "bayes"
      ))
    })

    expect_true(length(ls) >= 22L)
  }
)
