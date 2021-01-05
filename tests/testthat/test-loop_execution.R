
# loops - correlation ---------------------------------------------------------

test_that(
  desc = "expr_corr_test works in loop",
  code = {
    skip_if(getRversion() < "3.6")

    col.name <- colnames(mtcars)

    ls <-
      capture.output(for (i in 4:5) {
        print(expr_corr_test(
          data = mtcars,
          x = disp,
          y = col.name[i]
        ))
      })

    expect_equal(length(ls), 8L)
  }
)

# loops - one-sample ---------------------------------------------------------

test_that(
  desc = "expr_onesample works in loop",
  code = {
    skip_if(getRversion() < "3.6")

    col.name <- colnames(mtcars)

    ls <-
      capture.output(for (i in 3:5) {
        print(expr_t_onesample(
          data = mtcars,
          x = col.name[i],
          test.value = 3
        ))
      })

    expect_equal(length(ls), 12L)
  }
)


# loops - contingency tabs ---------------------------------------------------

test_that(
  desc = "expr_contingency_tab works in loop",
  code = {
    skip_if(getRversion() < "3.6")

    df <- dplyr::select(mtcars, am, cyl, vs)
    col.name <- colnames(df)

    set.seed(123)
    ls <-
      capture.output(for (i in 1:3) {
        print(expr_contingency_tab(
          data = mtcars,
          x = col.name[i]
        ))
      })

    expect_equal(length(ls), 12L)
  }
)
