
# loops - correlation ---------------------------------------------------------

test_that(
  desc = "corr_test works in loop",
  code = {
    col.name <- colnames(mtcars)

    ls <-
      capture.output(for (i in 4:5) {
        print(corr_test(
          data = mtcars,
          x = disp,
          y = col.name[i]
        ))
      })

    expect_true(length(ls) >= 8L)
  }
)

# loops - one-sample ---------------------------------------------------------

test_that(
  desc = "expr_onesample works in loop",
  code = {
    col.name <- colnames(mtcars)

    ls <-
      capture.output(for (i in 3:5) {
        print(t_onesample(
          data = mtcars,
          x = col.name[i],
          test.value = 3
        ))
      })

    expect_true(length(ls) >= 12L)
  }
)


# loops - contingency tabs ---------------------------------------------------

test_that(
  desc = "contingency_tab works in loop",
  code = {
    df <- dplyr::select(mtcars, am, cyl, vs)
    col.name <- colnames(df)

    set.seed(123)
    ls <-
      capture.output(for (i in 1:3) {
        print(contingency_tab(
          data = mtcars,
          x = col.name[i]
        ))
      })

    expect_true(length(ls) >= 12L)
  }
)
