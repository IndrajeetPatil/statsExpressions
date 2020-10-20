
# loops - correlation ---------------------------------------------------------

testthat::test_that(
  desc = "expr_corr_test works in loop",
  code = {
    testthat::skip_if(getRversion() < "3.6")

    col.name <- colnames(mtcars)

    ls <-
      capture.output(for (i in 4:5) {
        print(expr_corr_test(
          data = mtcars,
          x = disp,
          y = col.name[i]
        ))
      })

    testthat::expect_equal(length(ls), 8L)
  }
)

# loops - one-sample ---------------------------------------------------------

testthat::test_that(
  desc = "expr_corr_test works in loop",
  code = {
    testthat::skip_if(getRversion() < "3.6")

    col.name <- colnames(mtcars)

    ls <-
      capture.output(for (i in 3:5) {
        print(expr_t_onesample(
          data = mtcars,
          x = col.name[i],
          test.value = 3
        ))
      })

    testthat::expect_equal(length(ls), 12L)
  }
)
