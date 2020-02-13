# corr obj - pearson's r -----------------------------------------------------

testthat::test_that(
  desc = "corr_objects works - pearson",
  code = {
    testthat::skip_if(getRversion() < "3.6")

    # create object
    set.seed(123)
    df <- purrr::keep(ggplot2::msleep, purrr::is_bare_numeric)
    ls <- statsExpressions::corr_objects(df)

    # test random elements
    testthat::expect_equal(length(ls), 3L)
    testthat::expect_is(ls[[1]], "matrix")
    testthat::expect_is(ls[[2]], "matrix")
    testthat::expect_is(ls[[3]], "psych")
    testthat::expect_equal(dim(ls[[1]]), c(6, 6))
    testthat::expect_equal(dim(ls[[2]]), c(6, 6))
    testthat::expect_equal(ls[[3]]$r, ls[[1]], tolerance = 0.01)
    testthat::expect_equal(ls[[3]]$p, ls[[2]], tolerance = 0.01)
  }
)

# corr obj - robust -----------------------------------------------------

testthat::test_that(
  desc = "corr_objects works - robust",
  code = {
    testthat::skip_if(getRversion() < "3.6")

    # create object
    set.seed(123)
    df <- purrr::keep(ggplot2::msleep, purrr::is_bare_numeric)
    ls <- statsExpressions::corr_objects(df, corr.method = "robust")

    # test random elements
    testthat::expect_equal(length(ls), 3L)
    testthat::expect_is(ls[[1]], "matrix")
    testthat::expect_is(ls[[2]], "matrix")
    testthat::expect_is(ls[[3]], "psych")
    testthat::expect_equal(dim(ls[[1]]), c(6, 6))
    testthat::expect_equal(dim(ls[[2]]), c(6, 6))
  }
)
