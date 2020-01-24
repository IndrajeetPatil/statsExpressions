context("Specify decimals")

# specify_decimal_p works --------------------------------------------------

test_that(
  desc = "specify_decimal_p works",
  code = {

    # for reproducibility
    set.seed(123)

    # creating objects to test
    string1 <- specify_decimal_p(x = .00001234)
    string2 <- specify_decimal_p(x = .00001234, p.value = TRUE)
    string3 <- specify_decimal_p(x = .00001234, p.value = TRUE, k = 8)
    string4 <- specify_decimal_p(x = 0.001, k = 4, p.value = TRUE)
    string5 <- specify_decimal_p(x = 0.0001, k = 4, p.value = TRUE)
    string6 <- specify_decimal_p(x = 0.0001, k = 3, p.value = TRUE)
    string7 <- specify_decimal_p(x = 0.0001, k = 3, p.value = FALSE)
    string8 <- specify_decimal_p(x = .00333, k = 1, p.value = TRUE)

    # testing
    set.seed(123)

    # tests
    testthat::expect_match(string1, regexp = "0.000")
    testthat::expect_match(string2, regexp = "< 0.001")
    testthat::expect_match(string3, regexp = "1.234e-05")
    testthat::expect_error(specify_decimal_p("123"))
    testthat::expect_match(string4, regexp = "0.0010")
    testthat::expect_match(string5, regexp = "1e-04")
    testthat::expect_match(string6, regexp = "< 0.001")
    testthat::expect_match(string7, regexp = "0.000")
    testthat::expect_match(string8, regexp = "0.003")
  }
)
