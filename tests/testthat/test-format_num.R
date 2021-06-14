# format_num works --------------------------------------------------

test_that(
  desc = "format_num works",
  code = {

    # for reproducibility
    set.seed(123)

    # creating objects to test
    string1 <- format_num(.00001234)
    string2 <- format_num(.00001234, p.value = TRUE)
    string3 <- format_num(.00001234, p.value = TRUE, k = 8)
    string4 <- format_num(0.001, k = 4, p.value = TRUE)
    string5 <- format_num(0.0001, k = 4, p.value = TRUE)
    string6 <- format_num(0.0001, k = 3, p.value = TRUE)
    string7 <- format_num(0.0001, k = 3, p.value = FALSE)
    string8 <- format_num(.00333, k = 1, p.value = TRUE)

    # testing
    set.seed(123)
    expect_match(string1, regexp = "0.000")
    expect_match(string2, regexp = "1.23e-05")
    expect_match(string3, regexp = "1.234e-05")
    expect_error(format_num("123"))
    expect_match(string4, regexp = "0.0010")
    expect_match(string5, regexp = "1e-04")
    expect_match(string6, regexp = "1e-04")
    expect_match(string7, regexp = "0.000")
    expect_match(string8, regexp = "0.003")
  }
)
