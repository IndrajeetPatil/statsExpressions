# format_num works --------------------------------------------------

test_that(
  desc = "format_num works",
  code = {
    # for reproducibility
    set.seed(123)

    # creating objects to test
    string1 <- format_num(.00001234)
    string2 <- format_num(.00001234, TRUE)
    string3 <- format_num(.00001234, k = 8, TRUE)
    string4 <- format_num(0.001, k = 4, TRUE)
    string5 <- format_num(0.0001, k = 4, TRUE)
    string6 <- format_num(0.0001, k = 3, TRUE)
    string7 <- format_num(0.0001, k = 3, FALSE)
    string8 <- format_num(.00333, k = 1, TRUE)

    # testing
    set.seed(123)
    expect_error(format_num("123"))

    # small numbers
    expect_snapshot(
      list(
        format_num(.00001234),
        format_num(.00001234, TRUE),
        format_num(.00001234, k = 8, TRUE),
        format_num(0.001, k = 4, TRUE),
        format_num(0.0001, k = 4, TRUE),
        format_num(0.0001, k = 3, TRUE),
        format_num(0.0001, k = 3, FALSE),
        format_num(.00333, k = 1, TRUE)
      )
    )

    # large numbers
    expect_snapshot(
      list(
        format_num(123),
        format_num(1234),
        format_num(1234, k = 3),
        format_num(123445678, k = 4),
        format_num(123445678, k = 4),
        format_num(123445678, k = 3),
        format_num(123445678, k = 3),
        format_num(123445678, k = 1)
      )
    )

    expect_identical(format_num(NA), "NA")
    expect_identical(format_num(NA_complex_), "NA")
  }
)
