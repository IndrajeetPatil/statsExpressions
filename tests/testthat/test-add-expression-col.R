test_that(
  desc = ".prettyNum() does not warn under `options(OutDec = \",\")` (#146)",
  code = {
    withr::local_options(list(OutDec = ","))

    expect_no_warning(statsExpressions:::.prettyNum(1234L))
    expect_identical(statsExpressions:::.prettyNum(1234L), "1,234")
  }
)

test_that(
  desc = ".to_char() always uses `.` as decimal mark (#146)",
  code = {
    withr::local_options(list(OutDec = ","))

    expect_identical(statsExpressions:::.to_char(5.494, digits = 2L), "5.49")
    expect_identical(statsExpressions:::.to_char(NA_real_), "NA")
  }
)

test_that(
  desc = "add_expression_col() works under `options(OutDec = \",\")` (#146)",
  code = {
    withr::local_options(list(OutDec = ","))

    stats_df <- cbind.data.frame(
      statistic  = 5.494,
      df         = 29.234,
      p.value    = 0.00001,
      estimate   = -1.980,
      conf.level = 0.95,
      conf.low   = -2.873,
      conf.high  = -1.088,
      method     = "Student's t-test"
    )

    set.seed(123)
    expect_no_warning(
      df <- add_expression_col(
        data           = stats_df,
        statistic.text = list(quote(italic("t"))),
        effsize.text   = list(quote(italic("d"))),
        n              = 32L,
        digits         = 3L,
        digits.df      = 3L
      )
    )

    expect_true(is.language(df[["expression"]][[1L]]))
  }
)
