# Tests that verify package behaviour when the user sets a non-default
# decimal mark via `options(OutDec = ",")` (see issue #146).
#
# plotmath expressions used by downstream packages like `{ggstatsplot}`
# require `.` as the decimal mark because `,` is parsed as a list separator.
# Additionally, `prettyNum()` emits a warning when `big.mark` and
# `decimal.mark` are identical.

test_that(
  desc = ".prettyNum() does not warn under `options(OutDec = \",\")`",
  code = {
    withr::local_options(list(OutDec = ","))

    expect_no_warning(statsExpressions:::.prettyNum(1234L))
    expect_identical(statsExpressions:::.prettyNum(1234L), "1,234")
  }
)

test_that(
  desc = ".to_char() always uses `.` as decimal mark",
  code = {
    withr::local_options(list(OutDec = ","))

    expect_identical(statsExpressions:::.to_char(5.494, digits = 2L), "5.49")
    expect_identical(statsExpressions:::.to_char(NA_real_), "NA")
  }
)

test_that(
  desc = "add_expression_col() produces valid expressions under `options(OutDec = \",\")`",
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

    expect_no_warning(
      out <- add_expression_col(
        data           = stats_df,
        statistic.text = list(quote(italic("t"))),
        effsize.text   = list(quote(italic("d"))),
        n              = 32L,
        digits         = 3L,
        digits.df      = 3L
      )
    )

    # the returned expression must be parseable as an R language object
    expect_true(is.language(out$expression[[1L]]))
  }
)

test_that(
  desc = "contingency_table() works under `options(OutDec = \",\")`",
  code = {
    withr::local_options(list(OutDec = ","))

    expect_no_warning(df <- contingency_table(mtcars, am))
    expect_true(is.language(df$expression[[1L]]))
  }
)
