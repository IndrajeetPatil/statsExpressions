test_that(desc = "contingency_table works", code = {
  # contingency tab - without NAs ---------------------------------

  set.seed(123)
  df1 <- suppressWarnings(contingency_table(
    data = mtcars,
    x = am,
    y = cyl,
    digits = 5L,
    conf.level = 0.99
  ))

  expect_snapshot(select(df1, -expression))
  expect_snapshot(df1[["expression"]])

  set.seed(123)
  df2 <- contingency_table(
    data = as.data.frame(Titanic),
    x = Sex,
    y = Survived,
    counts = Freq
  )

  expect_snapshot(select(df2, -expression))
  expect_snapshot(df2[["expression"]])

  # contingency tab - with NAs --------------------------------------

  set.seed(123)
  df3 <- suppressWarnings(contingency_table(
    data = msleep,
    x = vore,
    y = conservation,
    conf.level = 0.990
  ))

  expect_snapshot(select(df3, -expression))
  expect_snapshot(df3[["expression"]])
})

test_that(desc = "paired contingency_table works ", code = {
  # paired data - without NAs and counts data ----------------------------

  paired_data <- dplyr::tibble(
    response_before = factor(c("no", "yes", "no", "yes")),
    response_after = factor(c("no", "no", "yes", "yes")),
    Freq = c(65L, 25L, 5L, 5L)
  )

  set.seed(123)
  df1 <- suppressWarnings(
    contingency_table(
      data = paired_data,
      x = response_before,
      y = response_after,
      paired = TRUE,
      counts = Freq,
      digits = 5
    )
  )

  expect_snapshot(select(df1, -expression))
  expect_snapshot(df1[["expression"]])

  # paired data with NAs  ---------------------------------------------

  paired_data <- tidyr::uncount(paired_data, weights = Freq)

  # deliberately introduce NAs
  paired_data[c(1L, 12L, 22L, 24L, 65L), 1L] <- NA

  set.seed(123)
  df2 <- suppressWarnings(
    contingency_table(
      data = paired_data,
      x = response_before,
      y = response_after,
      paired = TRUE,
      alternative = "greater",
      digits = 3L,
      conf.level = 0.90
    )
  )

  expect_snapshot(select(df2, -expression))
  expect_snapshot(df2[["expression"]])
})

test_that(desc = "Goodness of Fit contingency_table works without counts", code = {
  # one-sample test (without NAs) -------------------------------------

  set.seed(123)
  df1 <- suppressWarnings(contingency_table(
    data = mtcars,
    x = am,
    conf.level = 0.99,
    digits = 5
  ))

  expect_snapshot(select(df1, -expression))
  expect_snapshot(df1[["expression"]])

  set.seed(123)
  df2 <- contingency_table(
    data = as.data.frame(Titanic),
    x = Sex,
    counts = Freq,
    alternative = "greater"
  )

  expect_snapshot(select(df2, -expression))
  expect_snapshot(df2[["expression"]])

  # one-sample test (with NAs) -------------------------------------

  set.seed(123)
  df3 <- contingency_table(
    data = msleep,
    x = vore,
    ratio = c(0.2, 0.2, 0.3, 0.3)
  )

  expect_snapshot(select(df3, -expression))
  expect_snapshot(df3[["expression"]])

  # edge case
  expect_null(contingency_table(data.frame(x = "x"), x, type = "bayes"))
})

test_that(desc = "bayesian (proportion test)", code = {
  set.seed(123)
  df1 <- contingency_table(
    data = mtcars,
    x = am,
    type = "bayes"
  )

  expect_snapshot(select(df1, -expression))
  expect_snapshot(df1[["expression"]])

  set.seed(123)
  df2 <- contingency_table(
    type = "bayes",
    data = mtcars,
    x = cyl,
    prior.concentration = 10
  )

  expect_snapshot(select(df2, -expression))
  expect_snapshot(df2[["expression"]])
})

test_that(desc = "bayesian (contingency tab)", code = {
  # without NAs
  set.seed(123)
  df1 <- contingency_table(
    type = "bayes",
    data = mtcars,
    x = am,
    y = cyl
  )

  expect_snapshot(df1[["expression"]])

  # with NAs
  set.seed(123)
  df2 <- contingency_table(
    type = "bayes",
    data = msleep,
    x = vore,
    y = conservation
  )

  expect_snapshot(df2[["expression"]])
})

# `options(OutDec)` invariance (#146) -------------------------------------
#
# Regardless of the user's decimal-mark locale, the plotmath expression must
# carry `.` as the decimal mark (plotmath parses `,` as a list separator, so
# `,` would break it) and no `big.mark`/`decimal.mark` clash warning should
# be emitted.

patrick::with_parameters_test_that(
  "contingency_table() expression is invariant to `options(OutDec)`: ",
  {
    withr::local_options(list(OutDec = dec))

    set.seed(123)
    df <- suppressWarnings(contingency_table(
      data = mtcars,
      x = am,
      y = cyl,
      digits = 5L,
      conf.level = 0.99
    ))

    expect_snapshot(df[["expression"]])
  },
  .cases = dplyr::tibble(dec = c(".", ","))
)
