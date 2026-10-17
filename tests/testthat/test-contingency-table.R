test_that("contingency_table works", {
  # contingency tab - without NAs ---------------------------------

  df1 <- suppressWarnings(contingency_table(
    data = mtcars,
    x = am,
    y = cyl,
    digits = 5L,
    conf.level = 0.99
  ))

  expect_snapshot(select(df1, -expression))
  expect_snapshot(df1[["expression"]])

  df2 <- contingency_table(
    data = as.data.frame(Titanic),
    x = Sex,
    y = Survived,
    counts = Freq
  )

  expect_snapshot(select(df2, -expression))
  expect_snapshot(df2[["expression"]])

  # contingency tab - with NAs --------------------------------------

  df3 <- suppressWarnings(contingency_table(
    data = msleep,
    x = vore,
    y = conservation,
    conf.level = 0.990
  ))

  expect_snapshot(select(df3, -expression))
  expect_snapshot(df3[["expression"]])
})

test_that("paired contingency_table works", {
  # paired data - without NAs and counts data ----------------------------

  paired_data <- dplyr::tibble(
    response_before = factor(c("no", "yes", "no", "yes")),
    response_after = factor(c("no", "no", "yes", "yes")),
    Freq = c(65L, 25L, 5L, 5L)
  )

  df1 <- contingency_table(
    data = paired_data,
    x = response_before,
    y = response_after,
    paired = TRUE,
    counts = Freq,
    digits = 5
  )

  expect_snapshot(select(df1, -expression))
  expect_snapshot(df1[["expression"]])

  # paired data with NAs  ---------------------------------------------

  paired_data <- tidyr::uncount(paired_data, weights = Freq)

  # deliberately introduce NAs
  paired_data[c(1L, 12L, 22L, 24L, 65L), 1L] <- NA

  df2 <- contingency_table(
    data = paired_data,
    x = response_before,
    y = response_after,
    paired = TRUE,
    alternative = "greater",
    digits = 3L,
    conf.level = 0.90
  )

  expect_snapshot(select(df2, -expression))
  expect_snapshot(df2[["expression"]])
})

test_that("goodness of fit contingency_table works", {
  # one-sample test (without NAs) -------------------------------------

  df1 <- contingency_table(
    data = mtcars,
    x = am,
    conf.level = 0.99,
    digits = 5
  )

  expect_snapshot(select(df1, -expression))
  expect_snapshot(df1[["expression"]])

  df2 <- contingency_table(
    data = as.data.frame(Titanic),
    x = Sex,
    counts = Freq,
    alternative = "greater"
  )

  expect_snapshot(select(df2, -expression))
  expect_snapshot(df2[["expression"]])

  # one-sample test (with NAs) -------------------------------------

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

test_that("bayesian (proportion test)", {
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

test_that("bayesian (contingency tab)", {
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
  "contingency_table() expression is invariant to `options(OutDec)`:",
  {
    withr::local_options(list(OutDec = dec))

    # only the chi-squared approximation warning is expected for this table
    df <- suppressWarnings(expect_no_warning(
      contingency_table(
        data = mtcars,
        x = am,
        y = cyl,
        digits = 5L,
        conf.level = 0.99
      ),
      message = "big.mark"
    ))

    expect_snapshot(df[["expression"]])
  },
  .cases = dplyr::tibble(dec = c(".", ","))
)

# empty levels ------------------------------------------------------------
#
# A test of independence is undefined with an empty row or column, so unused
# factor levels (including ones emptied by removing missing values) must be
# dropped rather than cause an error.

patrick::with_parameters_test_that(
  "contingency_table() drops empty levels in a test of independence:",
  {
    df <- mutate(mtcars, am = factor(am), cyl = factor(cyl))
    df_unused <- mutate(df, cyl = factor(cyl, levels = c(4, 6, 8, 10)))
    df_emptied <- mutate(
      df,
      am = factor(am, levels = c(0, 1, 2)),
      cyl = replace_when(cyl, am == 1 & cyl == 8 ~ NA)
    )

    run <- function(data) {
      if (type == "bayes") {
        set.seed(123)
      }
      suppressWarnings(
        contingency_table(data, cyl, am, type = type, paired = paired)
      )
    }

    expect_identical(run(df_unused), run(df))
    expect_identical(run(df_emptied), run(filter_out(df, am == 1, cyl == 8)))
  },
  .cases = dplyr::tibble(
    .test_name = c("parametric", "bayes", "bayes, paired"),
    type = c("parametric", "bayes", "bayes"),
    paired = c(FALSE, FALSE, TRUE)
  )
)

test_that("contingency_table() keeps zero-count levels in goodness-of-fit tests", {
  df <- mutate(mtcars, cyl = factor(cyl, levels = c(4, 6, 8, 10)))

  res <- suppressWarnings(
    contingency_table(df, cyl, ratio = c(0.3, 0.2, 0.4, 0.1))
  )

  expect_identical(res$df, 3)
})
