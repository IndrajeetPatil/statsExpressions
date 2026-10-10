test_that("tidy_model_expressions works - t", {
  df_params <- tidy_model_parameters(lm(wt ~ mpg, data = mtcars))

  df_t <- tidy_model_expressions(df_params, statistic = "t")

  expect_snapshot(select(df_t, -expression))
  expect_snapshot(df_t[["expression"]])

  # with NA df.error
  df_t_na <- tidy_model_expressions(
    mutate(df_params, df.error = NA_real_),
    statistic = "t"
  )

  expect_snapshot(df_t_na[["expression"]])

  # with infinity as error
  df_t_inf <- tidy_model_expressions(
    mutate(df_params, df.error = Inf),
    statistic = "t"
  )

  expect_snapshot(df_t_inf[["expression"]])
})

test_that("tidy_model_expressions works - chi2", {
  skip_if_not_installed("survival")
  withr::local_package("survival")

  mod_chi <- survival::coxph(
    formula = Surv(time, status) ~ age + sex + frailty(inst),
    data = lung
  )

  df_chi <- tidy_model_expressions(
    tidy_model_parameters(mod_chi),
    statistic = "chi"
  )

  expect_snapshot(select(df_chi, -expression))
  expect_snapshot(df_chi[["expression"]])

  # no expression for a row with a missing estimate
  df_chi$estimate[1] <- NA
  df_chi2 <- tidy_model_expressions(
    select(df_chi, -expression),
    statistic = "chi"
  )
  expect_null(df_chi2[["expression"]][[1]])
})

test_that("tidy_model_expressions works - z", {
  df <- as.data.frame(Titanic)

  mod_z <- stats::glm(
    formula = Survived ~ Sex + Age,
    data = df,
    weights = df$Freq,
    family = stats::binomial(link = "logit")
  )

  df_z <- tidy_model_expressions(
    tidy_model_parameters(mod_z),
    statistic = "z"
  )

  expect_snapshot(select(df_z, -expression))
  expect_snapshot(df_z[["expression"]])
})

test_that("tidy_model_expressions works - F", {
  mod_f <- aov(yield ~ N * P + Error(block), npk)

  df1 <- tidy_model_expressions(
    tidy_model_parameters(mod_f, es_type = "omega", table_wide = TRUE),
    statistic = "f"
  )

  expect_snapshot(select(df1, -expression))
  expect_snapshot(df1[["expression"]])

  df2 <- tidy_model_expressions(
    tidy_model_parameters(mod_f, es_type = "eta", table_wide = TRUE),
    statistic = "f",
    effsize.type = "eta"
  )

  expect_snapshot(select(df2, -expression))
  expect_snapshot(df2[["expression"]])
})

# expression templates ---------------------------------------------------
#
# Every statistic and effect-size branch is snapshotted on a small data frame
# whose second row lacks an estimate, so it must get a `NULL` expression.
# Columns named like the function's internal variables must not change the
# result.

patrick::with_parameters_test_that(
  "tidy_model_expressions() builds the expected expression:",
  {
    df <- dplyr::tibble(
      term = c("x", "y"),
      estimate = c(0.5, NA),
      statistic = 2.3456,
      df = 2,
      df.error = df.error,
      p.value = 0.0312
    )
    df_shadow <- mutate(
      df,
      stat_part = "s",
      template = "s",
      template_no_df = "s"
    )

    res <- tidy_model_expressions(
      df,
      statistic = statistic,
      effsize.type = effsize.type,
      digits = digits
    )
    res_shadow <- tidy_model_expressions(
      df_shadow,
      statistic = statistic,
      effsize.type = effsize.type,
      digits = digits
    )

    expect_snapshot(res[["expression"]])
    expect_identical(res_shadow[["expression"]], res[["expression"]])
  },
  .cases = dplyr::tibble(
    .test_name = c(
      "t",
      "t, missing df.error",
      "t, infinite df.error",
      "t, upper case, 3 digits",
      "z",
      "chi",
      "F, omega",
      "F, eta",
      "F, upper case, 3 digits"
    ),
    statistic = c("t", "t", "t", "T", "z", "chi", "f", "f", "F"),
    effsize.type = c(rep("omega", 7L), "eta", "omega"),
    df.error = c(10, NA, Inf, rep(10, 6L)),
    digits = c(2L, 2L, 2L, 3L, 2L, 2L, 2L, 2L, 3L)
  )
)

test_that("tidy_model_expressions() drops the t degrees of freedom per row", {
  df <- dplyr::tibble(
    term = c("x", "y", "z"),
    estimate = 0.5,
    statistic = 2.3456,
    df.error = c(10, NA, Inf),
    p.value = 0.0312
  )

  expect_snapshot(tidy_model_expressions(df, statistic = "t")[["expression"]])
})
