test_that("tidy_model_expressions works - t", {
  set.seed(123)
  mod_t <- lm(wt ~ mpg, data = mtcars)

  set.seed(123)
  df_t <- suppressWarnings(tidy_model_expressions(
    tidy_model_parameters(mod_t),
    statistic = "t"
  ))

  expect_snapshot(select(df_t, -expression))
  expect_snapshot(df_t[["expression"]])

  # with NA df.error
  set.seed(123)
  df_t_na <- suppressWarnings(tidy_model_expressions(
    mutate(tidy_model_parameters(mod_t), df.error = NA_real_),
    statistic = "t"
  ))

  expect_snapshot(df_t_na[["expression"]])

  # with infinity as error
  set.seed(123)
  df_t_inf <- suppressWarnings(tidy_model_expressions(
    mutate(tidy_model_parameters(mod_t), df.error = Inf),
    statistic = "t"
  ))

  expect_snapshot(df_t_inf[["expression"]])
})

test_that("tidy_model_expressions works - chi2", {
  skip_if_not_installed("survival")
  withr::local_package("survival")

  mod_chi <- survival::coxph(
    formula = Surv(time, status) ~ age + sex + frailty(inst),
    data = lung
  )

  set.seed(123)
  df_chi <- suppressWarnings(tidy_model_expressions(
    tidy_model_parameters(mod_chi),
    statistic = "chi"
  ))

  expect_snapshot(select(df_chi, -expression))
  expect_snapshot(df_chi[["expression"]])

  df_chi$estimate[1] <- NA
  df_chi <- dplyr::select(df_chi, -expression)
  df_chi2 <- suppressWarnings(tidy_model_expressions(df_chi, statistic = "chi"))
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

  set.seed(123)
  df_z <- suppressWarnings(tidy_model_expressions(
    tidy_model_parameters(mod_z),
    statistic = "z"
  ))

  expect_snapshot(select(df_z, -expression))
  expect_snapshot(df_z[["expression"]])
})


test_that("tidy_model_expressions works - F", {
  set.seed(123)
  mod_f <- aov(yield ~ N * P + Error(block), npk)

  set.seed(123)
  df1 <- tidy_model_expressions(
    tidy_model_parameters(
      mod_f,
      es_type = "omega",
      table_wide = TRUE
    ),
    statistic = "f"
  )

  expect_snapshot(select(df1, -expression))
  expect_snapshot(df1[["expression"]])

  set.seed(123)
  df2 <- tidy_model_expressions(
    tidy_model_parameters(
      mod_f,
      es_type = "eta",
      table_wide = TRUE
    ),
    statistic = "f",
    effsize.type = "eta"
  )

  expect_snapshot(select(df2, -expression))
  expect_snapshot(df2[["expression"]])
})
