test_that("tidy_model_expressions works - t", {
  options(tibble.width = Inf)

  set.seed(123)
  mod_t <- lm(wt ~ mpg, data = mtcars)

  set.seed(123)
  df_t <- suppressWarnings(tidy_model_expressions(
    tidy_model_parameters(mod_t),
    statistic = "t"
  ))

  expect_snapshot(select(df_t, -expression))
  expect_snapshot(unlist(df_t$expression))

  # with NA df.error
  set.seed(123)
  df_t_na <- suppressWarnings(tidy_model_expressions(
    mutate(tidy_model_parameters(mod_t), df.error = NA_real_),
    statistic = "t"
  ))

  expect_snapshot(unlist(df_t_na$expression))

  # with infinity as error
  set.seed(123)
  df_t_inf <- suppressWarnings(tidy_model_expressions(
    mutate(tidy_model_parameters(mod_t), df.error = Inf),
    statistic = "t"
  ))

  expect_snapshot(unlist(df_t_inf$expression))
})

test_that("tidy_model_expressions works - chi2", {
  options(tibble.width = Inf)
  skip_if_not_installed("survival")
  library(survival)

  # model
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
  expect_snapshot(unlist(df_chi$expression))
})

test_that("tidy_model_expressions works - z", {
  options(tibble.width = Inf)
  df <- as.data.frame(Titanic)

  # model
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
  expect_snapshot(unlist(df_z$expression))
})


test_that("tidy_model_expressions works - F", {
  options(tibble.width = Inf)

  ## F-statistic --------------------------------

  set.seed(123)
  mod_f <- aov(yield ~ N * P + Error(block), npk)

  set.seed(123)
  df1 <- tidy_model_expressions(
    tidy_model_parameters(mod_f,
      omega_squared = "partial",
      table_wide = TRUE
    ),
    statistic = "f"
  )

  expect_snapshot(select(df1, -expression))
  expect_snapshot(unlist(df1$expression))

  set.seed(123)
  df2 <- tidy_model_expressions(
    tidy_model_parameters(mod_f,
      eta_squared = "partial",
      table_wide = TRUE
    ),
    statistic = "f",
    effsize.type = "eta"
  )

  expect_snapshot(select(df2, -expression))
  expect_snapshot(unlist(df2$expression))
})
