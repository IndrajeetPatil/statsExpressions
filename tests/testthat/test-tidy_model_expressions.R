test_that("tidy_model_expressions works", {
  skip_if_not_installed("survival")

  options(tibble.width = Inf)

  ## t-statistic --------------------------------

  set.seed(123)
  mod_t <- lm(wt ~ mpg, data = mtcars)

  set.seed(123)
  df_t <- suppressWarnings(tidy_model_expressions(
    tidy_model_parameters(mod_t),
    statistic = "t"
  ))

  expect_snapshot(select(df_t, -expression))
  expect_snapshot(df_t$expression)

  # with NA df.error
  set.seed(123)
  df_t_na <- suppressWarnings(tidy_model_expressions(
    mutate(tidy_model_parameters(mod_t), df.error = NA_real_),
    statistic = "t"
  ))

  expect_snapshot(df_t_na$expression)

  # with infinity as error
  set.seed(123)
  df_t_inf <- suppressWarnings(tidy_model_expressions(
    mutate(tidy_model_parameters(mod_t), df.error = Inf),
    statistic = "t"
  ))

  expect_snapshot(df_t_inf$expression)

  ## chi2-statistic --------------------------------

  # setup
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
  expect_snapshot(df_chi$expression)

  ## z-statistic --------------------------------

  # having a look at the Titanic dataset
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
  expect_snapshot(df_z$expression)
})
