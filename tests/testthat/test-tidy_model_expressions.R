test_that("tidy_model_expressions works", {
  skip_if_not_installed("survival")

  options(tibble.width = Inf)

  ## t-statistic --------------------------------

  set.seed(123)
  mod_t <- lm(wt ~ mpg, data = mtcars)

  set.seed(123)
  expect_snapshot(tidy_model_expressions(
    tidy_model_parameters(mod_t),
    statistic = "t"
  ))

  ## chi2-statistic --------------------------------

  # setup
  library(survival)

  # model
  mod_chi <- survival::coxph(
    formula = Surv(time, status) ~ age + sex + frailty(inst),
    data = lung
  )

  set.seed(123)
  expect_snapshot(tidy_model_expressions(
    tidy_model_parameters(mod_chi),
    statistic = "chi"
  ))

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
  expect_snapshot(tidy_model_expressions(
    tidy_model_parameters(mod_z),
    statistic = "z"
  ))
})
