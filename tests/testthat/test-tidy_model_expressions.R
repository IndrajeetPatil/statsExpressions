test_that("tidy_model_expressions works", {
  skip_if_not_installed("survival")

  options(tibble.width = Inf)

  ## t-statistic --------------------------------

  set.seed(123)
  mod_t <- lm(wt ~ as.factor(am) * as.factor(cyl), data = mtcars)

  set.seed(123)
  expect_snapshot(tidy_model_expressions(
    tidy_model_parameters(mod_t),
    statistic = "t"
  ))

  ## F-statistic --------------------------------

  if (packageVersion("parameters") >= "0.14.0.1") {
    set.seed(123)
    mod_f <- aov(yield ~ N * P * K + Error(block), npk)

    set.seed(123)
    expect_snapshot(tidy_model_expressions(
      tidy_model_parameters(mod_f,
        omega_squared = "partial",
        table_wide = TRUE
      ),
      statistic = "f"
    ))

    set.seed(123)
    expect_snapshot(tidy_model_expressions(
      tidy_model_parameters(mod_f,
        eta_squared = "partial",
        table_wide = TRUE
      ),
      statistic = "f",
      effsize.type = "eta"
    ))
  }

  ## chi2-statistic --------------------------------

  # setup
  library(survival)

  # model
  mod_chi <- survival::coxph(
    formula = Surv(time, status) ~ age + sex + frailty(inst),
    data = lung
  )

  set.seed(123)
  expect_snapshot(tidy_model_expressions(tidy_model_parameters(mod_chi), statistic = "chi"))

  ## z-statistic --------------------------------

  # having a look at the Titanic dataset
  df <- as.data.frame(Titanic)

  # model
  mod_z <-
    stats::glm(
      formula = Survived ~ Sex + Age,
      data = df,
      weights = df$Freq,
      family = stats::binomial(link = "logit")
    )

  set.seed(123)
  expect_snapshot(tidy_model_expressions(tidy_model_parameters(mod_z), statistic = "z"))
})
