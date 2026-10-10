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
# Every statistic and effect-size branch is checked against a hand-written
# expression, so that a change to the templates can't silently alter output.
# Each case is also run with columns named like the function's internal
# variables, which must not shadow the templates, and with a second row
# lacking an estimate, which must get a `NULL` expression.

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

    for (data in list(df, df_shadow)) {
      res <- tidy_model_expressions(
        data,
        statistic = statistic,
        effsize.type = effsize.type,
        digits = digits
      )

      expect_identical(res$expression[[1L]], expected)
      expect_null(res$expression[[2L]])
    }
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
    digits = c(2L, 2L, 2L, 3L, 2L, 2L, 2L, 2L, 3L),
    # `F` is the plotmath symbol here, not `FALSE`
    # nolint start: T_and_F_symbol_linter.
    expected = list(
      quote(list(
        widehat(italic(beta)) == "0.50",
        italic(t)("10") == "2.35",
        italic(p) == "0.03"
      )),
      quote(list(
        widehat(italic(beta)) == "0.50",
        italic(t) == "2.35",
        italic(p) == "0.03"
      )),
      quote(list(
        widehat(italic(beta)) == "0.50",
        italic(t) == "2.35",
        italic(p) == "0.03"
      )),
      quote(list(
        widehat(italic(beta)) == "0.500",
        italic(t)("10") == "2.346",
        italic(p) == "0.031"
      )),
      quote(list(
        widehat(italic(beta)) == "0.50",
        italic(z) == "2.35",
        italic(p) == "0.03"
      )),
      quote(list(
        widehat(italic(beta)) == "0.50",
        italic(chi)^2 * ("10") == "2.35",
        italic(p) == "0.03"
      )),
      quote(list(
        widehat(italic(omega)[p]^2) == "0.50",
        italic(F)("2", "10") == "2.35",
        italic(p) == "0.03"
      )),
      quote(list(
        widehat(italic(eta)[p]^2) == "0.50",
        italic(F)("2", "10") == "2.35",
        italic(p) == "0.03"
      )),
      quote(list(
        widehat(italic(omega)[p]^2) == "0.500",
        italic(F)("2", "10") == "2.346",
        italic(p) == "0.031"
      ))
    )
    # nolint end
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

  res <- tidy_model_expressions(df, statistic = "t")

  expect_identical(
    res$expression,
    list(
      quote(list(
        widehat(italic(beta)) == "0.50",
        italic(t)("10") == "2.35",
        italic(p) == "0.03"
      )),
      quote(list(
        widehat(italic(beta)) == "0.50",
        italic(t) == "2.35",
        italic(p) == "0.03"
      )),
      quote(list(
        widehat(italic(beta)) == "0.50",
        italic(t) == "2.35",
        italic(p) == "0.03"
      ))
    )
  )
})
