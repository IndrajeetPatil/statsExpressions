# `sleep_total` has no missing values, `brainwt` does
one_sample_cases <- tidyr::expand_grid(
  dplyr::tibble(x = c("sleep_total", "brainwt"), test.value = c(5.0, 0.25)),
  type = c("bayes", "parametric", "nonparametric", "robust"),
  dplyr::tibble(conf.level = c(0.95, 0.90), effsize.type = c("g", "d"))
) |>
  dplyr::mutate(
    .test_name = sprintf(
      "%s, type = %s, conf.level = %s, effsize.type = %s",
      x,
      type,
      conf.level,
      effsize.type
    )
  )

patrick::with_parameters_test_that(
  "one_sample_test() works:",
  {
    set.seed(123L)
    # `exec()` inlines the column name, as if it were typed in the call
    res <- rlang::exec(
      one_sample_test,
      data = msleep,
      x = x,
      type = type,
      conf.level = conf.level,
      effsize.type = effsize.type,
      test.value = test.value,
      digits = 3L
    )

    # Bayesian estimation results are too platform-sensitive, so don't
    # snapshot them; only recording Bayes Factors is enough
    if (type == "bayes") {
      expect_snapshot(names(res))
      expect_snapshot(res$bf10[[1L]])
    } else {
      expect_snapshot(select(res, -expression))
      expect_snapshot(res[["expression"]])
    }
  },
  .cases = one_sample_cases
)

# the one-sided interval must extend to the bound of the effect size's range
alternative_cases <- tidyr::expand_grid(
  dplyr::tibble(type = c("parametric", "nonparametric"), bound = c(Inf, 1)),
  dplyr::tibble(
    alternative = c("two.sided", "less", "greater"),
    at_bound = list(c(FALSE, FALSE), c(TRUE, FALSE), c(FALSE, TRUE))
  )
) |>
  dplyr::mutate(.test_name = paste(type, alternative, sep = ", "))

patrick::with_parameters_test_that(
  "one_sample_test() effect size CI follows `alternative`:",
  {
    res <- one_sample_test(
      mtcars,
      wt,
      type = type,
      test.value = 3,
      alternative = alternative
    )
    ci <- c(res$conf.low, res$conf.high)

    expect_identical(ci == c(-bound, bound), at_bound)
  },
  .cases = alternative_cases
)
