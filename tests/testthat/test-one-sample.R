run_one_sample_tests_with_parameters(
  "one-sample-test without missing data: ",
  sleep_total,
  5.0
)
run_one_sample_tests_with_parameters(
  "one-sample-test with missing data: ",
  brainwt,
  0.25
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
