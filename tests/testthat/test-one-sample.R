run_one_sample_tests_with_parameters <- function(title, x, test.value) {
  cases_data <- tidyr::expand_grid(
    type = c("bayes", "parametric", "nonparametric", "robust"),
    conf.level = c(0.95, 0.90)
  ) %>%
    dplyr::mutate(
      effsize.type = rep_len(c("g", "d"), length.out = nrow(.)) # relevant only for parametric
    )

  patrick::with_parameters_test_that(
    title,
    {
      set.seed(123L)
      res <- one_sample_test(
        data = msleep,
        x = {{ x }},
        type = type,
        conf.level = conf.level,
        effsize.type = effsize.type,
        conf.method = conf.method,
        test.value = test.value,
        digits = 4L
      )

      if (type != "bayes") {
        expect_snapshot(select(res, -expression))
        expect_snapshot(res[["expression"]])
      } else {
        # Bayesian estimation results are too platform-sensitive, so don't
        # snapshot them; only recording Bayes Factors is enough
        expect_snapshot(names(res))
        expect_snapshot(res$bf10[[1L]])
      }
    },
    .cases = cases_data
  )
}

run_one_sample_tests_with_parameters("one-sample-test without missing data: ", sleep_total, 5.0)
run_one_sample_tests_with_parameters("one-sample-test with missing data: ", brainwt, 0.25)
