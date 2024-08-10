arg_data <- tidyr::expand_grid(
  type = c("parametric", "nonparametric", "bayes", "robust"),
  conf.level = c(0.95, 0.90)
) %>%
  dplyr::mutate(
    effsize.type = rep_len(c("g", "d"), length.out = nrow(.)) # relevant only for parametric
  )

patrick::with_parameters_test_that(
  "one-sample-test works without missing data: ",
  {
    res <- one_sample_test(
      data = msleep,
      x = sleep_total,
      type = type,
      conf.level = conf.level,
      effsize.type = effsize.type,
      conf.method = conf.method,
      test.value = 5.0,
      digits = 4L,
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
  .cases = arg_data
)


patrick::with_parameters_test_that(
  "one-sample-test with missing data: ",
  {
    res <- one_sample_test(
      data = msleep,
      x = brainwt,
      type = type,
      conf.level = conf.level,
      effsize.type = effsize.type,
      conf.method = conf.method,
      test.value = 0.25,
      digits = 4L,
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
  .cases = arg_data
)
