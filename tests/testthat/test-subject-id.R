patrick::with_parameters_test_that(
  "`subject.id` gives the same result as data sorted by subject:",
  {
    expect_subject_id_invariance(fun, data, type = type)
  },
  .cases = dplyr::mutate(
    tidyr::expand_grid(
      dplyr::tibble(
        fun_name = c("oneway_anova", "pairwise_comparisons", "two_sample_test"),
        fun = list(oneway_anova, pairwise_comparisons, two_sample_test),
        data = list(
          data_with_subid,
          dplyr::rename(
            WRS2::WineTasting,
            condition = Wine,
            score = Taste,
            id = Taster
          ),
          data_with_subid_two_groups
        )
      ),
      type = c("parametric", "nonparametric", "robust", "bayes")
    ),
    .test_name = paste(fun_name, type, sep = ", ")
  )
)
