#' @title One-sample tests
#' @name one_sample_test
#'
#' @param x A numeric variable from the data frame `data`.
#' @param test.value A number indicating the true value of the mean (Default:
#'   `0`).
#' @inheritParams long_to_wide_converter
#' @inheritParams stats_type_switch
#' @inheritParams add_expression_col
#' @inheritParams two_sample_test
#' @inheritParams oneway_anova
#' @inheritParams stats::t.test
#'
#' @description
#' Parametric, non-parametric, robust, and Bayesian one-sample tests.
#'
#' @section One-sample tests:
#'
#' ```{r child="man/rmd-fragments/table_intro.Rmd"}
#' ```
#'
#' ```{r child="man/rmd-fragments/one_sample_test.Rmd"}
#' ```
#'
#' @returns
#'
#' ```{r child="man/rmd-fragments/return.Rmd"}
#' ```
#'
#' @autoglobal
#'
#' @example man/examples/examples-one_sample_test.R
#' @export
one_sample_test <- function(data,
                            x,
                            type = "parametric",
                            test.value = 0,
                            alternative = "two.sided",
                            k = 2L,
                            conf.level = 0.95,
                            tr = 0.2,
                            bf.prior = 0.707,
                            ...) {
  type <- stats_type_switch(type)
  x_vec <- stats::na.omit(data %>% pull({{ x }}))

  # Functions for inferential statistics and estimation
  c(.f, .f.es) %<-% switch(type,
    parametric    = list(stats::t.test, effectsize::hedges_g),
    nonparametric = list(stats::wilcox.test, effectsize::rank_biserial),
    robust        = list(WRS2::trimcibt, NULL),
    bayes         = list(BayesFactor::ttestBF, NULL)
  )

  # styler: off
  .f.args <- switch(type,
    parametric    = ,
    nonparametric = list(x = x_vec, mu = test.value, alternative = alternative, exact = FALSE),
    robust        = list(x = x_vec, nv = test.value, tr = tr, alpha = 1 - conf.level),
    bayes         = list(x = x_vec, rscale = bf.prior, mu = test.value)
  )
  # styler: on

  stats_df <- exec(.f, !!!.f.args) %>% tidy_model_parameters(ci = conf.level)

  # These are exceptions because the tidier for the underlying object won't also contain the effect size details
  # so those need to be extracted separately and merged
  if (type %in% c("parametric", "nonparametric")) {
    effsize_df <- exec(.f.es, !!!.f.args, verbose = FALSE, ci = conf.level) %>% tidy_model_effectsize()
    stats_df <- bind_cols(select(stats_df, -matches("^est|^conf|^diff|^term|^ci")), effsize_df)
  }

  add_expression_col(stats_df, n = length(x_vec), k = k)
}
