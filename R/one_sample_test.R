#' @title One-sample tests
#' @name one_sample_test
#'
#' @param x A numeric variable from the dataframe `data`.
#' @param test.value A number indicating the true value of the mean (Default:
#'   `0`).
#' @param effsize.type Type of effect size needed for *parametric* tests. The
#'   argument can be `"d"` (for Cohen's *d*) or `"g"` (for Hedge's *g*).
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
#' @return
#'
#' ```{r child="man/rmd-fragments/return.Rmd"}
#' ```
#'
#' @examplesIf requireNamespace("ggplot2", quietly = TRUE)
#' \donttest{
#' # for reproducibility
#' set.seed(123)
#' library(statsExpressions)
#' library(ggplot2) # for data
#' options(tibble.width = Inf, pillar.bold = TRUE, pillar.neg = TRUE)
#'
#' # ----------------------- parametric ---------------------------------------
#'
#' one_sample_test(
#'   data       = ggplot2::msleep,
#'   x          = brainwt,
#'   test.value = 0.275,
#'   type       = "parametric"
#' )
#'
#' # ----------------------- non-parametric -----------------------------------
#'
#' one_sample_test(
#'   data       = ggplot2::msleep,
#'   x          = brainwt,
#'   test.value = 0.275,
#'   type       = "nonparametric"
#' )
#'
#' # ----------------------- robust --------------------------------------------
#'
#' one_sample_test(
#'   data       = ggplot2::msleep,
#'   x          = brainwt,
#'   test.value = 0.275,
#'   type       = "robust"
#' )
#'
#' # ---------------------------- Bayesian -----------------------------------
#'
#' one_sample_test(
#'   data       = ggplot2::msleep,
#'   x          = brainwt,
#'   test.value = 0.275,
#'   type       = "bayes",
#'   bf.prior   = 0.8
#' )
#' }
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
                            effsize.type = "g",
                            ...) {
  # Preparing the vector
  x_vec <- stats::na.omit(data %>% pull({{ x }}))

  # Standardize the type of statistics
  type <- stats_type_switch(type)

  # Functions for inferential statistics and estimation
  c(.f, .f.es) %<-% switch(type,
    "parametric"    = list(stats::t.test, effectsize::hedges_g),
    "nonparametric" = list(stats::wilcox.test, effectsize::rank_biserial),
    "robust"        = list(WRS2::trimcibt, NULL),
    "bayes"         = list(BayesFactor::ttestBF, NULL)
  )

  # Arguments to be supplied to these functions
  # styler: off
  c(.f.args, .f.es.args) %<-% switch(type,
    "parametric"    = ,
    "nonparametric" = list(list(x = x_vec, mu = test.value, alternative = alternative), list(verbose = FALSE, ci = conf.level)),
    "robust"        = list(list(x = x_vec, nv = test.value, tr = tr, alpha = 1 - conf.level), NULL),
    "bayes"         = list(list(x = x_vec, rscale = bf.prior, mu = test.value), NULL)
  )
  # styler: on

  stats_df <- exec(.f, !!!.f.args, !!!.f.es.args) %>% tidy_model_parameters(ci = conf.level)

  # These are exceptions because the tidier for the underlying object won't also contain the effect size details
  # so those need to be extracted separately and merged
  if (type %in% c("parametric", "nonparametric")) {
    effsize_df <- exec(.f.es, !!!.f.args, !!!.f.es.args) %>% tidy_model_effectsize()
    stats_df <- bind_cols(select(stats_df, -matches("^est|^conf|^diff|^term|^ci")), effsize_df)
  }

  add_expression_col(stats_df, n = length(x_vec), k = k)
}
