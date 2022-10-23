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
#' @examples
#' \donttest{
#' # for reproducibility
#' set.seed(123)
#' library(statsExpressions)
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
  # standardize the type of statistics
  type <- stats_type_switch(type)

  # preparing the vector
  x_vec <- stats::na.omit(data %>% pull({{ x }}))

  c(.f, .f.es) %<-% switch(type,
    "parametric"    = list(stats::t.test, effectsize::hedges_g),
    "nonparametric" = list(stats::wilcox.test, effectsize::rank_biserial),
    "robust"        = list(WRS2::trimcibt, NULL),
    "bayes"         = list(BayesFactor::ttestBF, NULL)
  )

  # non-parametric ---------------------------------------

  if (type %in% c("parametric", "nonparametric")) {
    # extracting test details
    stats_df <- exec(.f, x = x_vec, mu = test.value, alternative = alternative) %>%
      tidy_model_parameters() %>%
      select(-matches("^est|^conf|^diff|^term|^ci"))

    # extracting effect size details
    effsize_df <- exec(
      .f.es,
      x       = x_vec,
      mu      = test.value,
      verbose = FALSE,
      ci      = conf.level
    ) %>%
      tidy_model_effectsize()

    stats_df <- bind_cols(stats_df, effsize_df)
  }

  # robust ---------------------------------------

  if (type == "robust") {
    # bootstrap-t method for one-sample test
    stats_df <- exec(.f, x = x_vec, nv = test.value, tr = tr, alpha = 1 - conf.level) %>%
      tidy_model_parameters()
  }

  # Bayesian ---------------------------------------

  if (type == "bayes") {
    stats_df <- exec(.f, x = x_vec, rscale = bf.prior, mu = test.value) %>%
      tidy_model_parameters(ci = conf.level)
  }

  # expression ---------------------------------------

  add_expression_col(stats_df, n = length(x_vec), k = k)
}
