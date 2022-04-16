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

  # parametric ---------------------------------------


  if (type == "parametric") {
    .f <- stats::t.test
    # styler: off
    if (effsize.type %in% c("unbiased", "g")) .f.es <- effectsize::hedges_g
    if (effsize.type %in% c("biased", "d")) .f.es   <- effectsize::cohens_d
    # styler: on
  }

  # non-parametric ---------------------------------------


  if (type == "nonparametric") c(.f, .f.es) %<-% c(stats::wilcox.test, effectsize::rank_biserial)

  if (type %in% c("parametric", "nonparametric")) {
    # extracting test details
    stats_df <- exec(.f, x = x_vec, mu = test.value, alternative = alternative) %>%
      tidy_model_parameters(.) %>%
      select(-matches("^est|^conf|^diff|^term|^ci"))

    # extracting effect size details
    effsize_df <- exec(
      .f.es,
      x       = x_vec,
      mu      = test.value,
      verbose = FALSE,
      ci      = conf.level
    ) %>%
      tidy_model_effectsize(.)

    # dataframe
    stats_df <- bind_cols(stats_df, effsize_df)
  }

  # robust ---------------------------------------

  if (type == "robust") {
    # bootstrap-t method for one-sample test
    stats_df <- exec(WRS2::trimcibt, x = x_vec, nv = test.value, tr = tr, alpha = 1 - conf.level) %>%
      tidy_model_parameters(.)
  }

  # Bayesian ---------------------------------------

  if (type == "bayes") {
    stats_df <- BayesFactor::ttestBF(x = x_vec, rscale = bf.prior, mu = test.value) %>%
      tidy_model_parameters(ci = conf.level)
  }

  # expression ---------------------------------------

  add_expression_col(stats_df, n = length(x_vec), k = k)
}
