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
#' @inheritParams expr_template
#' @inheritParams two_sample_test
#' @inheritParams oneway_anova
#' @inheritParams stats::t.test
#'
#' @description
#'
#' A dataframe containing results from a one-sample test.
#'
#' @details
#'
#' The exact test and the effect size details contained will depend on the
#' `type` argument.
#'
#'   Internal function `.f` used to carry out statistical test:
#'   - **parametric**: `stats::t.test`
#'   - **nonparametric**: `stats::wilcox.test`
#'   - **robust**: `WRS2::trimcibt`
#'   - **bayes**: `BayesFactor::ttestBF`
#'
#'   Internal function `.f.es` used to compute effect size:
#'   - **parametric**: `effectsize::cohens_d`, `effectsize::hedges_g`
#'   - **nonparametric**: `effectsize::rank_biserial`
#'   - **robust**: `WRS2::trimcibt`
#'   - **bayes**: `bayestestR::describe_posterior`
#'
#' For more, see-
#' <https://indrajeetpatil.github.io/statsExpressions/articles/stats_details.html>
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
                            top.text = NULL,
                            ...) {
  # standardize the type of statistics
  type <- stats_type_switch(type)

  # preparing the vector
  x_vec <- stats::na.omit(data %>% pull({{ x }}))

  # parametric ---------------------------------------

  # preparing expression parameters
  if (type == "parametric") {
    .f <- stats::t.test
    # styler: off
    if (effsize.type %in% c("unbiased", "g")) .f.es <- effectsize::hedges_g
    if (effsize.type %in% c("biased", "d")) .f.es   <- effectsize::cohens_d
    # styler: on
  }

  # non-parametric ---------------------------------------

  # preparing expression parameters
  if (type == "nonparametric") c(.f, .f.es) %<-% c(stats::wilcox.test, effectsize::rank_biserial)

  # preparing expression
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

  # running Bayesian one-sample t-test
  if (type == "bayes") {
    stats_df <- BayesFactor::ttestBF(x = x_vec, rscale = bf.prior, mu = test.value) %>%
      tidy_model_parameters(ci = conf.level)
  }

  # expression ---------------------------------------

  # add column with expression
  polish_data(stats_df) %>%
    mutate(expression = list(expr_template(
      data            = .,
      n               = length(x_vec),
      k               = k,
      top.text        = top.text
    )))
}
