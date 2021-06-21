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
#'   - **robust**: `trimcibt` (custom)
#'   - **bayes**: `BayesFactor::ttestBF`
#'
#'   Internal function `.f.es` used to compute effect size:
#'   - **parametric**: `effectsize::cohens_d`, `effectsize::hedges_g`
#'   - **nonparametric**: `effectsize::rank_biserial`
#'   - **robust**: `trimcibt` (custom)
#'   - **bayes**: `bayestestR::describe_posterior`
#'
#' For more, see-
#' <https://indrajeetpatil.github.io/statsExpressions/articles/stats_details.html>
#'
#' @importFrom dplyr select mutate pull rename_all recode
#' @importFrom effectsize cohens_d hedges_g rank_biserial
#' @importFrom stats t.test wilcox.test na.omit
#' @importFrom rlang !!! exec
#' @importFrom BayesFactor ttestBF
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
#'   data = ggplot2::msleep,
#'   x = brainwt,
#'   test.value = 0.275,
#'   type = "parametric"
#' )
#'
#' # ----------------------- non-parametric -----------------------------------
#'
#' one_sample_test(
#'   data = ggplot2::msleep,
#'   x = brainwt,
#'   test.value = 0.275,
#'   type = "nonparametric"
#' )
#'
#' # ----------------------- robust --------------------------------------------
#'
#' one_sample_test(
#'   data = ggplot2::msleep,
#'   x = brainwt,
#'   test.value = 0.275,
#'   type = "robust"
#' )
#'
#' # ---------------------------- Bayesian -----------------------------------
#'
#' one_sample_test(
#'   data = ggplot2::msleep,
#'   x = brainwt,
#'   test.value = 0.275,
#'   type = "bayes",
#'   bf.prior = 0.8
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
  x_vec <- stats::na.omit(data %>% dplyr::pull({{ x }}))

  # ----------------------- parametric ---------------------------------------

  if (type == "parametric") {
    # preparing expression parameters
    no.parameters <- 1L
    .f <- stats::t.test
    if (effsize.type %in% c("unbiased", "g")) .f.es <- effectsize::hedges_g
    if (effsize.type %in% c("biased", "d")) .f.es <- effectsize::cohens_d
  }

  # ----------------------- non-parametric ---------------------------------------

  if (type == "nonparametric") {
    # preparing expression parameters
    no.parameters <- 0L
    c(.f, .f.es) %<-% c(stats::wilcox.test, effectsize::rank_biserial)
  }

  # preparing expression
  if (type %in% c("parametric", "nonparametric")) {
    # extracting test details
    stats_df <- rlang::exec(.f, x = x_vec, mu = test.value, alternative = alternative) %>%
      tidy_model_parameters(.) %>%
      dplyr::select(-dplyr::matches("^est|^conf|^diff|^term|^ci"))

    # extracting effect size details
    effsize_df <- rlang::exec(
      .f.es,
      x = x_vec,
      mu = test.value,
      verbose = FALSE,
      ci = conf.level
    ) %>%
      tidy_model_effectsize(.)

    # these can be really big values
    if (type == "nonparametric") stats_df %<>% dplyr::mutate(statistic = log(statistic))

    # dataframe
    stats_df <- dplyr::bind_cols(stats_df, effsize_df)
  }

  # ----------------------- robust ---------------------------------------

  if (type == "robust") {
    # bootstrap-t method for one-sample test
    no.parameters <- 0L
    stats_df <- rlang::exec(trimcibt, x = x_vec, nv = test.value, tr = tr, ci = conf.level)
  }

  # ----------------------- Bayesian ---------------------------------------

  # running Bayesian one-sample t-test
  if (type == "bayes") {
    stats_df <- BayesFactor::ttestBF(x_vec, rscale = bf.prior, mu = test.value) %>%
      tidy_model_parameters(ci = conf.level)
  }

  # ----------------------- expression ---------------------------------------

  # return the output
  as_tibble(stats_df) %>%
    dplyr::mutate(expression = list(expr_template(
      data = .,
      no.parameters = no.parameters,
      n = length(x_vec),
      k = k,
      top.text = top.text,
      bayesian = ifelse(type == "bayes", TRUE, FALSE)
    )))
}

#' bootstrap-t method for one-sample test
#' @importFrom WRS2 trimse
#' @noRd

trimcibt <- function(x, nv = 0, tr = 0.2, nboot = 100L, ci = 0.95, ...) {
  test <- (mean(x, tr) - nv) / WRS2::trimse(x, tr)
  data <- matrix(sample(x, size = length(x) * nboot, replace = TRUE), nrow = nboot) - mean(x, tr)
  tval <- sort(abs(apply(data, 1, mean, tr) / apply(data, 1, WRS2::trimse, tr)))
  icrit <- round(ci * nboot)

  tibble(
    statistic = test,
    p.value = (sum(abs(test) <= abs(tval))) / nboot,
    method = "Bootstrap-t method for one-sample test",
    estimate = mean(x, tr),
    conf.low = mean(x, tr) - tval[icrit] * WRS2::trimse(x, tr),
    conf.high = mean(x, tr) + tval[icrit] * WRS2::trimse(x, tr),
    conf.level = ci,
    effectsize = "Trimmed mean"
  )
}
