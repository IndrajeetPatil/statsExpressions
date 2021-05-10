#' @title One-sample tests
#' @name one_sample_test
#'
#' @param x A numeric variable from the dataframe `data`.
#' @param test.value A number indicating the true value of the mean (Default:
#'   `0`).
#' @param .f.args,.f.es.args A **list** of additional arguments to be passed to
#'   internal functions `.f` and `.f.es`. These are used to carry out
#'   statistical tests and to compute effect sizes, respectively. To see what
#'   additional arguments are available, you can have a look at the
#'   documentation for the respective function. Note that the chosen internal
#'   functions (`.f` and `.f.es`) will themselves depend on the specified `type`
#'   argument. The defaults reflect arguments relevant for parametric tests
#'   because that's the default value for `type`. See `Details` section to see
#'   which internal functions are used. The function might fail if you provide
#'   an argument in a list which the underlying function does not take. This can
#'   happen when you forget that you changed `type` argument, but forget to
#'   change the `.f.args` and `.f.es.args` accordingly. Also, note that these
#'   arguments are useful to provide *additional arguments*. Therefore, you
#'   can's re-specify an argument you have already specified. For example, for
#'   robust tests, you can use `tr` argument to specify trimming level, but then
#'   you can't specify `tr` again inside list passed to `.f.args`
#' @inheritParams ipmisc::long_to_wide_converter
#' @inheritParams ipmisc::stats_type_switch
#' @inheritParams expr_template
#' @inheritParams bf_extractor
#' @inheritParams two_sample_test
#' @inheritParams oneway_anova
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
#' \url{https://indrajeetpatil.github.io/statsExpressions/articles/stats_details.html}
#'
#' @importFrom dplyr select mutate pull rename_all recode
#' @importFrom ipmisc stats_type_switch
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
                            k = 2L,
                            conf.level = 0.95,
                            tr = 0.2,
                            bf.prior = 0.707,
                            effsize.type = "g",
                            .f.args = list(),
                            .f.es.args = list(verbose = FALSE, ci = conf.level),
                            top.text = NULL,
                            ...) {
  # standardize the type of statistics
  type <- ipmisc::stats_type_switch(type)

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
    stats_df <- rlang::exec(.f, x = x_vec, mu = test.value, !!!.f.args) %>%
      tidy_model_parameters(.) %>%
      dplyr::select(-dplyr::matches("^est|^conf|^diff|^term|^ci"))

    # extracting effect size details
    effsize_df <- rlang::exec(.f.es, x = x_vec, mu = test.value, !!!.f.es.args) %>%
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
    stats_df <- rlang::exec(trimcibt, x = x_vec, nv = test.value, tr = tr, !!!.f.es.args)
  }

  # expression
  if (type != "bayes") {
    stats_df %<>%
      dplyr::mutate(expression = list(expr_template(
        no.parameters = no.parameters,
        data = .,
        n = length(x_vec),
        k = k
      )))
  }

  # ----------------------- Bayesian ---------------------------------------

  # running Bayesian one-sample t-test
  if (type == "bayes") {
    bf_object <- BayesFactor::ttestBF(x_vec, rscale = bf.prior, mu = test.value)

    # final return
    stats_df <- bf_extractor(bf_object, conf.level, k = k, top.text = top.text)
  }

  # return the output
  as_tibble(stats_df)
}

#' bootstrap-t method for one-sample test
#' @importFrom WRS2 trimse
#' @noRd

trimcibt <- function(x, tr = 0.2, nboot = 100, nv = 0, ci = 0.95, ...) {
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
