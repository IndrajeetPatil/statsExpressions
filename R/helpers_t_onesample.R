#' @title Expression for one sample *t*-test and its non-parametric and
#'   robust equivalents
#' @name expr_t_onesample
#'
#' @param x A numeric variable from the dataframe `data`.
#' @param test.value A number specifying the value of the null hypothesis
#'   (Default: `0`).
#' @param type Type of statistic expected (`"parametric"` or `"nonparametric"`
#'   or `"robust"` or `"bayes"`).Corresponding abbreviations are also accepted:
#'   `"p"` (for parametric), `"np"` (nonparametric), `"r"` (robust), or
#'   `"bf"`resp.
#' @inheritParams ipmisc::long_to_wide_converter
#' @inheritParams expr_t_twosample
#' @inheritParams expr_template
#' @inheritParams expr_oneway_anova
#' @inheritParams tidyBF::bf_ttest
#'
#' @return Expression containing results from a one-sample test. The exact test
#'   and the effect size details contained will be dependent on the `type`
#'   argument.
#'
#' @references For more details, see-
#' \url{https://indrajeetpatil.github.io/statsExpressions/articles/stats_details.html}
#'
#' @importFrom dplyr select mutate pull rename_all recode
#' @importFrom ipmisc stats_type_switch
#' @importFrom effectsize cohens_d hedges_g rank_biserial
#' @importFrom stats t.test wilcox.test na.omit
#' @importFrom rlang !! !!! enquo eval_tidy expr enexpr ensym exec new_formula
#'
#' @examples
#' \donttest{
#' # for reproducibility
#' set.seed(123)
#' library(statsExpressions)
#'
#' # ----------------------- parametric ---------------------------------------
#'
#' expr_t_onesample(
#'   data = ggplot2::msleep,
#'   x = brainwt,
#'   test.value = 0.275,
#'   type = "parametric"
#' )
#'
#' # ----------------------- non-parametric -----------------------------------
#'
#' expr_t_onesample(
#'   data = ggplot2::msleep,
#'   x = brainwt,
#'   test.value = 0.275,
#'   type = "nonparametric"
#' )
#'
#' # ----------------------- robust --------------------------------------------
#'
#' expr_t_onesample(
#'   data = ggplot2::msleep,
#'   x = brainwt,
#'   test.value = 0.275,
#'   type = "robust"
#' )
#'
#' # ----------------------- Bayes Factor -----------------------------------
#'
#' expr_t_onesample(
#'   data = ggplot2::msleep,
#'   x = brainwt,
#'   test.value = 0.275,
#'   type = "bayes",
#'   bf.prior = 0.8
#' )
#' }
#' @export

expr_t_onesample <- function(data,
                             x,
                             type = "parametric",
                             test.value = 0,
                             k = 2L,
                             conf.level = 0.95,
                             tr = 0.1,
                             bf.prior = 0.707,
                             effsize.type = "g",
                             nboot = 100L,
                             output = "expression",
                             ...) {
  # preparing the vector
  x_vec <- stats::na.omit(data %>% dplyr::pull({{ x }}))

  # standardize the type of statistics
  stats.type <- ipmisc::stats_type_switch(type)

  # ----------------------- parametric ---------------------------------------

  if (stats.type == "parametric") {
    # preparing expression parameters
    no.parameters <- 1L
    .f <- stats::t.test
    if (effsize.type %in% c("unbiased", "g")) .f.es <- effectsize::hedges_g
    if (effsize.type %in% c("biased", "d")) .f.es <- effectsize::cohens_d
  }

  # ----------------------- non-parametric ---------------------------------------

  if (stats.type == "nonparametric") {
    # preparing expression parameters
    no.parameters <- 0L
    c(.f, .f.es) %<-% c(stats::wilcox.test, effectsize::rank_biserial)
  }

  # preparing expression
  if (stats.type %in% c("parametric", "nonparametric")) {
    # extracting test details
    stats_df <-
      rlang::exec(
        .fn = .f,
        x = x_vec,
        mu = test.value,
        na.action = na.omit,
        exact = FALSE
      ) %>%
      tidy_model_parameters(.) %>%
      dplyr::select(-dplyr::matches("^est|^conf|^diff|^term"))

    # extracting effect size details
    effsize_df <-
      rlang::exec(
        .fn = .f.es,
        x = x_vec - test.value,
        ci = conf.level,
        iterations = nboot
      ) %>%
      tidy_model_effectsize(.)

    # these can be really big values
    if (stats.type == "nonparametric") stats_df %<>% dplyr::mutate(statistic = log(statistic))

    # dataframe
    stats_df <- dplyr::bind_cols(stats_df, effsize_df)
  }

  # ----------------------- robust ---------------------------------------

  if (stats.type == "robust") {
    # bootstrap-t method for one-sample test
    no.parameters <- 0L
    stats_df <-
      trimcibt(
        x = x_vec,
        tr = tr,
        nboot = nboot,
        nv = test.value,
        alpha = 1 - conf.level
      )
  }

  # expression
  if (stats.type != "bayes") {
    expression <-
      expr_template(
        no.parameters = no.parameters,
        stats.df = stats_df,
        n = length(x_vec),
        conf.level = conf.level,
        k = k
      )
  }

  # ----------------------- Bayesian ---------------------------------------

  # running Bayesian one-sample t-test
  if (stats.type == "bayes") {
    stats_df <-
      tidyBF::bf_ttest(
        data = data,
        x = {{ x }},
        test.value = test.value,
        bf.prior = bf.prior,
        output = output,
        k = k,
        ...
      )

    expression <- stats_df
  }

  # return the output
  switch(output, "dataframe" = as_tibble(stats_df), expression)
}

#' bootstrap-t method for one-sample test
#' @importFrom WRS2 trimse
#' @noRd

trimcibt <- function(x, tr = 0.2, nboot = 200, nv = 0, alpha = 0.05, ...) {
  test <- (mean(x, tr) - nv) / WRS2::trimse(x, tr)
  data <- matrix(sample(x, size = length(x) * nboot, replace = TRUE), nrow = nboot) - mean(x, tr)
  tval <- sort(abs(apply(data, 1, mean, tr) / apply(data, 1, WRS2::trimse, tr)))
  icrit <- round((1 - alpha) * nboot)

  tibble(
    estimate = mean(x, tr),
    conf.low = mean(x, tr) - tval[icrit] * WRS2::trimse(x, tr),
    conf.high = mean(x, tr) + tval[icrit] * WRS2::trimse(x, tr),
    statistic = test,
    p.value = (sum(abs(test) <= abs(tval))) / nboot,
    n = length(x),
    effectsize = "Trimmed mean",
    method = "Bootstrap-t method for one-sample test"
  )
}
