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
#' @param robust.estimator If `type = "robust"`, a robust estimator to be used
#'   (`"onestep"` (Default), `"mom"`, or `"median"`). For more, see
#'   `?WRS2::onesampb`.
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
#' @importFrom WRS2 onesampb
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
                             bf.prior = 0.707,
                             robust.estimator = "onestep",
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
    .f <- stats::wilcox.test
    .f.es <- effectsize::rank_biserial
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
      tidy_model_parameters(.)

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
    stats_df <- dplyr::bind_cols(dplyr::select(stats_df, -dplyr::matches("^est|^conf|^diff")), effsize_df)

    # expression
    expression <-
      expr_template(
        no.parameters = no.parameters,
        stats.df = stats_df,
        n = length(x_vec),
        conf.level = conf.level,
        k = k
      )
  }

  # ----------------------- robust ---------------------------------------

  if (stats.type == "robust") {
    # running one-sample percentile bootstrap
    mod <-
      WRS2::onesampb(
        x = x_vec,
        est = robust.estimator,
        nboot = nboot,
        nv = test.value,
        alpha = 1 - conf.level
      )

    # create a dataframe
    stats_df <- tidy_model_parameters(mod)

    # preparing the expression
    expression <-
      substitute(
        expr = paste(
          italic("M")["robust"],
          " = ",
          estimate,
          ", CI"[conf.level],
          " [",
          LL,
          ", ",
          UL,
          "], ",
          italic("p"),
          " = ",
          p.value,
          ", ",
          italic("n")["obs"],
          " = ",
          n
        ),
        env = list(
          estimate = format_num(stats_df$estimate[[1]], k = k),
          conf.level = paste0(conf.level * 100, "%"),
          LL = format_num(stats_df$conf.low[[1]], k = k),
          UL = format_num(stats_df$conf.high[[1]], k = k),
          p.value = format_num(stats_df$p.value[[1]], k = k, p.value = TRUE),
          n = .prettyNum(length(x_vec))
        )
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
