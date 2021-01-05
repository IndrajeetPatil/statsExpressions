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
#' @param ... Additional arguments passed to `tidyBF::bf_ttest`.
#' @inheritParams expr_t_parametric
#' @inheritParams tidyBF::bf_corr_test
#' @inheritParams expr_anova_parametric
#' @inheritParams expr_anova_nonparametric
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
#' @importFrom rcompanion wilcoxonOneSampleR
#' @importFrom ipmisc stats_type_switch
#' @importFrom effectsize cohens_d hedges_g
#' @importFrom stats t.test wilcox.test na.omit
#' @importFrom rlang !! ensym new_formula exec
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
                             conf.type = "norm",
                             bf.prior = 0.707,
                             robust.estimator = "onestep",
                             effsize.type = "g",
                             nboot = 100L,
                             output = "expression",
                             ...) {

  # ====================== data ========================================

  # preparing the vector
  x_vec <- stats::na.omit(data %>% dplyr::pull({{ x }}))

  # standardize the type of statistics
  stats.type <- ipmisc::stats_type_switch(type)

  # ========================= parametric ====================================

  if (stats.type == "parametric") {
    # deciding which effect size to use (Hedge's g or Cohen's d)
    if (effsize.type %in% c("unbiased", "g")) {
      effsize.text <- quote(widehat(italic("g"))["Hedge"])
      .f <- effectsize::hedges_g
    } else {
      effsize.text <- quote(widehat(italic("d"))["Cohen"])
      .f <- effectsize::cohens_d
    }

    # setting up the t-test model and getting its summary
    stats_df <-
      stats::t.test(
        x = x_vec,
        mu = test.value,
        conf.level = conf.level,
        na.action = na.omit
      ) %>%
      tidy_model_parameters(.)

    # creating effect size info
    effsize_df <-
      rlang::exec(
        .fn = .f,
        x = x_vec - test.value,
        ci = conf.level
      ) %>%
      parameters::standardize_names(data = ., style = "broom")

    # preparing expression parameters
    statistic.text <- quote(italic("t")["Student"])
    no.parameters <- 1L
  }

  # ========================== non-parametric ==============================

  if (stats.type == "nonparametric") {
    # setting up the Mann-Whitney U-test and getting its summary
    stats_df <-
      stats::wilcox.test(
        x = x_vec,
        na.action = na.omit,
        mu = test.value,
        exact = FALSE
      ) %>%
      tidy_model_parameters(.) %>%
      dplyr::mutate(.data = ., statistic = log(statistic))

    # effect size dataframe
    effsize_df <-
      rcompanion::wilcoxonOneSampleR(
        x = x_vec,
        mu = test.value,
        ci = TRUE,
        conf = conf.level,
        type = conf.type,
        R = nboot,
        digits = 5,
        reportIncomplete = TRUE
      ) %>%
      rcompanion_cleaner(.)

    # preparing expression parameters
    statistic.text <- quote("log"["e"](italic("V")["Wilcoxon"]))
    no.parameters <- 0L
    effsize.text <- quote(widehat(italic("r")))
  }

  # preparing expression
  if (stats.type %in% c("parametric", "nonparametric")) {
    # combining dataframes
    stats_df <-
      dplyr::bind_cols(dplyr::select(stats_df, -dplyr::matches("estimate|^conf")), effsize_df)

    # expression
    expression <-
      expr_template(
        no.parameters = no.parameters,
        stats.df = stats_df,
        statistic.text = statistic.text,
        effsize.text = effsize.text,
        n = length(x_vec),
        n.text = quote(italic("n")["obs"]),
        conf.level = conf.level,
        k = k
      )
  }

  # ======================= robust =========================================

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
          estimate = specify_decimal_p(x = stats_df$estimate[[1]], k = k),
          conf.level = paste(conf.level * 100, "%", sep = ""),
          LL = specify_decimal_p(x = stats_df$conf.low[[1]], k = k),
          UL = specify_decimal_p(x = stats_df$conf.high[[1]], k = k),
          p.value = specify_decimal_p(x = stats_df$p.value[[1]], k = k, p.value = TRUE),
          n = .prettyNum(length(x_vec))
        )
      )
  }

  # ======================== bayes ============================================

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
  switch(output, "dataframe" = stats_df, expression)
}
