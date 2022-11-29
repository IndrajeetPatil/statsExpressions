#' @title Correlation analyses
#' @name corr_test
#'
#' @description
#' Parametric, non-parametric, robust, and Bayesian correlation test.
#'
#' @section Correlation analyses:
#'
#' ```{r child="man/rmd-fragments/table_intro.Rmd"}
#' ```
#'
#' ```{r child="man/rmd-fragments/corr_test.Rmd"}
#' ```
#'
#' @return
#'
#' ```{r child="man/rmd-fragments/return.Rmd"}
#' ```
#'
#' @param x The column in `data` containing the explanatory variable to be
#'   plotted on the `x`-axis.
#' @param y The column in `data` containing the response (outcome) variable to
#'   be plotted on the `y`-axis.
#' @inheritParams oneway_anova
#'
#' @example man/examples/examples-corr_test.R
#' @export
corr_test <- function(data,
                      x,
                      y,
                      type = "parametric",
                      k = 2L,
                      conf.level = 0.95,
                      tr = 0.2,
                      bf.prior = 0.707,
                      ...) {
  # see which method was used to specify type of correlation
  type <- stats_type_switch(type)

  # correlation dataframes -----------------------

  stats_df <- correlation::correlation(
    data           = tidyr::drop_na(select(ungroup(data), {{ x }}, {{ y }})),
    method         = ifelse(type == "nonparametric", "spearman", "pearson"),
    ci             = conf.level,
    bayesian       = type == "bayes",
    bayesian_prior = bf.prior,
    winsorize      = ifelse(type == "robust", tr, FALSE)
  ) %>%
    standardize_names(style = "broom") %>%
    dplyr::mutate(conf.method = ifelse(type == "bayes", "HDI", "normal"))

  # expression ---------------------------------------

  add_expression_col(stats_df, paired = TRUE, k = k)
}
