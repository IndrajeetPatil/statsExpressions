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
#' @returns
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
#' @autoglobal
#'
#' @example man/examples/examples-corr-test.R
#' @export
corr_test <- function(
    data,
    x,
    y,
    type = "parametric",
    digits = 2L,
    conf.level = 0.95,
    tr = 0.2,
    bf.prior = 0.707,
    ...) {
  type <- extract_stats_type(type)

  stats_df <- correlation::correlation(
    data           = select(ungroup(data), {{ x }}, {{ y }}) %>% tidyr::drop_na(),
    method         = ifelse(type == "nonparametric", "spearman", "pearson"),
    ci             = conf.level,
    bayesian       = type == "bayes",
    bayesian_prior = bf.prior,
    winsorize      = ifelse(type == "robust", tr, FALSE)
  ) %>%
    standardize_names(style = "broom") %>%
    dplyr::mutate(conf.method = ifelse(type == "bayes", "HDI", "normal"))

  add_expression_col(stats_df, paired = TRUE, digits = digits)
}
