#' @title Correlation analyses
#' @name corr_test
#'
#' @description
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
#' @examples
#' \donttest{
#' # for reproducibility
#' set.seed(123)
#' library(statsExpressions)
#' options(tibble.width = Inf, pillar.bold = TRUE, pillar.neg = TRUE)
#'
#' # without changing defaults
#' corr_test(
#'   data = ggplot2::midwest,
#'   x    = area,
#'   y    = percblack
#' )
#'
#' # changing defaults
#' corr_test(
#'   data = ggplot2::midwest,
#'   x    = area,
#'   y    = percblack,
#'   type = "robust"
#' )
#' }
#' @export

# function body
corr_test <- function(data,
                      x,
                      y,
                      type = "parametric",
                      k = 2L,
                      conf.level = 0.95,
                      tr = 0.2,
                      bf.prior = 0.707,
                      top.text = NULL,
                      ...) {

  # see which method was used to specify type of correlation
  type <- stats_type_switch(type)

  # correlation dataframes -----------------------

  # creating a dataframe of results
  stats_df <- correlation::correlation(
    data           = tidyr::drop_na(select(ungroup(data), {{ x }}, {{ y }})),
    method         = ifelse(type == "nonparametric", "spearman", "pearson"),
    ci             = conf.level,
    bayesian       = ifelse(type == "bayes", TRUE, FALSE),
    bayesian_prior = bf.prior,
    winsorize      = ifelse(type == "robust", tr, FALSE)
  ) %>%
    standardize_names(style = "broom")

  # expression ---------------------------------------

  # preparing expression
  add_expression_col(
    data     = stats_df,
    top.text = top.text,
    paired   = TRUE,
    n        = stats_df$n.obs[[1]],
    k        = k
  )
}
