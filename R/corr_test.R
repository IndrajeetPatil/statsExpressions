#' @title Expression and dataframe for correlation analysis
#' @name expr_corr_test
#'
#' @return Expression containing results from correlation test with confidence
#'   intervals for the correlation coefficient estimate. Results are extracted
#'   via `correlation::correlation`.
#'
#' @references For more details, see-
#' \url{https://indrajeetpatil.github.io/statsExpressions/articles/stats_details.html}
#'
#' @param x The column in `data` containing the explanatory variable to be
#'   plotted on the `x`-axis. Can be entered either as a character string (e.g.,
#'   `"x"`) or as a bare expression (e.g, `x`).
#' @param y The column in `data` containing the response (outcome) variable to
#'   be plotted on the `y`-axis. Can be entered either as a character string
#'   (e.g., `"y"`) or as a bare expression (e.g, `y`).
#' @param type Type of association between paired samples required
#'   ("`"parametric"`: Pearson's product moment correlation coefficient" or
#'   "`"nonparametric"`: Spearman's rho" or "`"robust"`: percentage bend
#'   correlation coefficient" or "`"bayes"`: Bayes Factor for Pearson's *r*").
#'   Corresponding abbreviations are also accepted: `"p"` (for
#'   parametric/pearson), `"np"` (nonparametric/spearman), `"r"` (robust),
#'   `"bf"` (for bayes factor), resp.
#' @param beta bending constant (Default: `0.1`). For more, see [WRS2::pbcor()].
#' @inheritParams expr_oneway_anova
#'
#' @importFrom dplyr select rename_all recode pull
#' @importFrom correlation correlation
#' @importFrom ipmisc stats_type_switch
#'
#' @examples
#' # for reproducibility
#' set.seed(123)
#' library(statsExpressions)
#'
#' # without changing defaults
#' expr_corr_test(
#'   data = ggplot2::midwest,
#'   x = area,
#'   y = percblack
#' )
#'
#' # changing defaults
#' expr_corr_test(
#'   data = ggplot2::midwest,
#'   x = area,
#'   y = percblack,
#'   type = "robust",
#'   output = "dataframe"
#' )
#' @export

# function body
expr_corr_test <- function(data,
                           x,
                           y,
                           type = "parametric",
                           k = 2L,
                           conf.level = 0.95,
                           beta = 0.1,
                           bf.prior = 0.707,
                           top.text = NULL,
                           output = "expression",
                           ...) {

  # -------------------------- checking corr.method --------------------------

  # see which method was used to specify type of correlation
  type <- ipmisc::stats_type_switch(type)

  # if any of the abbreviations have been entered, change them
  corr.method <-
    switch(
      EXPR = type,
      "bayes" = ,
      "parametric" = "pearson",
      "nonparametric" = "spearman",
      "robust" = "percentage"
    )

  # ----------------- creating correlation dataframes -----------------------

  # creating a dataframe of results
  stats_df <-
    correlation::correlation(
      data = dplyr::select(.data = data, {{ x }}, {{ y }}),
      method = corr.method,
      ci = conf.level,
      bayesian = ifelse(type == "bayes", TRUE, FALSE),
      bayesian_prior = bf.prior,
      bayesian_ci_method = "hdi"
    ) %>%
    parameters::standardize_names(data = ., style = "broom") %>%
    dplyr::mutate(effectsize = method, ci.width = attributes(.)$ci)

  # only relevant for Bayesian
  if (type == "bayes") {
    stats_df %<>%
      dplyr::rename("bf10" = "bayes.factor") %>%
      dplyr::mutate(log_e_bf10 = log(bf10))
  }

  # ---------------------- preparing expression -------------------------------

  # no. of parameters
  no.parameters <- ifelse(type %in% c("parametric", "robust"), 1L, 0L)
  if (type == "nonparametric") stats_df %<>% dplyr::mutate(statistic = log(statistic))

  # preparing expression
  expression <-
    expr_template(
      no.parameters = no.parameters,
      stats.df = stats_df,
      paired = TRUE,
      n = stats_df$n.obs[[1]],
      top.text = top.text,
      k = k,
      bayesian = ifelse(type == "bayes", TRUE, FALSE)
    )

  # return the output
  switch(output, "dataframe" = as_tibble(stats_df), expression)
}
