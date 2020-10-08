#' @title Making expression for correlation analysis
#' @name expr_corr_test
#'
#' @return Expression containing results from correlation test with confidence
#'   intervals for the correlation coefficient estimate.
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
#' @inheritParams tidyBF::bf_corr_test
#' @inheritParams expr_anova_parametric
#' @inheritParams expr_anova_nonparametric
#'
#' @importFrom dplyr select rename_all recode
#' @importFrom correlation correlation
#' @importFrom ipmisc easystats_to_tidy_names stats_type_switch
#'
#' @examples
#'
#' # for reproducibility
#' set.seed(123)
#'
#' # without changing defaults
#' statsExpressions::expr_corr_test(
#'   data = ggplot2::midwest,
#'   x = area,
#'   y = percblack,
#'   type = "parametric"
#' )
#'
#' # changing defaults
#' statsExpressions::expr_corr_test(
#'   data = ggplot2::midwest,
#'   x = area,
#'   y = percblack,
#'   beta = 0.2,
#'   type = "robust"
#' )
#' @export

# function body
expr_corr_test <- function(data,
                           x,
                           y,
                           k = 2L,
                           conf.level = 0.95,
                           beta = 0.1,
                           type = "parametric",
                           bf.prior = 0.707,
                           ...) {

  # ============================ checking corr.method =======================

  # see which method was used to specify type of correlation
  stats_type <- ipmisc::stats_type_switch(type)

  # if any of the abbreviations have been entered, change them
  corr.method <-
    switch(
      EXPR = stats_type,
      "parametric" = "pearson",
      "nonparametric" = "spearman",
      "robust" = "percentage"
    )

  #----------------- creating correlation dataframes -----------------------

  # for all except `bayes`
  if (stats_type != "bayes") {
    # creating a dataframe of results
    stats_df <-
      correlation::correlation(
        data = dplyr::select(.data = data, {{ x }}, {{ y }}),
        method = corr.method,
        ci = conf.level
      ) %>%
      ipmisc::easystats_to_tidy_names(.) %>%
      dplyr::rename_all(.tbl = ., .funs = dplyr::recode, "df" = "parameter")

    # effect size dataframe is the same one
    effsize_df <- stats_df
  }

  #------------------------ subtitle text elements -----------------------------

  # preparing other needed objects
  if (stats_type == "parametric") {
    no.parameters <- 1L
    statistic.text <- quote(italic("t")["Student"])
    effsize.text <- quote(widehat(italic("r"))["Pearson"])
  }

  if (stats_type == "nonparametric") {
    stats_df %<>% dplyr::mutate(.data = ., statistic = log(statistic))
    no.parameters <- 0L
    statistic.text <- quote("log"["e"](italic("S")))
    effsize.text <- quote(widehat(italic(rho))["Spearman"])
  }

  if (stats_type == "robust") {
    no.parameters <- 1L
    statistic.text <- quote(italic("t")["Student"])
    effsize.text <- quote(widehat(italic(rho))["pb"])
  }

  #---------------------- preparing subtitle ---------------------------------

  if (stats_type != "bayes") {
    # preparing subtitle
    subtitle <-
      expr_template(
        no.parameters = no.parameters,
        stats.df = stats_df,
        effsize.df = effsize_df,
        statistic.text = statistic.text,
        effsize.text = effsize.text,
        n = stats_df$n.obs[[1]],
        conf.level = conf.level,
        k = k,
        n.text = quote(italic("n")["pairs"])
      )
  } else {
    # bayes factor results
    subtitle <-
      tidyBF::bf_corr_test(
        data = data,
        x = {{ x }},
        y = {{ y }},
        bf.prior = bf.prior,
        output = "h1",
        k = k
      )$expr
  }

  # return the subtitle
  return(subtitle)
}
