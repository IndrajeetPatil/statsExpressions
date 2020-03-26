#' @title Making expression for correlation analysis
#' @name expr_corr_test
#' @author \href{https://github.com/IndrajeetPatil}{Indrajeet Patil}
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
#'   parametric/pearson's), `"np"` (nonparametric/spearman), `"r"` (robust),
#'   `"bf"` (for bayes factor), resp.
#' @param beta bending constant (Default: `0.1`). For more, see `?WRS2::pbcor`.
#' @inheritParams tidyBF::bf_corr_test
#' @inheritParams expr_anova_parametric
#' @inheritParams expr_anova_nonparametric
#'
#' @importFrom dplyr select
#' @importFrom correlation correlation
#' @importFrom broomExtra easystats_to_tidy_names
#' @importFrom rlang !! enquo enexpr ensym enexpr
#' @importFrom stats cor.test
#' @importFrom rcompanion spearmanRho
#' @importFrom ipmisc stats_type_switch
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
#'   nboot = 25,
#'   beta = 0.2,
#'   type = "robust",
#'   k = 1
#' )
#' @export

# function body
expr_corr_test <- function(data,
                           x,
                           y,
                           nboot = 100,
                           beta = 0.1,
                           type = "parametric",
                           bf.prior = 0.707,
                           conf.level = 0.95,
                           conf.type = "norm",
                           k = 2,
                           stat.title = NULL,
                           ...) {

  # make sure both quoted and unquoted arguments are supported
  c(x, y) %<-% c(rlang::ensym(x), rlang::ensym(y))

  #------------------------ dataframe -------------------------------------

  # if dataframe is provided
  data %<>%
    dplyr::select(.data = ., {{ x }}, {{ y }}) %>%
    tidyr::drop_na(.) %>%
    as_tibble(.)

  # the total sample size for analysis
  sample_size <- nrow(data)

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
        data = data,
        method = corr.method,
        ci = conf.level,
        beta = beta
      ) %>%
      broomExtra::easystats_to_tidy_names(.) %>%
      dplyr::rename_all(
        .tbl = .,
        .funs = dplyr::recode,
        "r" = "estimate",
        "rho" = "estimate",
        "df" = "parameter",
        "s" = "statistic"
      )

    # effect size dataframe is the same one
    effsize_df <- stats_df
  }

  # `correlation` doesn't return CIs for Spearman'r rho
  if (stats_type == "nonparametric") {
    stats_df %<>% dplyr::mutate(.data = ., statistic = log(statistic))

    # getting confidence interval for rho using `rcompanion`
    effsize_df <-
      rcompanion::spearmanRho(
        x = data %>% dplyr::pull({{ x }}),
        y = data %>% dplyr::pull({{ y }}),
        method = "spearman",
        ci = TRUE,
        conf = conf.level,
        type = conf.type,
        R = nboot,
        histogram = FALSE,
        digits = 5,
        reportIncomplete = TRUE
      ) %>%
      rcompanion_cleaner(.)

    # subtitle parameters
    no.parameters <- 0L
    statistic.text <- quote("log"["e"](italic("S")))
    effsize.text <- quote(widehat(italic(rho))["Spearman"])
  }

  #------------------------ subtitle text elements -----------------------------

  # preparing other needed objects
  if (stats_type == "parametric") {
    # subtitle parameters
    no.parameters <- 1L
    statistic.text <- quote(italic("t"))
    effsize.text <- quote(widehat(italic("r"))["Pearson"])
  }

  if (stats_type == "robust") {
    # subtitle parameters
    no.parameters <- 1L
    statistic.text <- quote(italic("t"))
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
        stat.title = stat.title,
        statistic.text = statistic.text,
        effsize.text = effsize.text,
        n = sample_size,
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
      )
  }

  # return the subtitle
  return(subtitle)
}
