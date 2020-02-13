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
#' @inheritParams robcor_ci
#' @inheritParams bf_corr_test
#' @inheritParams expr_anova_parametric
#'
#' @importFrom dplyr select
#' @importFrom rlang !! enquo enexpr ensym enexpr
#' @importFrom stats cor.test
#' @importFrom rcompanion spearmanRho
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
                           type = "pearson",
                           bf.prior = 0.707,
                           conf.level = 0.95,
                           conf.type = "norm",
                           k = 2,
                           stat.title = NULL,
                           messages = TRUE,
                           ...) {

  # make sure both quoted and unquoted arguments are supported
  c(x, y) %<-% c(rlang::ensym(x), rlang::ensym(y))

  #------------------------ dataframe -------------------------------------

  # if dataframe is provided
  data %<>%
    dplyr::select(.data = ., {{ x }}, {{ y }}) %>%
    tidyr::drop_na(.) %>%
    tibble::as_tibble(x = .)

  # the total sample size for analysis
  sample_size <- nrow(data)

  # standardize the type of statistics
  stats.type <- stats_type_switch(type)

  #------------------------ Pearson's r -------------------------------------

  if (stats.type %in% c("parametric", "nonparametric")) {
    # choosing appropriate method
    if (stats.type == "parametric") cor.method <- "pearson"
    if (stats.type == "nonparametric") cor.method <- "spearman"

    # tidy dataframe with statistical details
    stats_df <-
      broomExtra::tidy(
        x = stats::cor.test(
          formula = rlang::new_formula(
            NULL, rlang::expr(!!rlang::enexpr(x) + !!rlang::enexpr(y))
          ),
          data = data,
          method = cor.method,
          alternative = "two.sided",
          exact = FALSE,
          conf.level = conf.level,
          na.action = na.omit
        )
      )
  }

  # preparing other needed objects
  if (stats.type == "parametric") {
    # stats object already contains effect size info
    effsize_df <- stats_df

    # subtitle parameters
    no.parameters <- 1L
    statistic.text <- quote(italic("t"))
    effsize.text <- quote(widehat(italic("r"))["Pearson"])
  }

  #--------------------- Spearnman's rho ---------------------------------

  if (stats.type == "nonparametric") {
    # tidy dataframe with statistical details
    stats_df %<>% dplyr::mutate(.data = ., statistic = log(statistic))

    # getting confidence interval for rho using broom bootstrap
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

  #---------------------- robust percentage bend --------------------------

  if (stats.type == "robust") {
    # running robust correlation
    stats_df <-
      robcor_ci(
        data = data,
        x = {{ x }},
        y = {{ y }},
        beta = beta,
        nboot = nboot,
        conf.level = conf.level,
        conf.type = conf.type
      ) %>%
      dplyr::mutate(.data = ., parameter = sample_size - 2L)

    # stats object already contains effect size info
    effsize_df <- stats_df

    # subtitle parameters
    no.parameters <- 1L
    statistic.text <- quote(italic("t"))
    effsize.text <- quote(widehat(italic(rho))["pb"])

    # message about effect size measure
    if (isTRUE(messages)) effsize_ci_message(nboot, conf.level)
  }

  #---------------------- preparing subtitle ---------------------------------

  if (stats.type %in% c("parametric", "nonparametric", "robust")) {
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
  }

  #---------------------- bayes factor -----------------------------------

  if (stats.type == "bayes") {
    # bayes factor results
    subtitle <-
      bf_corr_test(
        data = data,
        x = {{ x }},
        y = {{ y }},
        bf.prior = bf.prior,
        caption = NULL,
        output = "h1",
        k = k
      )
  }

  # return the subtitle
  return(subtitle)
}


#' @name corr_objects
#' @title Create all needed objects for correlation matrix.
#' @description This function is mostly useful in the context of
#'   `ggstatsplot::ggcorrmat`. It returns the correlation matrix, *p*-value
#'   matrix, and a correlation object from `psych`/`WRS2` package that contains
#'   all the details about the test.
#'
#' @note This is **not** a stable function and would likely to either change or
#'   completely go away in future. So avoid using it in your workflows.
#'
#' @return A list with all needed objects for displaying correlation tests in a
#'   correlation matrix visualization.
#'
#' @param ... Currently ignored.
#' @param data Dataframe from which variables specified are preferentially to be
#'   taken. Only numeric variables should be present.
#' @param corr.method A character string indicating which correlation
#'   coefficient is to be computed (`"pearson"` (default) or `"kendall"` or
#'   `"spearman"`). `"robust"` can also be entered but only if `output` argument
#'   is set to either `"correlations"` or `"p-values"`. The robust correlation
#'   used is percentage bend correlation (see `?WRS2::pball`). Abbreviations
#'   will also work: `"p"` (for parametric/Pearson's *r*), `"np"`
#'   (nonparametric/Spearman's *rho*), `"r"` (robust).
#' @param p.adjust.method What adjustment for multiple tests should be used?
#'   (`"holm"`, `"hochberg"`, `"hommel"`, `"bonferroni"`, `"BH"`, `"BY"`,
#'   `"fdr"`, `"none"`). See `stats::p.adjust` for details about why to use
#'   `"holm"` rather than `"bonferroni"`). Default is `"none"`. If adjusted
#'   *p*-values are displayed in the visualization of correlation matrix, the
#'   **adjusted** *p*-values will be used for the **upper** triangle, while
#'   **unadjusted** *p*-values will be used for the **lower** triangle of the
#'   matrix.
#' @param beta A numeric bending constant for percentage bend robust correlation
#'   coefficient (Default: `0.1`).
#' @param k Decides the number of decimal digits to be displayed
#'   (Default: `2`).
#' @inheritParams psych::corr.test
#'
#' @examples
#' # only numeric variables
#' df <- purrr::keep(WRS2::diet, purrr::is_bare_numeric)
#'
#' # using function
#' corr_objects(df)
#' @export

corr_objects <- function(data,
                         ci = FALSE,
                         corr.method = "pearson",
                         p.adjust.method = "none",
                         beta = 0.1,
                         k = 2,
                         ...) {
  if (corr.method %in% c("pearson", "spearman")) {
    # computing correlations using `psych` package
    psych_corr_obj <-
      psych::corr.test(
        x = as.data.frame(data),
        use = "pairwise",
        method = corr.method,
        adjust = p.adjust.method,
        alpha = 0.05,
        ci = ci,
        minlength = 20
      )

    # computing correlations on all included variables
    corr.mat <- round(x = psych_corr_obj$r, digits = k)

    # compute a correlation matrix of p-values
    p.mat <- psych_corr_obj$p
  }

  # robust correlation
  if (corr.method == "robust") {
    # get matrix of samples sizes to be used later in `corr.p` function (`n`)
    psych_corr_obj <-
      psych::corr.test(
        x = as.data.frame(data),
        use = "pairwise",
        adjust = "none",
        alpha = 0.05,
        ci = FALSE,
        minlength = 20
      )

    # computing the percentage bend correlation matrix
    rob_cor <- WRS2::pball(x = data, beta = beta)

    # extracting the correlations and formatting them
    corr.mat <- round(x = rob_cor$pbcorm, digits = k)

    # converting `NA`s to 0's
    rob_cor$p.values[is.na(rob_cor$p.values)] <- 0

    # adjusting for multiple comparisons (if needed)
    p.mat <-
      psych::corr.p(
        r = corr.mat,
        n = psych_corr_obj$n,
        adjust = p.adjust.method,
        alpha = 0.05,
        minlength = 20
      )$p
  }

  # return everything as a list
  list("corr.mat" = corr.mat, "p.mat" = p.mat, "psych_corr_obj" = psych_corr_obj)
}
