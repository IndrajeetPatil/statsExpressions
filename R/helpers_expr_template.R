#' @title Template for subtitles with statistical details for tests
#' @name expr_template
#'
#' @param no.parameters An integer that specifies that the number of parameters
#'   for the statistical test. Can be `0` for non-parametric tests, `1` for
#'   tests based on *t*-statistic or chi-squared statistic, `2` for tests based
#'   on *F*-statistic.
#' @param stats.df A dataframe containing details from the statistical analysis
#'   and should contain some of the the following columns:
#' \itemize{
#'   \item *statistic*: the numeric value of a statistic.
#'   \item *parameter*: the numeric value of a parameter being modeled (often
#' degrees of freedom for the test); note that if `no.parameters = 0L` (e.g.,
#' for non-parametric tests), this column will be irrelevant.
#'   \item *parameter1*, *parameter2* relevant only if the statistic in question
#' has two degrees of freedom (e.g., anova).
#'   \item *p.value* the two-sided *p*-value associated with the observed
#' statistic.
#'  \item *estimate*: estimated value of the effect size.
#'   \item *conf.low*:  lower bound for effect size estimate.
#'   \item *conf.high*: upper bound for effect size estimate.
#' }
#' @param statistic.text A character that specifies the relevant test statistic.
#'   For example, for tests with *t*-statistic, `statistic.text = "t"`. If you
#'   want to use plotmath, you will have to quote the argument (e.g.,
#'   `quote(italic("t"))`).
#' @param effsize.text A character that specifies the relevant effect size.
#'   For example, for Cohen's *d* statistic, `effsize.text = "d"`. If you
#'   want to use plotmath, you will have to quote the argument (e.g.,
#'   `quote(italic("d"))`).
#' @param conf.level Scalar between 0 and 1. If unspecified, the defaults return
#'   `95%` confidence/credible intervals (`0.95`).
#' @param k Number of digits after decimal point (should be an integer)
#'   (Default: `k = 2L`).
#' @param k.parameter,k.parameter2 Number of decimal places to display for the
#'   parameters (default: `0`).
#' @param n An integer specifying the sample size used for the test.
#' @param n.text A character that specifies the design, which will determine
#'   what the `n` stands for. If `NULL`, defaults to
#'   `quote(italic("n")["pairs"])` if `paired = TRUE`, and to
#'   `quote(italic("n")["obs"])` if `paired = FALSE`.
#' @param ... Currently ignored.
#' @inheritParams expr_oneway_anova
#' @inheritParams ipmisc::long_to_wide_converter
#'
#' @importFrom ipmisc format_num
#'
#' @examples
#' set.seed(123)
#'
#' # creating a dataframe with stats results
#' stats_df <-
#'   cbind.data.frame(
#'     statistic = 5.494,
#'     parameter = 29.234,
#'     p.value = 0.00001,
#'     estimate = -1.980,
#'     conf.low = -2.873,
#'     conf.high = -1.088
#'   )
#'
#' # subtitle for *t*-statistic with Cohen's *d* as effect size
#' statsExpressions::expr_template(
#'   no.parameters = 1L,
#'   stats.df = stats_df,
#'   statistic.text = quote(italic("t")),
#'   effsize.text = quote(italic("d")),
#'   n = 32L,
#'   conf.level = 0.95,
#'   k = 3L,
#'   k.parameter = 3L
#' )
#' @export

# function body
expr_template <- function(no.parameters,
                          stats.df,
                          statistic.text = NULL,
                          effsize.text = NULL,
                          n,
                          n.text = NULL,
                          paired = FALSE,
                          conf.level = 0.95,
                          k = 2L,
                          k.parameter = 0L,
                          k.parameter2 = 0L,
                          ...) {
  # rename effect size column
  if ("effsize" %in% names(stats.df)) stats.df %<>% dplyr::rename(estimate = effsize)

  # sample size info-related text
  if (isTRUE(paired) && is.null(n.text)) n.text <- quote(italic("n")["pairs"])
  if (isFALSE(paired) && is.null(n.text)) n.text <- quote(italic("n")["obs"])

  # if expression elements are NULL
  if (is.null(statistic.text)) statistic.text <- method_switch(stats.df$method[[1]])
  if (is.null(effsize.text)) effsize.text <- effectsize_switch(stats.df$effectsize[[1]])

  # ------------------ statistic with 0 degrees of freedom --------------------

  if (no.parameters == 0L) {
    # preparing subtitle
    subtitle <-
      substitute(
        expr = paste(
          statistic.text,
          " = ",
          statistic,
          ", ",
          italic("p"),
          " = ",
          p.value,
          ", ",
          effsize.text,
          " = ",
          effsize.estimate,
          ", CI"[conf.level],
          " [",
          effsize.LL,
          ", ",
          effsize.UL,
          "]",
          ", ",
          n.text,
          " = ",
          n
        ),
        env = list(
          statistic.text = statistic.text,
          statistic = format_num(stats.df$statistic[[1]], k),
          p.value = format_num(stats.df$p.value[[1]], k = k, p.value = TRUE),
          effsize.text = effsize.text,
          effsize.estimate = format_num(stats.df$estimate[[1]], k),
          conf.level = paste0(conf.level * 100, "%"),
          effsize.LL = format_num(stats.df$conf.low[[1]], k),
          effsize.UL = format_num(stats.df$conf.high[[1]], k),
          n = .prettyNum(n),
          n.text = n.text
        )
      )
  }

  # ------------------ statistic with 1 degree of freedom --------------------

  if (no.parameters == 1L) {
    if ("df" %in% names(stats.df)) stats.df %<>% dplyr::rename("parameter" = "df")
    if ("df.error" %in% names(stats.df)) stats.df %<>% dplyr::rename("parameter" = "df.error")

    # preparing subtitle
    subtitle <-
      substitute(
        expr = paste(
          statistic.text,
          "(",
          parameter,
          ") = ",
          statistic,
          ", ",
          italic("p"),
          " = ",
          p.value,
          ", ",
          effsize.text,
          " = ",
          effsize.estimate,
          ", CI"[conf.level],
          " [",
          effsize.LL,
          ", ",
          effsize.UL,
          "]",
          ", ",
          n.text,
          " = ",
          n
        ),
        env = list(
          statistic.text = statistic.text,
          statistic = format_num(stats.df$statistic[[1]], k),
          parameter = format_num(stats.df$parameter[[1]], k = k.parameter),
          p.value = format_num(stats.df$p.value[[1]], k = k, p.value = TRUE),
          effsize.text = effsize.text,
          effsize.estimate = format_num(stats.df$estimate[[1]], k),
          conf.level = paste0(conf.level * 100, "%"),
          effsize.LL = format_num(stats.df$conf.low[[1]], k),
          effsize.UL = format_num(stats.df$conf.high[[1]], k),
          n = .prettyNum(n),
          n.text = n.text
        )
      )
  }

  # ------------------ statistic with 2 degrees of freedom -----------------

  if (no.parameters == 2L) {
    # renaming pattern from `easystats`
    stats.df %<>%
      dplyr::rename_all(.funs = dplyr::recode, df = "parameter1", df.error = "parameter2")

    # preparing subtitle
    subtitle <-
      substitute(
        expr = paste(
          statistic.text,
          "(",
          parameter1,
          ",",
          parameter2,
          ") = ",
          statistic,
          ", ",
          italic("p"),
          " = ",
          p.value,
          ", ",
          effsize.text,
          " = ",
          effsize.estimate,
          ", CI"[conf.level],
          " [",
          effsize.LL,
          ", ",
          effsize.UL,
          "]",
          ", ",
          n.text,
          " = ",
          n
        ),
        env = list(
          statistic.text = statistic.text,
          statistic = format_num(stats.df$statistic[[1]], k),
          parameter1 = format_num(stats.df$parameter1[[1]], k = k.parameter),
          parameter2 = format_num(stats.df$parameter2[[1]], k = k.parameter2),
          p.value = format_num(stats.df$p.value[[1]], k = k, p.value = TRUE),
          effsize.text = effsize.text,
          effsize.estimate = format_num(stats.df$estimate[[1]], k),
          conf.level = paste0(conf.level * 100, "%"),
          effsize.LL = format_num(stats.df$conf.low[[1]], k),
          effsize.UL = format_num(stats.df$conf.high[[1]], k),
          n = .prettyNum(n),
          n.text = n.text
        )
      )
  }

  # return the formatted subtitle
  return(subtitle)
}

#' @noRd

method_switch <- function(method) {
  switch(
    method,
    "Pearson" = ,
    "Percentage Bend" = ,
    "One Sample t-test" = ,
    "Two Sample t-test" = ,
    "Paired t-test" = quote(italic("t")["Student"]),
    "Welch Two Sample t-test" = quote(italic("t")["Welch"]),
    "Wilcoxon rank sum test" = quote("log"["e"](italic("W")["Mann-Whitney"])),
    "Wilcoxon signed rank test" = quote("log"["e"](italic("V")["Wilcoxon"])),
    "Yuen's test on trimmed means for independent samples" = ,
    "Yuen's test on trimmed means for dependent samples" = quote(italic("t")["Yuen"]),
    "One-way analysis of means (not assuming equal variances)" = quote(italic("F")["Welch"]),
    "One-way analysis of means" = quote(italic("F")["Fisher"]),
    "Friedman rank sum test" = quote(chi["Friedman"]^2),
    "Kruskal-Wallis rank sum test" = quote(chi["Kruskal-Wallis"]^2),
    "A heteroscedastic one-way ANOVA for trimmed means" = quote(italic("F")["trimmed-means"]),
    "Spearman" = quote("log"["e"](italic("S"))),
    "Chi-squared test for given probabilities" = quote(chi["gof"]^2),
    "Pearson's Chi-squared test" = quote(chi["Pearson"]^2),
    "McNemar's Chi-squared test" = quote(chi["McNemar"]^2),
    NULL
  )
}

#' @noRd

effectsize_switch <- function(type) {
  switch(
    type,
    "Pearson" = quote(widehat(italic("r"))["Pearson"]),
    "Spearman" = quote(widehat(italic(rho))["Spearman"]),
    "Percentage Bend" = quote(widehat(italic(rho))["% bend"]),
    "Cohen's d" = quote(widehat(italic("d"))["Cohen"]),
    "Hedges' g" = quote(widehat(italic("g"))["Hedge"]),
    "r (rank biserial)" = quote(widehat(italic("r"))["biserial"]^"rank"),
    "Explanatory measure of effect size" = quote(widehat(italic(xi))),
    "AKP" = quote(widehat(italic(delta))["R"]),
    "Eta2" = ,
    "Eta2 (partial)" = quote(widehat(eta["p"]^2)),
    "Omega2" = ,
    "Omega2 (partial)" = quote(widehat(omega["p"]^2)),
    "Kendall's W" = quote(widehat(italic("W"))["Kendall"]),
    "Epsilon2 (rank)" = quote(widehat(epsilon)["ordinal"]^2),
    "Cramer's V (adj.)" = quote(widehat(italic("V"))["Cramer"]),
    "Cohen's g" = quote(widehat(italic("g"))["Cohen"])
  )
}

#' @noRd

.prettyNum <- function(x) prettyNum(x, big.mark = ",", scientific = FALSE)
