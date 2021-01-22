#' @title Template for expressions with statistical details
#' @name expr_template
#'
#' @param no.parameters An integer that specifies that the number of parameters
#'   for the statistical test. Can be `0` for non-parametric tests, `1` for
#'   tests based on *t*-statistic or chi-squared statistic, `2` for tests based
#'   on *F*-statistic.
#' @param stats.df A dataframe containing details from the statistical analysis
#'   and should contain some or all of the the following columns:
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
#'   \item *conf.low*: lower bound for effect size estimate.
#'   \item *conf.high*: upper bound for effect size estimate.
#'   \item *`bf10`* Bayes Factor value (if `bayesian = TRUE`).
#'   \item *method*: method describing the test carried out.
#' }
#' @param statistic.text A character that specifies the relevant test statistic.
#'   For example, for tests with *t*-statistic, `statistic.text = "t"`.
#' @param effsize.text A character that specifies the relevant effect size or
#'   posterior estimate.
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
#' @param prior.text A character that specifies the prior type.
#' @param effsize.text A character that specifies the relevant effect size.
#' @param bayesian Is this Bayesian analysis? Defaults to `FALSE`. The template
#'   is slightly different for Bayesian analysis.
#' @param ... Currently ignored.
#' @inheritParams bf_extractor
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
#' # expression for *t*-statistic with Cohen's *d* as effect size
#' # note that the plotmath expressions need to be quoted
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
expr_template <- function(stats.df,
                          no.parameters = 0L,
                          bayesian = FALSE,
                          statistic.text = NULL,
                          effsize.text = NULL,
                          top.text = NULL,
                          prior.text = NULL,
                          n = NULL,
                          n.text = NULL,
                          paired = FALSE,
                          conf.level = NULL,
                          centrality = "median",
                          conf.method = "HDI",
                          k = 2L,
                          k.parameter = 0L,
                          k.parameter2 = 0L,
                          ...) {

  # extracting estimate values
  if ("r2" %in% names(stats.df)) {
    # for ANOVA designs
    c(estimate, estimate.LB, estimate.UB) %<-%
      c(stats.df$r2[[1]], stats.df$r2.conf.low[[1]], stats.df$r2.conf.high[[1]])
  } else {
    # for non-ANOVA designs
    c(estimate, estimate.LB, estimate.UB) %<-%
      c(stats.df$estimate[[1]], stats.df$conf.low[[1]], stats.df$conf.high[[1]])
  }

  # if not present, create a new column for Bayesian analysis
  if (!"effectsize" %in% names(stats.df) && bayesian) stats.df %<>% dplyr::mutate(effectsize = method)

  # if expression text elements are `NULL`
  if (isTRUE(paired) && is.null(n.text)) n.text <- quote(italic("n")["pairs"])
  if (isFALSE(paired) && is.null(n.text)) n.text <- quote(italic("n")["obs"])
  if (is.null(statistic.text)) statistic.text <- stat_text_switch(stats.df$method[[1]])
  if (is.null(effsize.text)) effsize.text <- estimate_type_switch(stats.df$effectsize[[1]])
  if (is.null(prior.text) && bayesian) prior.text <- prior_type_switch(stats.df$method[[1]])
  if (is.null(conf.level)) conf.level <- stats.df$ci.width[[1]]

  if (conf.level > 1) conf.level <- conf.level / 100

  # -------------------------- Bayesian analysis ------------------------------

  if (isTRUE(bayesian)) {
    expression <-
      substitute(
        atop(
          displaystyle(top.text),
          expr = paste(
            "log"["e"] * "(BF"["01"] * ") = " * bf * ", ",
            widehat(effsize.text)[centrality]^"posterior" * " = " * estimate * ", ",
            "CI"[conf.level]^conf.method * " [" * estimate.LB * ", " * estimate.UB * "], ",
            prior.text * " = " * bf.prior
          )
        ),
        env = list(
          top.text = top.text,
          effsize.text = effsize.text,
          centrality = centrality,
          conf.level = paste0(conf.level * 100, "%"),
          conf.method = toupper(conf.method),
          bf = format_num(-log(stats.df$bf10[[1]]), k = k),
          estimate = format_num(estimate, k = k),
          estimate.LB = format_num(estimate.LB, k = k),
          estimate.UB = format_num(estimate.UB, k = k),
          prior.text = prior.text,
          bf.prior = format_num(stats.df$prior.scale[[1]], k = k)
        )
      )

    # return the final expression
    if (is.null(top.text)) expression <- expression$expr
  }

  # ------------------ statistic with 0 degrees of freedom --------------------

  if (isFALSE(bayesian) && no.parameters == 0L) {
    # preparing expression
    expression <-
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
          estimate,
          ", CI"[conf.level],
          " [",
          estimate.LB,
          ", ",
          estimate.UB,
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
          estimate = format_num(estimate, k),
          conf.level = paste0(conf.level * 100, "%"),
          estimate.LB = format_num(estimate.LB, k),
          estimate.UB = format_num(estimate.UB, k),
          n = .prettyNum(n),
          n.text = n.text
        )
      )
  }

  # ------------------ statistic with 1 degree of freedom --------------------

  if (isFALSE(bayesian) && no.parameters == 1L) {
    if ("df" %in% names(stats.df)) stats.df %<>% dplyr::rename("parameter" = "df")
    if ("df.error" %in% names(stats.df)) stats.df %<>% dplyr::rename("parameter" = "df.error")

    # preparing expression
    expression <-
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
          estimate,
          ", CI"[conf.level],
          " [",
          estimate.LB,
          ", ",
          estimate.UB,
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
          estimate = format_num(estimate, k),
          conf.level = paste0(conf.level * 100, "%"),
          estimate.LB = format_num(estimate.LB, k),
          estimate.UB = format_num(estimate.UB, k),
          n = .prettyNum(n),
          n.text = n.text
        )
      )
  }

  # ------------------ statistic with 2 degrees of freedom -----------------

  if (isFALSE(bayesian) && no.parameters == 2L) {
    # renaming pattern from `easystats`
    stats.df %<>% dplyr::rename_all(.funs = dplyr::recode, df = "parameter1", df.error = "parameter2")

    # preparing expression
    expression <-
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
          estimate,
          ", CI"[conf.level],
          " [",
          estimate.LB,
          ", ",
          estimate.UB,
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
          estimate = format_num(estimate, k),
          conf.level = paste0(conf.level * 100, "%"),
          estimate.LB = format_num(estimate.LB, k),
          estimate.UB = format_num(estimate.UB, k),
          n = .prettyNum(n),
          n.text = n.text
        )
      )
  }

  # return the formatted expression
  expression
}

#' @noRd

stat_text_switch <- function(method) {
  switch(
    method,
    "Pearson" = ,
    "Percentage Bend" = ,
    "One Sample t-test" = ,
    "Two Sample t-test" = ,
    "Paired t-test" = quote(italic("t")["Student"]),
    "Bootstrap-t method for one-sample test" = quote(italic("t")["bootstrapped"]),
    "Welch Two Sample t-test" = quote(italic("t")["Welch"]),
    "Wilcoxon rank sum test" = quote("log"["e"](italic("W")["Mann-Whitney"])),
    "Wilcoxon signed rank test" = quote("log"["e"](italic("V")["Wilcoxon"])),
    "Yuen's test on trimmed means for independent samples" = ,
    "Yuen's test on trimmed means for dependent samples" = quote(italic("t")["Yuen"]),
    "One-way analysis of means (not assuming equal variances)" = quote(italic("F")["Welch"]),
    "One-way analysis of means" = ,
    "ANOVA estimation for factorial designs using 'afex'" = quote(italic("F")["Fisher"]),
    "Friedman rank sum test" = quote(chi["Friedman"]^2),
    "Kruskal-Wallis rank sum test" = quote(chi["Kruskal-Wallis"]^2),
    "A heteroscedastic one-way repeated measures ANOVA for trimmed means" = ,
    "A heteroscedastic one-way ANOVA for trimmed means" = quote(italic("F")["trimmed-means"]),
    "Spearman" = quote("log"["e"](italic("S"))),
    "Chi-squared test for given probabilities" = quote(chi["gof"]^2),
    "Pearson's Chi-squared test" = quote(chi["Pearson"]^2),
    "McNemar's Chi-squared test" = quote(chi["McNemar"]^2),
    "Meta-analysis using 'metafor'" = ,
    "Robust meta-analysis using 'metaplus'" = quote(italic("z")),
    NULL
  )
}

#' @noRd

estimate_type_switch <- function(method) {
  switch(
    method,
    "Pearson" = quote(widehat(italic("r"))["Pearson"]),
    "Spearman" = quote(widehat(rho)["Spearman"]),
    "Percentage Bend" = quote(widehat(rho)["% bend"]),
    "Cohen's d" = quote(widehat(italic("d"))["Cohen"]),
    "Hedges' g" = quote(widehat(italic("g"))["Hedge"]),
    "r (rank biserial)" = quote(widehat(italic("r"))["biserial"]^"rank"),
    "Explanatory measure of effect size" = quote(widehat(xi)),
    "Algina-Keselman-Penfield robust standardized difference" = quote(widehat(delta)["R"]^"AKP"),
    "Algina-Keselman-Penfield robust standardized difference average" = quote(widehat(delta)["R-avg"]^"AKP"),
    "Trimmed mean" = quote(widehat(mu)["trimmed"]),
    "Eta2" = ,
    "Eta2 (partial)" = quote(widehat(eta["p"]^2)),
    "Omega2" = ,
    "Omega2 (partial)" = quote(widehat(omega["p"]^2)),
    "Kendall's W" = quote(widehat(italic("W"))["Kendall"]),
    "Epsilon2 (rank)" = quote(widehat(epsilon)["ordinal"]^2),
    "Cramer's V (adj.)" = quote(widehat(italic("V"))["Cramer"]),
    "Cohen's g" = quote(widehat(italic("g"))["Cohen"]),
    "meta-analytic summary estimate" = quote(widehat(beta)["summary"]^"meta"),
    "Bayesian contingency tabs analysis" = quote(italic("V")),
    "Bayesian Pearson" = quote(rho),
    "Bayesian meta-analysis using 'metaBMA'" = ,
    "Bayesian t-test" = quote(italic(delta)),
    "Bayes factors for linear models" = quote(italic(R^"2")),
    NULL
  )
}

#' @noRd

prior_type_switch <- function(method) {
  switch(
    method,
    "Bayesian contingency tabs analysis" = quote(italic("a")["Gunel-Dickey"]),
    quote(italic("r")["Cauchy"]^"JZS")
  )
}

#' @noRd

.prettyNum <- function(x) prettyNum(x, big.mark = ",", scientific = FALSE)
