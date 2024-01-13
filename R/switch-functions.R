#' @name stats_type_switch
#' @title Switch the type of statistics.
#'
#' @description
#'
#' Relevant mostly for `{ggstatsplot}` and `{statsExpressions}` packages, where
#' different statistical approaches are supported via this argument: parametric,
#' non-parametric, robust, and Bayesian. This switch function converts strings
#' entered by users to a common pattern for convenience.
#'
#' @param type A character specifying the type of statistical approach:
#'   - `"parametric"`
#'   - `"nonparametric"`
#'   - `"robust"`
#'   - `"bayes"`
#'
#' You can specify just the initial letter.
#'
#' @autoglobal
#'
#' @examples
#' stats_type_switch("p")
#' stats_type_switch("bf")
#' @export
# styler: off
stats_type_switch <- function(type) {
  case_when(
    grepl("^n|^s", type) ~ "nonparametric", # s is for Spearman's rho
    grepl("^r", type)    ~ "robust",
    grepl("^b", type)    ~ "bayes",
    TRUE                 ~ "parametric"
  )
}


#' @noRd
estimate_type_switch <- function(x) {
  case_when(
    .grepl("pearson's c", x)      ~ list(quote(widehat(italic("C"))["Pearson"])),
    .grepl("cohen's d", x)        ~ list(quote(widehat(italic("d"))["Cohen"])),
    .grepl("hedges' g", x)        ~ list(quote(widehat(italic("g"))["Hedges"])),
    .grepl("biserial", x)         ~ list(quote(widehat(italic("r"))["biserial"]^"rank")),
    .grepl("cramer", x)           ~ list(quote(widehat(italic("V"))["Cramer"])),
    .grepl("cohen's g", x)        ~ list(quote(widehat(italic("g"))["Cohen"])),
    .grepl("r-squared", x)        ~ list(quote(widehat(italic(R^"2"))["Bayesian"])),
    .grepl("spearman", x)         ~ list(quote(widehat(rho)["Spearman"])),
    grepl("^bayesian pearson", x) ~ list(quote(widehat(rho)["Pearson"])),
    grepl("posterior|t-", x)      ~ list(quote(widehat(delta)["difference"])),
    grepl("difference$", x)       ~ list(quote(widehat(delta)["R"]^"AKP")),
    grepl("average$", x)          ~ list(quote(widehat(delta)["R-avg"]^"AKP")),
    grepl("^eta2", x)             ~ list(quote(widehat(eta["p"]^2))),
    grepl("^omega2", x)           ~ list(quote(widehat(omega["p"]^2))),
    grepl("^trimmed", x)          ~ list(quote(widehat(mu)["trimmed"])),
    grepl("^epsilon2", x)         ~ list(quote(widehat(epsilon)["ordinal"]^2)),
    grepl("^explanatory", x)      ~ list(quote(widehat(xi))),
    grepl("^meta", x)             ~ list(quote(widehat(beta)["summary"]^"meta")),
    grepl("^pearson", x)          ~ list(quote(widehat(italic("r"))["Pearson"])),
    grepl("^winsor", x)           ~ list(quote(widehat(italic("r"))["Winsorized"])),
    grepl("^kendall", x)          ~ list(quote(widehat(italic("W"))["Kendall"])),
    TRUE                          ~ list(NULL)
  )
}

#' @noRd
stat_text_switch <- function(x) {
  case_when(
    grepl("^welch", x)               ~ list(quote(italic("t")["Welch"])),
    grepl("afex| of means$", x)      ~ list(quote(italic("F")["Fisher"])),
    grepl("^boot", x)                ~ list(quote(italic("t")["bootstrapped"])),
    grepl("^spearman", x)            ~ list(quote(italic("S"))),
    grepl("t-test$|correlation$", x) ~ list(quote(italic("t")["Student"])),
    .grepl("wilcoxon rank", x)       ~ list(quote(italic("W")["Mann-Whitney"])),
    .grepl("wilcoxon signed", x)     ~ list(quote(italic("V")["Wilcoxon"])),
    .grepl("variances", x)           ~ list(quote(italic("F")["Welch"])),
    .grepl("yuen", x)                ~ list(quote(italic("t")["Yuen"])),
    .grepl("heteroscedastic", x)     ~ list(quote(italic("F")["trimmed-means"])),
    .grepl("meta", x)                ~ list(quote(italic("z"))),
    .grepl("friedman", x)            ~ list(quote(chi["Friedman"]^2)),
    .grepl("kruskal", x)             ~ list(quote(chi["Kruskal-Wallis"]^2)),
    .grepl("probabilities", x)       ~ list(quote(chi["gof"]^2)),
    .grepl("pearson's chi", x)       ~ list(quote(chi["Pearson"]^2)),
    .grepl("mcnemar's chi", x)       ~ list(quote(chi["McNemar"]^2)),
    TRUE                             ~ list(NULL)
  )
}

#' @noRd
prior_switch <- function(x) {
  case_when(
    .grepl("contingency", x) ~ list(quote(italic("a")["Gunel-Dickey"])),
    .grepl("correlation", x) ~ list(quote(italic("r")["beta"]^"JZS")),
    TRUE                     ~ list(quote(italic("r")["Cauchy"]^"JZS"))
  )
}

#' @note Don't curry using `purrr::partial()` because it causes `Found a .Internal call` warning.
#' @noRd
.grepl <- function(pattern, x) grepl(pattern, x, fixed = TRUE)

# styler: on
