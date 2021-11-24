#' @title Template for expressions with statistical details
#' @name expr_template
#'
#' @description
#'
#' Creates an expression from a dataframe containing statistical details.
#' Ideally, this dataframe would come from having run `tidy_model_parameters`
#' function on your model object.
#'
#' This function is currently **not** stable and should not be used outside of
#' this package context.
#'
#' @param data A dataframe containing details from the statistical analysis
#'   and should contain some or all of the the following columns:
#' - *statistic*: the numeric value of a statistic.
#' - *df.error*: the numeric value of a parameter being modeled (often degrees
#' of freedom for the test); note that if there are no degrees of freedom (e.g.,
#' for non-parametric tests), this column will be irrelevant.
#' - *df*: relevant only if the statistic in question has two degrees of freedom.
#' - *p.value*: the two-sided *p*-value associated with the observed statistic.
#' - *method*: method describing the test carried out.
#' - *effectsize*: name of the effect size (if not present, same as `method`).
#' - *estimate*: estimated value of the effect size.
#' - *conf.level*: width for the confidence intervals.
#' - *conf.low*: lower bound for effect size estimate.
#' - *conf.high*: upper bound for effect size estimate.
#' - *bf10*: Bayes Factor value (if `bayesian = TRUE`).
#' @param statistic.text A character that specifies the relevant test statistic.
#'   For example, for tests with *t*-statistic, `statistic.text = "t"`.
#' @param effsize.text A character that specifies the relevant effect size or
#'   posterior estimate.
#' @param k Number of digits after decimal point (should be an integer)
#'   (Default: `k = 2L`).
#' @param k.df,k.df.error Number of decimal places to display for the
#'   parameters (default: `0L`).
#' @param n An integer specifying the sample size used for the test.
#' @param n.text A character that specifies the design, which will determine
#'   what the `n` stands for. It defaults to `quote(italic("n")["pairs"])` if
#'   `paired = TRUE`, and to `quote(italic("n")["obs"])` if `paired = FALSE`. If
#'   you wish to customize this further, you will need to provide object of
#'   `language` type.
#' @param effsize.text A character that specifies the relevant effect size.
#' @param prior.type The type of prior.
#' @param conf.method The type of index used for Credible Interval. Can be
#'   `"hdi"` (default), `"eti"`, or `"si"` (see `si()`, `hdi()`, `eti()`
#'   functions from `bayestestR` package).
#' @param k Number of digits after decimal point (should be an integer)
#'   (Default: `k = 2L`).
#' @param top.text Text to display on top of the Bayes Factor message. This is
#'   mostly relevant in the context of `ggstatsplot` package functions.
#' @param ... Currently ignored.
#' @inheritParams oneway_anova
#' @inheritParams long_to_wide_converter
#'
#' @examples
#' set.seed(123)
#'
#' # creating a dataframe with stats results
#' stats_df <- cbind.data.frame(
#'   statistic  = 5.494,
#'   df         = 29.234,
#'   p.value    = 0.00001,
#'   estimate   = -1.980,
#'   conf.level = 0.95,
#'   conf.low   = -2.873,
#'   conf.high  = -1.088,
#'   method     = "Student's t-test"
#' )
#'
#' # expression for *t*-statistic with Cohen's *d* as effect size
#' # note that the plotmath expressions need to be quoted
#' expr_template(
#'   data           = stats_df,
#'   statistic.text = list(quote(italic("t"))),
#'   effsize.text   = list(quote(italic("d"))),
#'   n              = 32L,
#'   n.text         = list(quote(italic("n")["no.obs"])),
#'   k              = 3L,
#'   k.df           = 3L
#' )
#' @export

# function body
expr_template <- function(data,
                          paired = FALSE,
                          statistic.text = NULL,
                          effsize.text = NULL,
                          top.text = NULL,
                          prior.type = NULL,
                          n = NULL,
                          n.text = ifelse(
                            paired,
                            list(quote(italic("n")["pairs"])),
                            list(quote(italic("n")["obs"]))
                          ),
                          conf.method = "HDI",
                          k = 2L,
                          k.df = 0L,
                          k.df.error = k.df,
                          ...) {
  # is this Bayesian test?
  bayesian <- ifelse("bf10" %in% names(data), TRUE, FALSE)

  # special case for Bayesian analysis
  if (bayesian) {
    # if not present, create a new column for Bayesian analysis
    if (!"effectsize" %in% names(data)) data %<>% mutate(effectsize = method)

    # special handling of contingency tabs analysis
    if (grepl("contingency", data$method[[1]])) data %<>% mutate(effectsize = "Cramers_v")
  }

  # extracting estimate values
  if ("r2" %in% names(data)) {
    # for ANOVA designs
    c(estimate, conf.low, conf.high) %<-% c(data$r2[[1]], data$r2.conf.low[[1]], data$r2.conf.high[[1]])
  } else {
    # for non-ANOVA designs
    c(estimate, conf.low, conf.high) %<-% c(data$estimate[[1]], data$conf.low[[1]], data$conf.high[[1]])
  }

  # convert needed columns to character type
  df <- .data_to_char(data, k, k.df, k.df.error)

  # adding a few other columns
  df %<>% mutate(
    statistic.text = statistic.text %||% stat_text_switch(method),
    es.text = effsize.text %||% estimate_type_switch(effectsize),
    prior.type = prior.type %||% prior_type_switch(method),
    prior.distribution = prior_switch(method),
    conf.method = toupper(conf.method),
    n.obs = .prettyNum(n)
  )

  # Bayesian analysis ------------------------------

  if (bayesian) {
    if (is.null(top.text)) {
      df %<>% mutate(expression = glue("list(
            log[e]*(BF['01'])=='{format_value(-log(bf10), k)}',
            {es.text}[{prior.type}]^'posterior'=='{estimate}',
            CI['{conf.level}']^{conf.method}~'['*'{conf.low}', '{conf.high}'*']',
            {prior.distribution}=='{prior.scale}')"))
    } else {
      df %<>% mutate(expression = glue("list(
            atop(displaystyle({top.text}),
            list(log[e]*(BF['01'])=='{format_value(-log(bf10), k)}',
            {es.text}[{prior.type}]^'posterior'=='{estimate}',
            CI['{conf.level}']^{conf.method}~'['*'{conf.low}', '{conf.high}'*']',
            {prior.distribution}=='{prior.scale}')))"))
    }
  }

  # how many parameters?
  no.parameters <- sum("df.error" %in% names(data) + "df" %in% names(data))

  # 0 degrees of freedom --------------------

  if (!bayesian && no.parameters == 0L) {
    df %<>% mutate(expression = glue("list(
            {statistic.text}=='{statistic}', italic(p)=='{p.value}',
            {es.text}=='{estimate}', CI['{conf.level}']~'['*'{conf.low}', '{conf.high}'*']',
            {n.text}=='{n.obs}')"))
  }

  # 1 degree of freedom --------------------

  if (!bayesian && no.parameters == 1L) {
    # for chi-squared statistic
    if ("df" %in% names(df)) df %<>% mutate(df.error = df)

    df %<>% mutate(expression = glue("list(
            {statistic.text}*'('*{df.error}*')'=='{statistic}', italic(p)=='{p.value}',
            {es.text}=='{estimate}', CI['{conf.level}']~'['*'{conf.low}', '{conf.high}'*']',
            {n.text}=='{n.obs}')"))
  }

  # 2 degrees of freedom -----------------

  if (!bayesian && no.parameters == 2L) {
    df %<>% mutate(expression = glue("list(
            {statistic.text}({df}, {df.error})=='{statistic}', italic(p)=='{p.value}',
            {es.text}=='{estimate}', CI['{conf.level}']~'['*'{conf.low}', '{conf.high}'*']',
            {n.text}=='{n.obs}')"))
  }

  # return the formatted expression
  parse(text = df$expression[[1]])
}


#' @noRd

stat_text_switch <- function(x) {
  # to make life easier
  x <- tolower(x)

  # need to list because `case_when` can't handle outputs of different lengths
  case_when(
    grepl("^one sample|^two sample|^pair|pearson correlation$", x) ~ list(quote(italic("t")["Student"])),
    grepl("^boot", x) ~ list(quote(italic("t")["bootstrapped"])),
    grepl("^welch", x) ~ list(quote(italic("t")["Welch"])),
    grepl("wilcoxon rank", x) ~ list(quote(italic("W")["Mann-Whitney"])),
    grepl("wilcoxon signed", x) ~ list(quote(italic("V")["Wilcoxon"])),
    grepl("afex| of means$", x) ~ list(quote(italic("F")["Fisher"])),
    grepl("variances", x) ~ list(quote(italic("F")["Welch"])),
    grepl("friedman", x) ~ list(quote(chi["Friedman"]^2)),
    grepl("kruskal", x) ~ list(quote(chi["Kruskal-Wallis"]^2)),
    grepl("spearman", x) ~ list(quote(italic("S"))),
    grepl("yuen", x) ~ list(quote(italic("t")["Yuen"])),
    grepl("heteroscedastic", x) ~ list(quote(italic("F")["trimmed-means"])),
    grepl("probabilities", x) ~ list(quote(chi["gof"]^2)),
    grepl("pearson's chi", x) ~ list(quote(chi["Pearson"]^2)),
    grepl("mcnemar's chi", x) ~ list(quote(chi["McNemar"]^2)),
    grepl("meta", x) ~ list(quote(italic("z"))),
    TRUE ~ list(NULL)
  )
}

#' @noRd

estimate_type_switch <- function(x) {
  # to make life easier
  x <- tolower(x)

  # need to list because `case_when` can't handle outputs of different lengths
  case_when(
    grepl("pearson's c", x) ~ list(quote(widehat(italic("C"))["Pearson"])),
    grepl("^pearson", x) ~ list(quote(widehat(italic("r"))["Pearson"])),
    grepl("spearman", x) ~ list(quote(widehat(rho)["Spearman"])),
    grepl("^winsor", x) ~ list(quote(widehat(italic("r"))["Winsorized"])),
    grepl("cohen's d", x) ~ list(quote(widehat(italic("d"))["Cohen"])),
    grepl("hedges' g", x) ~ list(quote(widehat(italic("g"))["Hedges"])),
    grepl("^eta2", x) ~ list(quote(widehat(eta["p"]^2))),
    grepl("^omega2", x) ~ list(quote(widehat(omega["p"]^2))),
    grepl("biserial", x) ~ list(quote(widehat(italic("r"))["biserial"]^"rank")),
    grepl("^trimmed", x) ~ list(quote(widehat(mu)["trimmed"])),
    grepl("^kendall", x) ~ list(quote(widehat(italic("W"))["Kendall"])),
    grepl("^epsilon2", x) ~ list(quote(widehat(epsilon)["ordinal"]^2)),
    grepl("^cramers_v$", x) ~ list(quote(italic("V"))),
    grepl("cramer", x) ~ list(quote(widehat(italic("V"))["Cramer"])),
    grepl("cohen's g", x) ~ list(quote(widehat(italic("g"))["Cohen"])),
    grepl("^explanatory", x) ~ list(quote(widehat(xi))),
    grepl("difference$", x) ~ list(quote(widehat(delta)["R"]^"AKP")),
    grepl("average$", x) ~ list(quote(widehat(delta)["R-avg"]^"AKP")),
    grepl("^bayesian pearson", x) ~ list(quote(rho)),
    grepl("posterior|t-", x) ~ list(quote(italic(delta))),
    grepl("linear", x) ~ list(quote(italic(R^"2"))),
    grepl("^meta", x) ~ list(quote(widehat(beta)["summary"]^"meta")),
    TRUE ~ list(NULL)
  )
}

#' @noRd

prior_switch <- function(x) {
  case_when(
    grepl("contingency", x, TRUE) ~ list(quote(italic("a")["Gunel-Dickey"])),
    grepl("correlation", x, TRUE) ~ list(quote(italic("r")["beta"]^"JZS")),
    TRUE ~ list(quote(italic("r")["Cauchy"]^"JZS"))
  )
}

#' @noRd

prior_type_switch <- function(x) {
  case_when(
    grepl("contingency", x, TRUE) ~ list("Cramer"),
    grepl("correlation", x, TRUE) ~ list("Pearson"),
    grepl("t-|meta-", x, TRUE) ~ list("difference"),
    grepl("linear", x, TRUE) ~ list("Bayesian"),
    TRUE ~ list(NULL)
  )
}

#' @noRd

.prettyNum <- function(x) prettyNum(x, big.mark = ",", scientific = FALSE)
