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
#'   parameters (default: `0`).
#' @param n An integer specifying the sample size used for the test.
#' @param n.text A character that specifies the design, which will determine
#'   what the `n` stands for. It defaults to `quote(italic("n")["pairs"])` if
#'   `paired = TRUE`, and to `quote(italic("n")["obs"])` if `paired = FALSE`. If
#'   you wish to customize this further, you will need to provide object of
#'   `language` type.
#' @param prior.distribution A character that specifies the prior type.
#' @param effsize.text A character that specifies the relevant effect size.
#' @param bayesian Is this Bayesian analysis? Defaults to `FALSE`. The template
#'   is slightly different for Bayesian analysis.
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
#'   statistic = 5.494,
#'   df = 29.234,
#'   p.value = 0.00001,
#'   estimate = -1.980,
#'   conf.level = 0.95,
#'   conf.low = -2.873,
#'   conf.high = -1.088
#' )
#'
#' # expression for *t*-statistic with Cohen's *d* as effect size
#' # note that the plotmath expressions need to be quoted
#' expr_template(
#'   data = stats_df,
#'   statistic.text = quote(italic("t")),
#'   effsize.text = quote(italic("d")),
#'   n = 32L,
#'   n.text = quote(italic("n")["no.obs"]),
#'   k = 3L,
#'   k.df = 3L
#' )
#' @export

# function body
expr_template <- function(data,
                          paired = FALSE,
                          bayesian = FALSE,
                          statistic.text = NULL,
                          effsize.text = NULL,
                          top.text = NULL,
                          prior.distribution = NULL,
                          prior.type = NULL,
                          n = NULL,
                          n.text = ifelse(
                            paired,
                            list(quote(italic("n")["pairs"])),
                            list(quote(italic("n")["obs"]))
                          )[[1]],
                          conf.method = "HDI",
                          k = 2L,
                          k.df = 0L,
                          k.df.error = 0L,
                          ...) {

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

  # Bayesian analysis ------------------------------

  if (bayesian) {
    # Bayesian expression
    expression <- substitute(
      atop(
        displaystyle(top.text),
        expr = paste(
          "log"["e"] * "(BF"["01"] * ") = " * bf * ", ",
          widehat(effsize.text)[prior.type]^"posterior" * " = " * estimate * ", ",
          "CI"[conf.level]^conf.method * " [" * conf.low * ", " * conf.high * "], ",
          prior.distribution * " = " * bf.prior
        )
      ),
      env = list(
        top.text           = top.text,
        effsize.text       = effsize.text %||% estimate_type_switch(data$effectsize[[1]]),
        prior.type         = prior.type %||% prior_type_switch(data$method[[1]]),
        conf.level         = paste0(data$conf.level[[1]] * 100, "%"),
        conf.method        = toupper(conf.method),
        bf                 = format_value(-log(data$bf10[[1]]), k),
        estimate           = format_value(estimate, k),
        conf.low           = format_value(conf.low, k),
        conf.high          = format_value(conf.high, k),
        prior.distribution = prior.distribution %||% prior_switch(data$method[[1]]),
        bf.prior           = format_value(data$prior.scale[[1]], k)
      )
    )

    # return the final expression
    if (is.null(top.text)) expression <- expression$expr
  }

  # 0 degrees of freedom --------------------

  no.parameters <- sum("df.error" %in% names(data) + "df" %in% names(data))

  if (!bayesian && no.parameters == 0L) {
    # preparing expression
    expression <- substitute(
      expr = paste(
        statistic.text, " = ", statistic, ", ",
        italic("p"), " = ", p.value, ", ",
        effsize.text, " = ", estimate, ", CI"[conf.level], " [", conf.low, ", ", conf.high, "], ",
        n.text, " = ", n
      ),
      env = list(
        statistic.text = statistic.text %||% stat_text_switch(data$method[[1]]),
        statistic      = format_num(data$statistic[[1]], k),
        p.value        = format_num(data$p.value[[1]], k, p.value = TRUE),
        effsize.text   = effsize.text %||% estimate_type_switch(data$effectsize[[1]]),
        estimate       = format_value(estimate, k),
        conf.level     = paste0(data$conf.level[[1]] * 100, "%"),
        conf.low       = format_value(conf.low, k),
        conf.high      = format_value(conf.high, k),
        n              = .prettyNum(n),
        n.text         = n.text
      )
    )
  }

  # 1 degree of freedom --------------------

  if (!bayesian && no.parameters == 1L) {
    # for chi-squared statistic
    if ("df" %in% names(data)) data %<>% mutate(df.error = df)

    # preparing expression
    expression <- substitute(
      expr = paste(
        statistic.text, "(", parameter, ") = ", statistic, ", ",
        italic("p"), " = ", p.value, ", ",
        effsize.text, " = ", estimate, ", CI"[conf.level], " [", conf.low, ", ", conf.high, "], ",
        n.text, " = ", n
      ),
      env = list(
        statistic.text = statistic.text %||% stat_text_switch(data$method[[1]]),
        statistic      = format_num(data$statistic[[1]], k),
        parameter      = format_value(data$df.error[[1]], k.df),
        p.value        = format_num(data$p.value[[1]], k, p.value = TRUE),
        effsize.text   = effsize.text %||% estimate_type_switch(data$effectsize[[1]]),
        estimate       = format_value(estimate, k),
        conf.level     = paste0(data$conf.level[[1]] * 100, "%"),
        conf.low       = format_value(conf.low, k),
        conf.high      = format_value(conf.high, k),
        n              = .prettyNum(n),
        n.text         = n.text
      )
    )
  }

  # 2 degrees of freedom -----------------

  if (!bayesian && no.parameters == 2L) {
    # preparing expression
    expression <- substitute(
      expr = paste(
        statistic.text, "(", parameter1, ",", parameter2, ") = ", statistic, ", ",
        italic("p"), " = ", p.value, ", ",
        effsize.text, " = ", estimate, ", CI"[conf.level], " [", conf.low, ", ", conf.high, "], ",
        n.text, " = ", n
      ),
      env = list(
        statistic.text = statistic.text %||% stat_text_switch(data$method[[1]]),
        statistic      = format_num(data$statistic[[1]], k),
        parameter1     = format_value(data$df[[1]], k.df),
        parameter2     = format_value(data$df.error[[1]], k.df.error),
        p.value        = format_num(data$p.value[[1]], k, p.value = TRUE),
        effsize.text   = effsize.text %||% estimate_type_switch(data$effectsize[[1]]),
        estimate       = format_value(estimate, k),
        conf.level     = paste0(data$conf.level[[1]] * 100, "%"),
        conf.low       = format_value(conf.low, k),
        conf.high      = format_value(conf.high, k),
        n              = .prettyNum(n),
        n.text         = n.text
      )
    )
  }

  # return the formatted expression
  expression
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
  )[[1]]
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
  )[[1]]
}

#' @noRd

prior_switch <- function(x) {
  case_when(
    grepl("contingency", x, TRUE) ~ quote(italic("a")["Gunel-Dickey"]),
    grepl("correlation", x, TRUE) ~ quote(italic("r")["beta"]^"JZS"),
    TRUE ~ quote(italic("r")["Cauchy"]^"JZS")
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
  )[[1]]
}

#' @noRd

.prettyNum <- function(x) prettyNum(x, big.mark = ",", scientific = FALSE)
