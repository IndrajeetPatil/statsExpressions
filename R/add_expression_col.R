#' @title Template for expressions with statistical details
#' @name add_expression_col
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
#' @param k Number of digits after decimal point (should be an integer)
#'   (Default: `k = 2L`).
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
#' add_expression_col(
#'   data           = stats_df,
#'   statistic.text = list(quote(italic("t"))),
#'   effsize.text   = list(quote(italic("d"))),
#'   n              = 32L,
#'   n.text         = list(quote(italic("n")["no.obs"])),
#'   k              = 3L,
#'   k.df           = 3L
#' )
#' @export
add_expression_col <- function(data,
                               paired = FALSE,
                               statistic.text = NULL,
                               effsize.text = NULL,
                               prior.type = NULL,
                               n = NULL,
                               n.text = ifelse(
                                 paired,
                                 list(quote(italic("n")["pairs"])),
                                 list(quote(italic("n")["obs"]))
                               ),
                               k = 2L,
                               k.df = 0L,
                               k.df.error = k.df,
                               ...) {
  if (!"n.obs" %in% colnames(data)) data %<>% mutate(n.obs = n)
  if (!"effectsize" %in% colnames(data)) data %<>% mutate(effectsize = method)
  data %<>% rename_all(.funs = recode, "bayes.factor" = "bf10")

  bayesian <- any("bf10" == colnames(data))
  no.parameters <- sum("df.error" %in% names(data) + "df" %in% names(data))

  # special case for Bayesian contingency table analysis
  if (bayesian && grepl("contingency", data$method[[1]], fixed = TRUE)) data %<>% mutate(effectsize = "Cramers_v")

  # convert needed columns to character type
  df_expr <- .data_to_char(data, k, k.df, k.df.error)

  df_expr %<>% mutate(
    statistic.text     = statistic.text %||% stat_text_switch(method),
    es.text            = effsize.text %||% estimate_type_switch(effectsize),
    prior.distribution = prior_switch(method),
    n.obs              = .prettyNum(n.obs)
  )

  # Bayesian analysis ------------------------------

  if (bayesian) {
    df_expr %<>% mutate(expression = glue("list(
            log[e]*(BF['01'])=='{format_value(-log(bf10), k)}',
            {es.text}^'posterior'=='{estimate}',
            CI['{conf.level}']^{conf.method}~'['*'{conf.low}', '{conf.high}'*']',
            {prior.distribution}=='{prior.scale}')"))
  }

  # 0 degrees of freedom --------------------

  if (!bayesian && no.parameters == 0L) {
    df_expr %<>% mutate(expression = glue("list(
            {statistic.text}=='{statistic}', italic(p)=='{p.value}',
            {es.text}=='{estimate}', CI['{conf.level}']~'['*'{conf.low}', '{conf.high}'*']',
            {n.text}=='{n.obs}')"))
  }

  # 1 degree of freedom --------------------

  if (!bayesian && no.parameters == 1L) {
    # for chi-squared statistic
    if ("df" %in% colnames(df_expr)) df_expr %<>% mutate(df.error = df)

    df_expr %<>% mutate(expression = glue("list(
            {statistic.text}*'('*{df.error}*')'=='{statistic}', italic(p)=='{p.value}',
            {es.text}=='{estimate}', CI['{conf.level}']~'['*'{conf.low}', '{conf.high}'*']',
            {n.text}=='{n.obs}')"))
  }

  # 2 degrees of freedom -----------------

  if (!bayesian && no.parameters == 2L) {
    df_expr %<>% mutate(expression = glue("list(
            {statistic.text}({df}, {df.error})=='{statistic}', italic(p)=='{p.value}',
            {es.text}=='{estimate}', CI['{conf.level}']~'['*'{conf.low}', '{conf.high}'*']',
            {n.text}=='{n.obs}')"))
  }

  # convert `expression` to `language`
  df_expr %<>% .glue_to_expression()

  as_tibble(data) %>%
    relocate(matches("^effectsize$"), .before = matches("^estimate$")) %>%
    mutate(expression = df_expr$expression)
}


# utilities -----------------

#' Helper function to convert certain numeric columns to character type
#' @noRd
.data_to_char <- function(data, k = 2L, k.df = 0L, k.df.error = 0L) {
  data %>%
    mutate(
      across(.fns = ~ format_value(.x, k), .cols = matches("^est|^sta|p.value|.scale$|.low$|.high$|^log")),
      across(.fns = ~ format_value(.x, k.df), .cols = matches("^df$")),
      across(.fns = ~ format_value(.x, k.df.error), .cols = matches("^df.error$")),
      across(.fns = ~ paste0(.x * 100, "%"), .cols = matches("^conf.level$"))
    )
}

#' @noRd
.prettyNum <- function(x) prettyNum(x, big.mark = ",", scientific = FALSE)
