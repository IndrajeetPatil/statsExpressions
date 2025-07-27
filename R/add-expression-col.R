#' @title Template for expressions with statistical details
#' @name add_expression_col
#'
#' @description
#'
#' Creates an expression from a data frame containing statistical details.
#' Ideally, this data frame would come from having run `tidy_model_parameters()`
#' on your model object.
#'
#' This function is currently **not** stable and should not be used outside of
#' this package context.
#'
#' @param data A data frame containing details from the statistical analysis
#'   and should contain some or all of the the following columns:
#' - *statistic*: the numeric value of a statistic.
#' - *df.error*: the numeric value of a parameter being modeled (often degrees
#' of freedom for the test); irrelevant. if there are no degrees of freedom.
#' - *df*: relevant if the statistic in question has two degrees of freedom.
#' - *p.value*: the two-sided *p*-value associated with observed statistic.
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
#' @param digits,digits.df,digits.df.error Number of decimal places to display
#'   for the parameters (default: `0L`).
#' @param n An integer specifying the sample size used for the test.
#' @param n.text A character that specifies the design, which will determine
#'   what the `n` stands for. It defaults to `quote(italic("n")["pairs"])` if
#'   `paired = TRUE`, and to `quote(italic("n")["obs"])` if `paired = FALSE`. If
#'   you wish to customize this further, you will need to provide object of
#'   `language` type.
#' @param effsize.text A character that specifies the relevant effect size.
#' @param prior.type The type of prior.
#' @param ... Currently ignored.
#' @inheritParams oneway_anova
#' @inheritParams long_to_wide_converter
#'
#' @autoglobal
#'
#' @examples
#' set.seed(123)
#'
#' # creating a data frame with stats results
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
#'   digits         = 3L,
#'   digits.df      = 3L
#' )
#'
#' @template citation
#'
#' @export
add_expression_col <- function(
  data,
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
  digits = 2L,
  digits.df = 0L,
  digits.df.error = digits.df,
  ...
) {
  if (!"n.obs" %in% colnames(data)) data %<>% mutate(n.obs = n)
  if (!"effectsize" %in% colnames(data)) data %<>% mutate(effectsize = method)
  data %<>% rename_with(recode, bayes.factor = "bf10")

  bayesian <- any("bf10" == colnames(data))
  no.parameters <- sum("df.error" %in% names(data) + "df" %in% names(data))

  # special case for Bayesian contingency table analysis
  if (bayesian && grepl("contingency", data$method[[1L]], fixed = TRUE)) data %<>% mutate(effectsize = "Cramers_v")

  # dealing with exactly 0 p-values
  if ("p.value" %in% colnames(data)) data %<>% mutate(p.value = if_else(p.value == 0, .Machine$double.xmin, p.value))

  df_expr <- data %>% # convert needed columns to character type
    .data_to_char(digits, digits.df, digits.df.error) %>%
    mutate(
      statistic.text     = statistic.text %||% extract_statistic_text(tolower(method)),
      es.text            = effsize.text %||% extract_estimate_type(tolower(effectsize)),
      prior.distribution = prior_switch(tolower(method)),
      n.obs              = .prettyNum(n.obs)
    )

  # Bayesian analysis ------------------------------

  if (bayesian) {
    df_expr %<>% mutate(expression = glue("list(
            log[e]*(BF['01'])=='{format_value(-log(bf10), digits)}',
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
    if ("df" %in% colnames(df_expr)) df_expr %<>% mutate(df.error = df) # for chi-squared statistic

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

  data %>%
    relocate(matches("^effectsize$"), .before = matches("^estimate$")) %>%
    mutate(expression = df_expr$expression) %>%
    .add_package_class()
}


# utilities -----------------

#' Helper function to convert certain numeric columns to character type
#' @noRd
.data_to_char <- function(data, digits = 2L, digits.df = 0L, digits.df.error = 0L) {
  data %>%
    mutate(
      across(.fns = ~ .to_char(.x, digits), .cols = matches("^est|^sta|p.value|.scale$|.low$|.high$|^log")),
      across(.fns = ~ .to_char(.x, digits.df), .cols = matches("^df$")),
      across(.fns = ~ .to_char(.x, digits.df.error), .cols = matches("^df.error$")),
      across(.fns = ~ paste0(.x * 100L, "%"), .cols = matches("^conf.level$"))
    )
}

#' @noRd
.prettyNum <- function(x) prettyNum(x, big.mark = ",", scientific = FALSE)

#' @noRd
.to_char <- function(x, digits = 2L) format_value(x, digits, missing = "NA")
