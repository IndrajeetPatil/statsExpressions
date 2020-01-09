#' @title Template for subtitles with statistical details for tests
#' @name expr_template
#' @author \href{https://github.com/IndrajeetPatil}{Indrajeet Patil}
#'
#' @param no.parameters An integer that specifies that the number of parameters
#'   for the statistical test. Can be `0` for non-parametric tests, `1` for
#'   tests based on *t*-statistic or chi-squared statistic, `2` for tests based
#'   on *F*-statistic.
#' @param stat.title A character describing the test being run, which will be
#'   added as a prefix in the subtitle. The default is `NULL`. An example of a
#'   `stat.title` argument will be something like `"Student's t-test: "`.
#' @param stats.df A dataframe containing the following columns:
#' \itemize{
#'   \item *statistic*: the numeric value of a statistic.
#'   \item *parameter*: the numeric value of a parameter being modeled (often
#' degrees of freedom for the test); note that if `no.parameters = 0L` (e.g.,
#' for non-parametric tests), this column will be irrelevant.
#'   \item *parameter1*, *parameter2* relevant only if the statistic in question
#' has two degrees of freedom (e.g., anova).
#'   \item *p.value* the two-sided *p*-value associated with the observed
#' statistic.
#' }
#' @param statistic.text A character that specifies the relevant test statistic.
#'   For example, for tests with *t*-statistic, `statistic.text = "t"`. If you
#'   want to use plotmath, you will have to quote the argument (e.g.,
#'   `quote(italic("t"))`).
#' @param effsize.df A dataframe containing the following columns:
#' \itemize{
#'   \item *estimate*: estimated value of the effect size.
#'   \item *conf.low*:  lower bound for effect size estimate.
#'   \item *conf.high*: upper bound for effect size estimate.
#' }
#' @param effsize.text A character that specifies the relevant effect size.
#'   For example, for Cohen's *d* statistic, `effsize.text = "d"`. If you
#'   want to use plotmath, you will have to quote the argument (e.g.,
#'   `quote(italic("d"))`).
#' @param k Number of digits after decimal point (should be an integer)
#'   (Default: `k = 2`).
#' @param k.parameter,k.parameter2 Number of decimal places to display for the
#'   parameters (default: `0`).
#' @param n An integer specifying the sample size used for the test.
#' @param n.text A character that specifies the design, which will determine
#'   what the `n` stands for. For example, for repeated measures, this can be
#'   `quote(italic("n")["pairs"])`, while for independent subjects design this
#'   can be `quote(italic("n")["obs"])`. If `NULL`, defaults to generic
#'   `quote(italic("n"))`.
#' @param ... Currently ignored.
#' @inheritParams t1way_ci
#'
#' @examples
#' set.seed(123)
#'
#' # creating a dataframe with stats results
#' stats_df <- cbind.data.frame(
#'   statistic = 5.494,
#'   parameter = 29.234,
#'   p.value = 0.00001
#' )
#'
#' # creating a dataframe with effect size results
#' effsize_df <- cbind.data.frame(
#'   estimate = -1.980,
#'   conf.low = -2.873,
#'   conf.high = -1.088
#' )
#'
#' # subtitle for *t*-statistic with Cohen's *d* as effect size
#' statsExpressions::expr_template(
#'   no.parameters = 1L,
#'   stats.df = stats_df,
#'   effsize.df = effsize_df,
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
                          stat.title = NULL,
                          statistic.text,
                          stats.df,
                          effsize.text,
                          effsize.df,
                          n,
                          conf.level = 0.95,
                          k = 2L,
                          k.parameter = 0L,
                          k.parameter2 = 0L,
                          n.text = NULL,
                          ...) {

  # if sample size nature is not specified, use generic n
  if (rlang::is_null(n.text)) n.text <- quote(italic("n"))

  # extracting the common values
  statistic <- stats.df$statistic[[1]]
  p.value <- stats.df$p.value[[1]]
  effsize.estimate <- effsize.df$estimate[[1]]
  effsize.LL <- effsize.df$conf.low[[1]]
  effsize.UL <- effsize.df$conf.high[[1]]

  # ------------------ statistic with 0 degrees of freedom --------------------

  if (no.parameters == 0L) {
    # preparing subtitle
    subtitle <-
      substitute(
        expr = paste(
          stat.title,
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
          stat.title = stat.title,
          statistic.text = statistic.text,
          statistic = specify_decimal_p(x = statistic, k = k),
          p.value = specify_decimal_p(x = p.value, k = k, p.value = TRUE),
          effsize.text = effsize.text,
          effsize.estimate = specify_decimal_p(x = effsize.estimate, k = k),
          conf.level = paste(conf.level * 100, "%", sep = ""),
          effsize.LL = specify_decimal_p(x = effsize.LL, k = k),
          effsize.UL = specify_decimal_p(x = effsize.UL, k = k),
          n = n,
          n.text = n.text
        )
      )
  }

  # ------------------ statistic with 1 degree of freedom --------------------

  if (no.parameters == 1L) {
    # check if parameter is specified
    parameter <- stats.df$parameter[[1]]

    # preparing subtitle
    subtitle <-
      substitute(
        expr = paste(
          stat.title,
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
          stat.title = stat.title,
          statistic.text = statistic.text,
          statistic = specify_decimal_p(x = statistic, k = k),
          parameter = specify_decimal_p(x = parameter, k = k.parameter),
          p.value = specify_decimal_p(x = p.value, k = k, p.value = TRUE),
          effsize.text = effsize.text,
          effsize.estimate = specify_decimal_p(x = effsize.estimate, k = k),
          conf.level = paste(conf.level * 100, "%", sep = ""),
          effsize.LL = specify_decimal_p(x = effsize.LL, k = k),
          effsize.UL = specify_decimal_p(x = effsize.UL, k = k),
          n = n,
          n.text = n.text
        )
      )
  }

  # ------------------ statistic with 2 degrees of freedom -----------------

  if (no.parameters == 2L) {
    # check if parameters are specified
    parameter <- stats.df$parameter1[[1]]
    parameter2 <- stats.df$parameter2[[1]]

    # preparing subtitle
    subtitle <-
      substitute(
        expr = paste(
          stat.title,
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
          stat.title = stat.title,
          statistic.text = statistic.text,
          statistic = specify_decimal_p(x = statistic, k = k),
          parameter1 = specify_decimal_p(x = parameter, k = k.parameter),
          parameter2 = specify_decimal_p(x = parameter2, k = k.parameter2),
          p.value = specify_decimal_p(x = p.value, k = k, p.value = TRUE),
          effsize.text = effsize.text,
          effsize.estimate = specify_decimal_p(x = effsize.estimate, k = k),
          conf.level = paste(conf.level * 100, "%", sep = ""),
          effsize.LL = specify_decimal_p(x = effsize.LL, k = k),
          effsize.UL = specify_decimal_p(x = effsize.UL, k = k),
          n = n,
          n.text = n.text
        )
      )
  }

  # return the formatted subtitle
  return(subtitle)
}

#' @noRd
#'
#' @importFrom dplyr rename_all recode
#'
#' @keywords internal

rcompanion_cleaner <- function(object) {
  # if a list, extract the first component containing results
  if (inherits(object, "list")) object <- object[[1]]

  # rename columns uniformly
  tibble::as_tibble(object) %>%
    dplyr::rename_all(
      .tbl = .,
      .funs = dplyr::recode,
      epsilon.squared = "estimate",
      r = "estimate",
      rho = "estimate",
      W = "estimate",
      Cramer.V = "estimate",
      Value = "estimate",
      lower.ci = "conf.low",
      upper.ci = "conf.high"
    )
}


#' @title Converts long-format dataframe to wide-format dataframe
#' @name long_to_wide_converter
#' @author \href{https://github.com/IndrajeetPatil}{Indrajeet Patil}
#' @description This conversion is helpful mostly for repeated measures design.
#'
#' @inheritParams expr_t_parametric
#'
#' @importFrom rlang !! enquo :=
#' @importFrom dplyr n row_number select mutate mutate_at group_by ungroup
#' @importFrom tidyr spread
#'
#' @examples
#' \donttest{
#' statsExpressions::long_to_wide_converter(
#'   data = iris_long,
#'   x = condition,
#'   y = value,
#'   paired = TRUE
#' )
#' }
#' @export

long_to_wide_converter <- function(data, x, y, paired = TRUE) {

  # make sure both quoted and unquoted arguments are allowed
  x <- rlang::ensym(x)
  y <- rlang::ensym(y)

  # creating a dataframe
  data %<>%
    dplyr::select(.data = ., {{ x }}, {{ y }}) %>%
    dplyr::mutate(.data = ., {{ x }} := droplevels(as.factor({{ x }}))) %>%
    tibble::as_tibble(x = .)

  # figuring out number of levels in the grouping factor
  x_n_levels <- nlevels(data %>% dplyr::pull({{ x }}))[[1]]

  # wide format
  data_wide <-
    data %>%
    dplyr::filter(.data = ., !is.na({{ x }})) %>%
    dplyr::group_by(.data = ., {{ x }}) %>%
    dplyr::mutate(.data = ., rowid = dplyr::row_number()) %>%
    dplyr::ungroup(x = .) %>%
    dplyr::filter(.data = ., !is.na({{ y }}))

  # clean up for repeated measures design
  if (isTRUE(paired)) {
    data_wide %<>%
      dplyr::group_by(.data = ., rowid) %>%
      dplyr::mutate(.data = ., n = dplyr::n()) %>%
      dplyr::ungroup(x = .) %>%
      dplyr::filter(.data = ., n == x_n_levels) %>%
      dplyr::select(.data = ., {{ x }}, {{ y }}, rowid)
  }

  # spreading the columns of interest
  data_wide %<>%
    tidyr::spread(
      data = .,
      key = {{ x }},
      value = {{ y }},
      convert = TRUE
    )

  # return the dataframe in wide format
  return(data_wide)
}


#' @noRd
#' @keywords internal

df_cleanup_paired <- function(data, x, y) {
  data %<>%
    long_to_wide_converter(data = ., x = {{ x }}, y = {{ y }}) %>%
    tidyr::gather(data = ., key, value, -rowid) %>%
    dplyr::arrange(.data = ., rowid) %>%
    dplyr::rename(.data = ., {{ x }} := key, {{ y }} := value) %>%
    dplyr::mutate(.data = ., {{ x }} := factor({{ x }}))
}
