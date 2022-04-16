#' @title Random-effects meta-analyses
#' @name meta_analysis
#'
#' @param data A dataframe. It **must** contain columns named `estimate` (effect
#'   sizes or outcomes)  and `std.error` (corresponding standard errors). These
#'   two columns will be used:
#'   - as `yi`  and `sei` arguments in `metafor::rma` (for **parametric** test)
#'   or `metaplus::metaplus` (for **robust** test)
#'   - as `y` and `SE` arguments in `metaBMA::meta_random` (for **Bayesian**
#'   test).
#' @inheritParams one_sample_test
#' @inheritParams metaplus::metaplus
#' @inheritParams oneway_anova
#' @param ... Additional arguments passed to the respective meta-analysis
#'   function.
#'
#' @description
#'
#' ```{r child="man/rmd-fragments/table_intro.Rmd"}
#' ```
#'
#' ```{r child="man/rmd-fragments/corr_test.Rmd"}
#' ```
#'
#' @return
#'
#' ```{r child="man/rmd-fragments/return.Rmd"}
#' ```
#'
#' @note
#'
#' **Important**: The function assumes that you have already downloaded the
#' needed package (`{metafor}`, `{metaplus}`, or `{metaBMA}`) for meta-analysis.
#' If they are not available, you will be asked to install them.
#'
#' @examples
#' \donttest{
#' # a dataframe with estimates and standard errors (`mag` dataset from `{metaplus}`)
#' df <- structure(list(
#'   study = structure(c(
#'     8L, 10L, 15L, 1L, 4L, 11L, 3L, 2L, 14L, 9L, 12L, 5L, 16L, 7L, 13L, 6L
#'   ), .Label = c(
#'     "Abraham", "Bertschat", "Ceremuzynski", "Feldstedt", "Golf",
#'     "ISIS-4", "LIMIT-2", "Morton", "Pereira", "Rasmussen", "Schechter", "Schechter 1",
#'     "Schechter 2", "Singh", "Smith", "Thogersen"
#'   ), class = "factor"),
#'   estimate = c(
#'     -0.8303483, -1.056053, -1.27834, -0.0434851, 0.2231435,
#'     -2.40752, -1.280934, -1.191703, -0.695748, -2.208274, -2.03816,
#'     -0.8501509, -0.7932307, -0.2993399, -1.570789, 0.0575873
#'   ),
#'   std.error = c(
#'     1.24701799987009, 0.41407060026039, 0.808139200261935,
#'     1.42950999996502, 0.489168400451215, 1.07220799987689, 1.1937340001022,
#'     1.66129199992054, 0.536177600240816, 1.10964800004326, 0.780726300312728,
#'     0.618448600127771, 0.625866199758383, 0.146572899950844,
#'     0.574039500383031, 0.0316420922190679
#'   )
#' ), row.names = c(NA, -16L), class = "data.frame")
#'
#' # setup
#' set.seed(123)
#' library(statsExpressions)
#' options(tibble.width = Inf, pillar.bold = TRUE, pillar.neg = TRUE)
#'
#' meta_analysis(df) # parametric
#' # meta_analysis(df, type = "random", random = "normal") # robust
#' # meta_analysis(df, type = "bayes") # Bayesian
#' }
#' @export
meta_analysis <- function(data,
                          type = "parametric",
                          random = "mixture",
                          k = 2L,
                          conf.level = 0.95,
                          ...) {
  # check the type of test
  type <- stats_type_switch(type)

  # additional arguments
  if (type != "bayes") .f.args <- list(yi = quote(estimate), sei = quote(std.error), random = random, ...)
  if (type == "bayes") .f.args <- list(y = quote(estimate), SE = quote(std.error), ...)

  # styler: off
  if (type == "parametric") c(.ns, .fn) %<-% c("metafor", "rma")
  if (type == "robust") c(.ns, .fn)     %<-% c("metaplus", "metaplus")
  if (type == "bayes") c(.ns, .fn)      %<-% c("metaBMA", "meta_random")
  # styler: on

  # needed package installed?
  check_if_installed(.ns)

  # construct a call and then extract a tidy dataframe
  stats_df <- eval(call2(.fn = .fn, .ns = .ns, data = data, !!!.f.args)) %>%
    tidy_model_parameters(include_studies = FALSE, ci = conf.level)

  # add a column describing effect size
  if (type != "bayes") stats_df %<>% mutate(effectsize = "meta-analytic summary estimate")
  if (type == "bayes") stats_df %<>% mutate(effectsize = "meta-analytic posterior estimate")

  # add expression column
  add_expression_col(
    data     = stats_df,
    n        = nrow(data),
    n.text   = list(quote(italic("n")["effects"])),
    k        = k,
  )
}
