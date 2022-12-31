#' @title Random-effects meta-analysis
#' @name meta_analysis
#'
#' @description
#' Parametric, non-parametric, robust, and Bayesian random-effects meta-analysis.
#'
#' @param data A data frame. It **must** contain columns named `estimate` (effect
#'   sizes or outcomes)  and `std.error` (corresponding standard errors). These
#'   two columns will be used:
#'   - as `yi`  and `sei` arguments in `metafor::rma()` (for **parametric** test)
#'   or `metaplus::metaplus()` (for **robust** test)
#'   - as `y` and `SE` arguments in `metaBMA::meta_random()` (for **Bayesian**
#'   test).
#' @inheritParams one_sample_test
#' @inheritParams metaplus::metaplus
#' @inheritParams oneway_anova
#' @param ... Additional arguments passed to the respective meta-analysis
#'   function.
#'
#' @section Random-effects meta-analysis:
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
#' @examplesIf identical(Sys.getenv("NOT_CRAN"), "true") && requireNamespace("metafor", quietly = TRUE)
#' # setup
#' set.seed(123)
#' library(statsExpressions)
#' options(tibble.width = Inf, pillar.bold = TRUE, pillar.neg = TRUE)
#'
#' # a data frame with estimates and standard errors
#' # (`mag` dataset from `{metaplus}`)
#' df <- tibble::tribble(
#'   ~study, ~estimate, ~std.error,
#'   "Abraham", -0.83, 1.247,
#'   "Bertschat", -1.056, 0.414,
#'   "Ceremuzynski", -1.278, 0.808,
#'   "Feldstedt", -0.043, 1.429,
#'   "Golf", 0.223, 0.489,
#'   "ISIS-4", -2.407, 1.072,
#'   "LIMIT-2", -1.28, 1.193,
#'   "Morton", -1.191, 1.661,
#'   "Pereira", -0.695, 0.536,
#'   "Rasmussen", -2.208, 1.109,
#'   "Schechter", -2.038, 0.78,
#'   "Schechter 1", -0.85, 0.618,
#'   "Schechter 2", -0.793, 0.625,
#'   "Singh", -0.299, 0.146,
#'   "Smith", -1.57, 0.574,
#'   "Thogersen", 0.057, 0.031
#' )
#'
#' # parametric
#' meta_analysis(df)
#'
#' @examplesIf identical(Sys.getenv("NOT_CRAN"), "true") && requireNamespace("metaplus", quietly = TRUE)
#' # robust
#' meta_analysis(df, type = "random", random = "normal")
#'
#' @examplesIf identical(Sys.getenv("NOT_CRAN"), "true") && requireNamespace("metaBMA", quietly = TRUE)
#' # Bayesian
#' meta_analysis(df, type = "bayes")
#'
#' @export
meta_analysis <- function(data,
                          type = "parametric",
                          random = "mixture",
                          k = 2L,
                          conf.level = 0.95,
                          ...) {
  type <- stats_type_switch(type)

  # styler: off
  if (type == "parametric") c(.ns, .fn) %<-% c("metafor", "rma")
  if (type == "robust") c(.ns, .fn)     %<-% c("metaplus", "metaplus")
  if (type == "bayes") c(.ns, .fn)      %<-% c("metaBMA", "meta_random")
  # styler: on

  check_if_installed(.ns)

  if (type != "bayes") .f.args <- list(yi = quote(estimate), sei = quote(std.error), random = random, ...)
  if (type == "bayes") .f.args <- list(y = quote(estimate), SE = quote(std.error), ...)

  # construct a call and then extract a tidy data frame
  stats_df <- eval(call2(.fn = .fn, .ns = .ns, data = data, !!!.f.args)) %>%
    tidy_model_parameters(include_studies = FALSE, ci = conf.level)

  if (type != "bayes") stats_df %<>% mutate(effectsize = "meta-analytic summary estimate")
  if (type == "bayes") stats_df %<>% mutate(effectsize = "meta-analytic posterior estimate")

  add_expression_col(stats_df, n = nrow(data), n.text = list(quote(italic("n")["effects"])), k = k)
}
