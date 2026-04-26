#' @title Random-effects meta-analysis
#' @name meta_analysis
#'
#' @description
#' Parametric, non-parametric, robust, and Bayesian random-effects meta-analysis.
#'
#' @param data A data frame. It **must** contain columns named `estimate` (effect
#'   sizes or outcomes)  and `std.error` (corresponding standard errors). These
#'   two columns will be used:
#'   - as `yi`  and `sei` arguments in [`metafor::rma()`] (for **parametric** test)
#'   - as `yi`  and `sei` arguments in [`metaplus::metaplus()`] (for **robust** test)
#'   - as `y` and `SE` arguments in [`metaBMA::meta_random()`] (for **Bayesian** test)
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
#' @returns
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
#' @autoglobal
#'
#' @examplesIf identical(Sys.getenv("NOT_CRAN"), "true") && requireNamespace("metaplus", quietly = TRUE)
#' set.seed(123)
#' library(statsExpressions)
#'
#' # let's use `mag` dataset from `{metaplus}`
#' data(mag, package = "metaplus")
#' dat <- dplyr::rename(mag, estimate = yi, std.error = sei)
#'
#' # ----------------------- parametric ----------------------------------------
#'
#' @examplesIf identical(Sys.getenv("NOT_CRAN"), "true") && requireNamespace("metaplus") && requireNamespace("metafor")
#'
#' meta_analysis(dat)
#'
#' @examplesIf identical(Sys.getenv("NOT_CRAN"), "true") && requireNamespace("metaplus")
#'
#' # ----------------------- robust --------------------------------------------
#'
#' meta_analysis(dat, type = "random", random = "normal")
#'
#' @examplesIf identical(Sys.getenv("NOT_CRAN"), "true") && requireNamespace("metaplus") && requireNamespace("metaBMA")
#'
#' # ----------------------- Bayesian ------------------------------------------
#'
#' meta_analysis(dat, type = "bayes")
#'
#' @template citation
#'
#' @export
meta_analysis <- function(
  data,
  type = "parametric",
  random = "mixture",
  digits = 2L,
  conf.level = 0.95,
  ...
) {
  type <- extract_stats_type(type)

  # nolint start: line_length_linter.
  .meta_args <- switch(
    type,
    parametric = list(
      .ns = "metafor",
      .fn = "rma",
      .f.args = list(yi = quote(estimate), sei = quote(std.error), ...)
    ),
    robust = list(
      .ns = "metaplus",
      .fn = "metaplus",
      .f.args = list(
        yi = quote(estimate),
        sei = quote(std.error),
        random = random,
        ...
      )
    ),
    bayes = list(
      .ns = "metaBMA",
      .fn = "meta_random",
      .f.args = list(y = quote(estimate), SE = quote(std.error), ...)
    )
  )
  .ns <- .meta_args$.ns
  .fn <- .meta_args$.fn
  .f.args <- .meta_args$.f.args
  # nolint end

  check_if_installed(.ns)

  stats_df <- eval(call2(.fn = .fn, .ns = .ns, data = data, !!!.f.args)) |>
    tidy_model_parameters(include_studies = FALSE, ci = conf.level)

  if (type != "bayes") {
    stats_df <- mutate(stats_df, effectsize = "meta-analytic summary estimate")
  }
  if (type == "bayes") {
    stats_df <- mutate(
      stats_df,
      effectsize = "meta-analytic posterior estimate"
    )
  }

  add_expression_col(
    stats_df,
    n = nrow(data),
    n.text = list(quote(italic("n")["effects"])),
    digits = digits
  )
}
