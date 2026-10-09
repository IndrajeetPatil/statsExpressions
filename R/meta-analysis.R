#' @title Random-effects meta-analysis
#' @name meta_analysis
#'
#' @description
#' Parametric, robust, and Bayesian random-effects meta-analysis.
#'
#' A non-parametric meta-analysis is not available, so `type = "nonparametric"`
#' is not supported.
#'
#' @param data A data frame. It **must** contain columns named `estimate` (effect
#'   sizes or outcomes) and `std.error` (corresponding standard errors). These
#'   two columns will be used:
#'   - as `yi` and `sei` arguments in [`metafor::rma()`] (for **parametric** test)
#'   - as `yi` and `sei` arguments in [`metaplus::metaplus()`] (for **robust** test)
#'   - as `y` and `SE` arguments in [`metaBMA::meta_random()`] (for **Bayesian** test)
#' @param random The type of random-effects distribution for the robust
#'   meta-analysis: `"mixture"` (default; mixture of normals), `"normal"`, or
#'   `"t-dist"` (*t*-distribution). Passed to [`metaplus::metaplus()`] and only
#'   used when `type = "robust"`.
#' @inheritParams one_sample_test
#' @inheritParams oneway_anova
#' @param ... Additional arguments passed to the respective meta-analysis
#'   function.
#'
#' @section Random-effects meta-analysis:
#'
#' ```{r child="man/rmd-fragments/table_intro.Rmd"}
#' ```
#'
#' ```{r child="man/rmd-fragments/meta_analysis.Rmd"}
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
#' meta_analysis(dat, type = "robust", random = "normal")
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

  check_if_installed(switch(
    type,
    parametric = "metafor",
    robust = "metaplus",
    bayes = "metaBMA"
  ))

  stats_df <- switch(
    type,
    parametric = inject(metafor::rma(
      yi = !!sym("estimate"),
      sei = !!sym("std.error"),
      data = !!data,
      ...
    )),
    robust = inject(metaplus::metaplus(
      yi = !!sym("estimate"),
      sei = !!sym("std.error"),
      random = random,
      data = !!data,
      ...
    )),
    bayes = inject(metaBMA::meta_random(
      y = !!sym("estimate"),
      SE = !!sym("std.error"),
      data = !!data,
      ...
    ))
  ) |>
    tidy_model_parameters(include_studies = FALSE, ci = conf.level)

  stats_df <- mutate(
    stats_df,
    effectsize = if (type == "bayes") {
      "meta-analytic posterior estimate"
    } else {
      "meta-analytic summary estimate"
    }
  )

  add_expression_col(
    stats_df,
    n = nrow(data),
    n.text = list(quote(italic("n")["effects"])),
    digits = digits
  )
}
