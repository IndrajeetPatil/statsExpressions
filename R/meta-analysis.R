# nolint start: line_length_linter. pkgdown needs the examplesIf guard on one line.
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
#' The package needed for the chosen `type` (`{metafor}`, `{metaplus}`, or
#' `{metaBMA}`) is only suggested, so you will be asked to install it if it is
#' not available.
#'
#' @autoglobal
#'
#' @examples
#' \dontshow{if (identical(Sys.getenv("NOT_CRAN"), "true") && rlang::is_installed(c("metafor", "metaplus", "metaBMA"))) withAutoprint(\{ # examplesIf}
#' @example man/examples/examples-meta-analysis.R
#' @examples
#' \dontshow{\}) # examplesIf}
#'
#' @template citation
#'
#' @export
# nolint end
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
    tidy_model_parameters(include_studies = FALSE, ci = conf.level) |>
    mutate(
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
