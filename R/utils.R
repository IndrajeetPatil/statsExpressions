#' @title Formatting numeric values
#' @name format_num
#'
#' @description
#'
#' Function to format an R object for pretty printing with a specified (`k`)
#' number of decimal places. The function also allows really small *p*-values to
#' be denoted as `"p < 0.001"` rather than `"p = 0.000"`. Note that if `p.value`
#' is set to `TRUE`, the minimum value of `k` allowed is `3`. If `k` is set to
#' less than 3, the function will ignore entered `k` value and use `k = 3`
#' instead.
#'
#' @note This function is **not** vectorized.
#'
#' @param x A numeric value.
#' @param k Number of digits after decimal point (should be an integer)
#'   (Default: `k = 3L`).
#' @param p.value Decides whether the number is a *p*-value (Default: `FALSE`).
#' @param ... Currently ignored.
#'
#' @return Formatted numeric value.
#'
#' @examples
#' format_num(0.0000123, k = 2, p.value = TRUE)
#' format_num(0.008675, k = 2, p.value = TRUE)
#' format_num(0.003458, k = 3, p.value = FALSE)
#' @export

# function body
format_num <- function(x, k = 2L, p.value = FALSE, ...) {

  # for example, if p.value is 0.002, it should be displayed as such
  if (k < 3L && p.value) k <- 3L

  # formatting the output properly
  output <- format(round(x, digits = k), nsmall = k)

  # if it's a p-value, then format it properly
  if (!is.na(x)) {
    if (p.value && x < 0.001) output <- prettyNum(x, scientific = TRUE, digits = k)
    if (!p.value && x > 1000) output <- prettyNum(x, scientific = TRUE, digits = 2L)
  }

  # this will return a character
  output
}


#' @name stats_type_switch
#' @title Switch type of statistics.
#'
#' @description
#'
#' Relevant mostly for `ggstatsplot` and `statsExpressions` packages, where
#' different statistical approaches are supported via this argument: parametric,
#' non-parametric, robust, and Bayesian. This switch function converts strings
#' entered by users to a common pattern for convenience.
#'
#' @param type A character specifying the type of statistical approach:
#'   - `"parametric"`
#'   - `"nonparametric"`
#'   - `"robust"`
#'   - `"bayes"`
#'
#' You can specify just the initial letter.
#'
#' @examples
#' stats_type_switch("p")
#' stats_type_switch("bf")
#' @export

stats_type_switch <- function(type) {
  dplyr::case_when(
    grepl("^p", type, TRUE) ~ "parametric",
    grepl("^n|^s", type, TRUE) ~ "nonparametric", # s is for Spearman's rho
    grepl("^r", type, TRUE) ~ "robust",
    grepl("^b", type, TRUE) ~ "bayes",
    TRUE ~ "parametric"
  )
}
