#' @name stats_type_switch
#' @title Switch the type of statistics.
#'
#' @description
#'
#' Relevant mostly for `{ggstatsplot}` and `{statsExpressions}` packages, where
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

# styler: off
stats_type_switch <- function(type) {
  case_when(
    grepl("^p", type, TRUE)    ~ "parametric",
    grepl("^n|^s", type, TRUE) ~ "nonparametric", # s is for Spearman's rho
    grepl("^r", type, TRUE)    ~ "robust",
    grepl("^b", type, TRUE)    ~ "bayes",
    TRUE                       ~ "parametric"
  )
}
# styler: on


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
