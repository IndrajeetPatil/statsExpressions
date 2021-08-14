#' @title Expressions with statistics for tidy regression dataframes
#' @name tidy_model_expressions
#'
#' @param ... Currently ignored.
#' @param data A tidy dataframe from regression model object.
#' @param statistic Which statistic is to be displayed (either `"t"` or `"f"`or
#'   `"z"` or `"chi"`) in the label.
#' @inheritParams oneway_anova
#'
#' @note This is an **experimental** function and may change in the future.
#'   Please do not use it yet in your workflow.
#'
#' @examples
#' set.seed(123)
#'
#' # tidy dataframe
#' df <- tidy_model_parameters(lm(wt ~ am * cyl, mtcars))
#'
#' # create a column containing expressions
#' tidy_model_expressions(df, statistic = "t")
#' @export

# function body
tidy_model_expressions <- function(data,
                                   statistic = NULL,
                                   k = 2L,
                                   effsize.type = "omega",
                                   ...) {

  # standardize
  statistic <- substring(tolower(statistic), 1L, 1L)

  # all operations will have to be done rowwise
  data %<>% dplyr::rowwise()

  # t-statistic --------------------------------

  if (statistic == "t") {
    data %<>%
      dplyr::mutate(
        label = dplyr::case_when(
          is.na(df.error) || is.infinite(df.error) ~ paste0(
            "list(~widehat(italic(beta))=='", format_value(estimate, k),
            "', ~italic(t)=='", format_value(statistic, k),
            "', ~italic(p)=='", format_num(p.value, k, TRUE), "')"
          ),
          TRUE ~ paste0(
            "list(~widehat(italic(beta))=='", format_value(estimate, k),
            "', ~italic(t)", "('", format_value(df.error, 0L), "')=='", format_value(statistic, k),
            "', ~italic(p)=='", format_num(p.value, k, TRUE), "')"
          )
        )
      )
  }

  # z-statistic ---------------------------------

  # if the statistic is z-value
  if (statistic == "z") {
    data %<>%
      dplyr::mutate(
        label = paste0(
          "list(~widehat(italic(beta))=='", format_value(estimate, k),
          "', ~italic(z)=='", format_value(statistic, k),
          "', ~italic(p)=='", format_num(p.value, k, TRUE), "')"
        )
      )
  }

  # chi^2-statistic -----------------------------

  if (statistic == "c") {
    data %<>%
      dplyr::mutate(
        label = paste0(
          "list(~widehat(italic(beta))=='", format_value(estimate, k),
          "', ~italic(chi)^2~", "('", format_value(df.error, 0L), "')=='", format_value(statistic, k),
          "', ~italic(p)=='", format_num(p.value, k, TRUE), "')"
        )
      )
  }

  # f-statistic ---------------------------------

  if (statistic == "f") {
    # which effect size is needed?
    if (effsize.type == "eta") es.text <- list(quote(widehat(italic(eta)[p]^2)))
    if (effsize.type == "omega") es.text <- list(quote(widehat(italic(omega)[p]^2)))

    # which effect size is needed?
    data %<>%
      dplyr::mutate(
        label = paste0(
          "list(~italic(F)", "('", format_value(df, 0L), "'*\",\"*'", format_value(df.error, 0L),
          "')=='", format_value(statistic, k),
          "', ~italic(p)=='", format_num(p.value, k, TRUE),
          "', ~", es.text, "=='", format_value(estimate, k), "')"
        )
      )
  }

  # return the ungrouped dataframe
  dplyr::ungroup(data)
}
