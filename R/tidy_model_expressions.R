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

  # standardize the statistic naming
  statistic <- substring(tolower(statistic), 1L, 1L)

  # convert the necessary to character type for expression
  df <- .data_to_char(data, k)

  # t-statistic --------------------------------

  es.text <- list(quote(widehat(italic(beta))))

  if (statistic == "t") {
    df %<>%
      mutate(
        label = case_when(
          df.error %in% c("", "Inf") ~ glue("list({es.text}=='{estimate}', italic(t)=='{statistic}', italic(p)=='{p.value}')"),
          TRUE ~ glue("list({es.text}=='{estimate}', italic(t)('{df.error}')=='{statistic}', italic(p)=='{p.value}')")
        )
      )
  }

  # z-statistic ---------------------------------

  # if the statistic is z-value
  if (statistic == "z") {
    df %<>%
      mutate(label = glue("list({es.text}=='{estimate}', italic(z)=='{statistic}', italic(p)=='{p.value}')"))
  }

  # chi^2-statistic -----------------------------

  if (statistic == "c") {
    df %<>%
      mutate(label = glue("list({es.text}=='{estimate}', italic(chi)^2*('{df.error}')=='{statistic}', italic(p)=='{p.value}')"))
  }

  # f-statistic ---------------------------------

  if (statistic == "f") {
    # which effect size is needed?
    if (effsize.type == "eta") es.text <- list(quote(widehat(italic(eta)[p]^2)))
    if (effsize.type == "omega") es.text <- list(quote(widehat(italic(omega)[p]^2)))

    # which effect size is needed?
    df %<>%
      mutate(label = glue("list({es.text}=='{estimate}', italic(F)('{df}', '{df.error}')=='{statistic}', italic(p)=='{p.value}')"))
  }

  # return the ungrouped dataframe
  left_join(data, select(df, term, label), by = "term")
}

#' Helper function to convert certain numeric columns to character type
#' @noRd

.data_to_char <- function(data, k = 2L, k.df = 0L) {
  data %>%
    mutate(
      across(.fns = ~ format_value(.x, k), .cols = matches("^est|^sta|p.value")),
      across(.fns = ~ format_value(.x, k.df), .cols = matches("^df"))
    )
}
