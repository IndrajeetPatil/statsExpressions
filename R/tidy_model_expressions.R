#' @title Expressions with statistics for tidy regression data frames
#' @name tidy_model_expressions
#'
#' @param ... Currently ignored.
#' @param data A tidy data frame from regression model object (see
#'   `statsExpressions::tidy_model_parameters()`).
#' @param statistic Which statistic is to be displayed (either `"t"` or `"f"`or
#'   `"z"` or `"chi"`) in the expression.
#' @inheritParams oneway_anova
#'
#' @details
#' When any of the necessary numeric column values (`estimate`, `statistic`,
#' `p.value`) are missing, for these rows, a `NULL` is returned instead of an
#' expression with empty strings.
#'
#' @note
#'
#' This is an **experimental** function and may change in the future. Please do
#' not use it yet in your workflow.
#'
#' @autoglobal
#'
#' @examplesIf identical(Sys.getenv("NOT_CRAN"), "true")
#' # setup
#' set.seed(123)
#' library(statsExpressions)
#'
#' # extract a tidy data frame
#' df_expr <- tidy_model_parameters(lm(wt ~ am * cyl, mtcars))
#'
#' # create a column containing expression; the expression will depend on `statistic`
#' tidy_model_expressions(df_expr, statistic = "t")
#' tidy_model_expressions(df_expr, statistic = "z")
#' tidy_model_expressions(df_expr, statistic = "chi")
#' @export
tidy_model_expressions <- function(data,
                                   statistic = NULL,
                                   k = 2L,
                                   effsize.type = "omega",
                                   ...) {
  # standardize the statistic naming
  statistic <- substring(tolower(statistic), 1L, 1L)

  # if any of the necessary numeric columns are missing, there shouldn't be an
  # expression corresponding to that row
  #
  # convert the necessary columns to character type for expression
  df_expr <- data %>%
    filter(if_all(
      .cols = matches("estimate|statistic|std.error|p.value"),
      .fns = Negate(is.na)
    )) %>%
    .data_to_char(k)

  # effect size text for the expression (common for t, z, and chi^2)
  es.text <- list(quote(widehat(italic(beta))))

  # t-statistic --------------------------------

  # nolint start: line_length_linter.
  if (statistic == "t") {
    df_expr %<>% mutate(
      expression = case_when(
        df_expr.error %in% c("NA", "Inf") ~ glue("list({es.text}=='{estimate}', italic(t)=='{statistic}', italic(p)=='{p.value}')"),
        .default = glue("list({es.text}=='{estimate}', italic(t)('{df_expr.error}')=='{statistic}', italic(p)=='{p.value}')")
      )
    )
  }

  # z-statistic ---------------------------------

  if (statistic == "z") {
    df_expr %<>% mutate(expression = glue("list({es.text}=='{estimate}', italic(z)=='{statistic}', italic(p)=='{p.value}')"))
  }

  # chi^2-statistic -----------------------------

  if (statistic == "c") {
    df_expr %<>% mutate(expression = glue("list({es.text}=='{estimate}', italic(chi)^2*('{df_expr.error}')=='{statistic}', italic(p)=='{p.value}')"))
  }

  # f-statistic ---------------------------------

  if (statistic == "f") {
    if (effsize.type == "eta") es.text <- list(quote(widehat(italic(eta)[p]^2)))
    if (effsize.type == "omega") es.text <- list(quote(widehat(italic(omega)[p]^2)))

    df_expr %<>% mutate(expression = glue("list({es.text}=='{estimate}', italic(F)('{df_expr}', '{df_expr.error}')=='{statistic}', italic(p)=='{p.value}')"))
  }

  # nolint end

  # Replace `NA` with `NULL` to show nothing instead of an empty string ("")
  left_join(data, select(df_expr, term, expression), by = "term") %>%
    .glue_to_expression()
}


#' @keywords internal
#' @noRd
.glue_to_expression <- function(data) {
  data %>%
    rowwise() %>%
    mutate(expression = list(parse_expr(expression))) %>%
    ungroup() %>% # convert from `expression` to `language`
    mutate(expression = case_when(
      is.na(unlist(expression)) ~ list(NULL),
      .default = unlist(expression)
    )) %>%
    .add_package_class()
}

#' @keywords internal
#' @noRd
.add_package_class <- function(data) {
  data <- as_tibble(data)
  class(data) <- c("statsExpressions", class(data))
  data
}
