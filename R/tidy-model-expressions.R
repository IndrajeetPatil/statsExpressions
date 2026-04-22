#' @title Expressions with statistics for tidy regression data frames
#' @name tidy_model_expressions
#'
#' @param ... Currently ignored.
#' @param data A tidy data frame from regression model object (see
#'   [`statsExpressions::tidy_model_parameters()`]).
#' @param statistic Which statistic is to be displayed (either `"t"` or `"f"`or
#'   `"z"` or `"chi"`) in the expression.
#' @inheritParams oneway_anova
#'
#' @details
#' When any of the necessary numeric column values (`estimate`, `statistic`,
#' `p.value`) are missing, for these rows, a `NULL` is returned instead of an
#' expression with empty strings.
#'
#' @autoglobal
#'
#' @examplesIf identical(Sys.getenv("NOT_CRAN"), "true")
#' # setup
#' set.seed(123)
#' library(statsExpressions)
#'
#' # extract a tidy data frame
#' df <- tidy_model_parameters(lm(wt ~ am * cyl, mtcars))
#'
#' # create a column containing expression; the expression will depend on `statistic`
#' tidy_model_expressions(df, statistic = "t")
#' tidy_model_expressions(df, statistic = "z")
#' tidy_model_expressions(df, statistic = "chi")
#'
#' # f-statistic (requires a data frame with `df`, `df.error`, and effect-size columns)
#' df_f <- tidy_model_parameters(
#'   aov(wt ~ cyl, mtcars),
#'   es_type = "omega",
#'   table_wide = TRUE
#' )
#' tidy_model_expressions(df_f, statistic = "f")
#' df_f_eta <- tidy_model_parameters(
#'   aov(wt ~ cyl, mtcars),
#'   es_type = "eta",
#'   table_wide = TRUE
#' )
#' tidy_model_expressions(df_f_eta, statistic = "f", effsize.type = "eta")
#'
#' @template citation
#'
#' @export
tidy_model_expressions <- function(
  data,
  statistic = NULL,
  digits = 2L,
  effsize.type = "omega",
  ...
) {
  # standardize the statistic naming
  statistic <- substring(tolower(statistic), 1L, 1L)

  # if any of the necessary numeric columns are missing, there shouldn't be an
  # expression corresponding to that row; convert the necessary columns to
  # character type for expression
  df_expr <- data |>
    filter(if_all(
      .cols = matches("estimate|statistic|std.error|p.value"),
      .fns = Negate(is.na)
    )) |>
    .data_to_char(digits)

  stat_type <- statistic

  es.text <- if (stat_type == "f") {
    switch(
      effsize.type,
      eta = list(quote(widehat(italic(eta)[p]^2))),
      list(quote(widehat(italic(omega)[p]^2)))
    )
  } else {
    list(quote(widehat(italic(beta))))
  }

  # nolint start: line_length_linter.
  stat_part <- switch(
    stat_type,
    t = "italic(t)('{df.error}')=='{statistic}'",
    z = "italic(z)=='{statistic}'",
    c = "italic(chi)^2*('{df.error}')=='{statistic}'",
    f = "italic(F)('{df}', '{df.error}')=='{statistic}'"
  )
  expr_template <- paste0(
    "list({es.text}=='{estimate}', ",
    stat_part,
    ", italic(p)=='{p.value}')"
  )

  df_expr <- df_expr |>
    mutate(
      expression = if (stat_type == "t") {
        case_when(
          df.error %in% c("NA", "Inf") ~ glue(
            "list({es.text}=='{estimate}', italic(t)=='{statistic}', italic(p)=='{p.value}')"
          ),
          .default = glue(expr_template)
        )
      } else {
        glue(expr_template)
      }
    )
  # nolint end

  # Replace `NA` with `NULL` to show nothing instead of an empty string ("")
  left_join(data, select(df_expr, term, expression), by = "term") |>
    .glue_to_expression()
}


#' @keywords internal
#' @noRd
.glue_to_expression <- function(data) {
  data |>
    rowwise() |>
    mutate(expression = list(parse_expr(expression))) |>
    ungroup() |> # convert from `expression` to `language`
    mutate(
      expression = case_when(
        is.na(unlist(expression)) ~ list(NULL),
        .default = unlist(expression)
      )
    ) |>
    .add_package_class()
}

#' @keywords internal
#' @noRd
.add_package_class <- function(data) {
  data <- as_tibble(data)
  class(data) <- c("statsExpressions", class(data))
  data
}
