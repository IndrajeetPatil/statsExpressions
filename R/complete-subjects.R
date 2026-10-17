#' @title Keep complete subjects in repeated measures data
#' @name complete_subjects
#'
#' @description
#'
#' Prepares long/tidy repeated measures data for analysis by keeping only the
#' subjects observed in every level of `x`. This is the listwise deletion step
#' that [long_to_wide_converter()] applies when `paired = TRUE`, and thus the
#' data that [two_sample_test()], [oneway_anova()], and
#' [pairwise_comparisons()] analyze for within-subjects designs.
#'
#' The following steps are carried out:
#'
#' - Rows with a missing `x` or `subject.id` are dropped, since they cannot be
#'   paired.
#' - An error is thrown if any subject has more than one row in a level of `x`.
#' - Subjects with a missing `y` value, or without a row, in any level of `x`
#'   are removed entirely.
#'
#' @inheritParams long_to_wide_converter
#'
#' @returns A tibble in long format with columns `.rowid` (the subject
#'   identifier, or an internal row identifier when `subject.id` is `NULL`),
#'   `x` (as a factor with unused levels dropped), and `y`, sorted by `.rowid`.
#'
#' @autoglobal
#'
#' @examples
#' # subjects with a missing rating in any condition are removed
#' complete_subjects(bugs_long, condition, desire, subject.id = subject)
#'
#' @template citation
#'
#' @export
complete_subjects <- function(data, x, y, subject.id = NULL) {
  data <- data |>
    select({{ x }}, {{ y }}, .rowid = {{ subject.id }}) |>
    filter(!is.na({{ x }})) |>
    mutate({{ x }} := droplevels(as.factor({{ x }}))) |>
    arrange({{ x }})

  if (!".rowid" %in% names(data)) {
    data <- mutate(data, .rowid = row_number(), .by = {{ x }})
  }

  data <- data |>
    filter(!is.na(.rowid)) |>
    mutate({{ x }} := droplevels({{ x }}))

  if (anyDuplicated(select(data, {{ x }}, .rowid)) > 0L) {
    # nocov start
    # duplicated subjects are an input error, so examples never reach this
    abort(c(
      glue(
        "Each `{as_name(enquo(subject.id))}` value must appear at most once ",
        "per level of `{as_name(enquo(x))}`."
      ),
      i = "Aggregate repeated observations within each condition first."
    ))
    # nocov end
  }

  data |>
    filter(!is.na({{ y }})) |>
    filter(n() == nlevels({{ x }}), .by = .rowid) |>
    relocate(.rowid) |>
    arrange(.rowid) |>
    as_tibble()
}
