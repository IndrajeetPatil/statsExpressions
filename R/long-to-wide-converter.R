#' @title Convert long/tidy data frame to wide format
#' @name long_to_wide_converter
#'
#' @description
#'
#' This conversion is helpful mostly for repeated measures design, where
#' removing `NA`s by participant can be a bit tedious.
#'
#' @param data A data frame (or a tibble) from which variables specified are to
#'   be taken. Other data types (e.g., matrix, table, array, etc.) will **not**
#'   be accepted. Additionally, grouped data frames from `{dplyr}` should be
#'   ungrouped before they are entered as `data`.
#' @param x The grouping (or independent) variable from `data`. For repeated
#'   measures designs, see the note on ordering in `subject.id`.
#' @param y The response (or outcome or dependent) variable from `data`.
#' @param subject.id Relevant in case of a repeated measures or within-subjects
#'   design (i.e., `paired = TRUE`), it specifies the subject or repeated
#'   measures identifier. **Important**: If this argument is `NULL` (which is
#'   the default), observations are paired by their row order within each level
#'   of `x` (i.e., the data is assumed to be sorted in a subject-1, subject-2,
#'   ... pattern within every level). If the data is **not** sorted this way,
#'   the paired results will be silently incorrect, so it is safest to always
#'   specify `subject.id`. For paired designs, each subject must have at most
#'   one row per level of `x` (an error is thrown otherwise), and rows with a
#'   missing `subject.id` are dropped because they cannot be paired.
#' @param paired Logical that decides whether the experimental design is
#'   repeated measures/within-subjects or between-subjects. The default is
#'   `FALSE`.
#' @param spread Logical that decides whether the data frame needs to be
#'   converted from long/tidy to wide (default: `TRUE`).
#' @param ... Currently ignored.
#'
#' @returns A tibble with `NA`s removed while respecting the
#'   between-or-within-subjects nature of the dataset: for paired designs, only
#'   subjects observed in every level of `x` are kept (a subject with a missing
#'   value or a missing row in any condition is removed entirely), while for
#'   unpaired designs only the rows with missing values are removed. Rows are
#'   grouped by `subject.id` whenever it is supplied, so with `paired = FALSE`
#'   and a `subject.id`, a missing value still removes every row of that
#'   subject. The `.rowid` column contains the subject identifier (or an
#'   internal row identifier).
#'
#' @autoglobal
#'
#' @examples
#' # for reproducibility
#' library(statsExpressions)
#' set.seed(123)
#'
#' # repeated measures design
#' long_to_wide_converter(
#'   bugs_long,
#'   condition,
#'   desire,
#'   subject.id = subject,
#'   paired = TRUE
#' )
#'
#' # independent measures design
#' long_to_wide_converter(mtcars, cyl, wt, paired = FALSE)
#'
#' @template citation
#'
#' @export
long_to_wide_converter <- function(
  data,
  x,
  y,
  subject.id = NULL,
  paired = TRUE,
  spread = TRUE,
  ...
) {
  data <- data |>
    select({{ x }}, {{ y }}, .rowid = {{ subject.id }}) |>
    mutate({{ x }} := droplevels(as.factor({{ x }}))) |>
    arrange({{ x }})

  if (!".rowid" %in% names(data)) {
    data <- if (paired) {
      mutate(data, .rowid = row_number(), .by = {{ x }})
    } else {
      mutate(data, .rowid = row_number())
    }
  }

  data <- if (paired) {
    .complete_subjects(data, {{ x }}, {{ y }}, {{ subject.id }})
  } else {
    filter_out(data, anyNA(pick({{ x }}, {{ y }})), .by = .rowid)
  }

  if (spread) {
    data <- tidyr::pivot_wider(
      data,
      names_from = {{ x }},
      values_from = {{ y }}
    )
  }

  data |>
    relocate(.rowid) |>
    arrange(.rowid) |>
    as_tibble()
}

#' @title Keep subjects observed in every condition
#' @description Drops rows that cannot be paired (missing `x` or subject
#'   identifier), checks that each subject has at most one row per level of
#'   `x`, and keeps only subjects with a non-missing `y` in every level of `x`.
#' @param subject.id The subject identifier, used only in the error message for
#'   duplicated subjects (which cannot occur without it).
#' @noRd
.complete_subjects <- function(data, x, y, subject.id) {
  data <- data |>
    filter(!is.na(.rowid), !is.na({{ x }})) |>
    mutate({{ x }} := droplevels({{ x }}))

  if (anyDuplicated(select(data, {{ x }}, .rowid)) > 0L) {
    abort(c(
      glue(
        "Each `{as_name(enquo(subject.id))}` value must appear at most once ",
        "per level of `{as_name(enquo(x))}`."
      ),
      i = "Aggregate repeated observations within each condition first."
    ))
  }

  data |>
    filter(!is.na({{ y }})) |>
    filter(n() == nlevels({{ x }}), .by = .rowid)
}

#' @title Paired-aware observation count for the expression's `n`
#' @description Returns the number of unique subjects for repeated-measures
#'   designs (`paired = TRUE`) and the number of rows otherwise, operating on
#'   the `.rowid`-tagged data frame produced by [long_to_wide_converter()].
#'   Shared by [two_sample_test()] and [oneway_anova()].
#' @noRd
.n_obs <- function(data, paired) {
  if (paired) length(unique(data$.rowid)) else nrow(data)
}
