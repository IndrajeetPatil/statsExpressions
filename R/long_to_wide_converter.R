#' @title Convert long/tidy data frame to wide format with `NA`s removed
#' @name long_to_wide_converter
#'
#' @description
#'
#' This conversion is helpful mostly for repeated measures design, where
#' removing `NA`s by participant can be a bit tedious.
#'
#' It does not make sense to spread the data frame to wide format when the
#' measure is not repeated, so if `paired = TRUE`, `spread` argument will be
#' ignored.
#'
#' @param data A data frame (or a tibble) from which variables specified are to
#'   be taken. Other data types (e.g., matrix,table, array, etc.) will **not**
#'   be accepted. Additionally, grouped data frames from `{dplyr}` should be
#'   ungrouped before they are entered as `data`.
#' @param x The grouping (or independent) variable from `data`. In case of a
#'   repeated measures or within-subjects design, if `subject.id` argument is
#'   not available or not explicitly specified, the function assumes that the
#'   data has already been sorted by such an id by the user and creates an
#'   internal identifier. So if your data is **not** sorted, the results *can*
#'   be inaccurate when there are more than two levels in `x` and there are
#'   `NA`s present. The data is expected to be sorted by user in
#'   subject-1,subject-2, ..., pattern.
#' @param y The response (or outcome or dependent) variable from `data`.
#' @param subject.id Relevant in case of a repeated measures or within-subjects
#'   design (`paired = TRUE`, i.e.), it specifies the subject or repeated
#'   measures identifier. **Important**: Note that if this argument is `NULL`
#'   (which is the default), the function assumes that the data has already been
#'   sorted by such an id by the user and creates an internal identifier. So if
#'   your data is **not** sorted and you leave this argument unspecified, the
#'   results *can* be inaccurate when there are more than two levels in `x` and
#'   there are `NA`s present.
#' @param paired Logical that decides whether the experimental design is
#'   repeated measures/within-subjects or between-subjects. The default is
#'   `FALSE`.
#' @param spread Logical that decides whether the data frame needs to be
#'   converted from long/tidy to wide (default: `TRUE`). Relevant only if
#'   `paired = TRUE`.
#' @param ... Currently ignored.
#'
#' @return A data frame with `NA`s removed while respecting the
#'   between-or-within-subjects nature of the dataset.
#'
#' @examples
#' # for reproducibility
#' library(statsExpressions)
#' set.seed(123)
#'
#' # repeated measures design
#' long_to_wide_converter(
#'   data       = bugs_long,
#'   x          = condition,
#'   y          = desire,
#'   subject.id = subject,
#'   paired     = TRUE
#' )
#'
#' # independent measures design
#' long_to_wide_converter(
#'   data   = mtcars,
#'   x      = cyl,
#'   y      = wt,
#'   paired = FALSE
#' )
#' @export
long_to_wide_converter <- function(data,
                                   x,
                                   y,
                                   subject.id = NULL,
                                   paired = TRUE,
                                   spread = TRUE,
                                   ...) {
  data %<>%
    select({{ x }}, {{ y }}, .rowid = {{ subject.id }}) %>%
    mutate({{ x }} := droplevels(as.factor({{ x }}))) %>%
    arrange({{ x }})

  # if `subject.id` wasn't provided, create one for internal usage
  if (!".rowid" %in% names(data)) {
    # the row number needs to be assigned for each participant in paired data
    if (paired) data %<>% group_by({{ x }})

    # unique id for each participant
    data %<>% mutate(.rowid = row_number())
  }

  # NA removal
  data %<>%
    ungroup() %>%
    nest_by(.rowid, .key = "nested_data") %>%
    filter(sum(is.na(nested_data)) == 0L) %>%
    tidyr::unnest(cols = nested_data)

  # convert to wide?
  if (spread && paired) data %<>% tidyr::pivot_wider(names_from = {{ x }}, values_from = {{ y }})

  as_tibble(relocate(data, .rowid) %>% arrange(.rowid))
}
