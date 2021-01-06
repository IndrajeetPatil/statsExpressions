
#' @name tidy_model_parameters
#' @title Convert `parameters` output to `tidymodels` convention
#'
#' @inheritParams parameters::model_parameters
#'
#' @importFrom parameters model_parameters
#' @importFrom insight standardize_names
#' @importFrom dplyr rename_all
#'
#' @examples
#' model <- lm(mpg ~ wt + cyl, data = mtcars)
#' tidy_model_parameters(model)
#' @export

tidy_model_parameters <- function(model, ...) {
  # extracting parameters
  df <- parameters::model_parameters(model, verbose = FALSE, ...)

  # special handling for t-test
  if ("Difference" %in% names(df)) df %<>% dplyr::select(-dplyr::matches("Diff|^CI"))

  # naming clean-up
  parameters::standardize_names(data = df, style = "broom") %>%
    dplyr::rename_all(~ gsub("omega2\\.|eta2\\.|cohens\\.|cramers\\.|d\\.|g\\.", "", .x)) %>%
    as_tibble(.)
}

#' @name tidy_model_performance
#' @title Convert `performance` output to `tidymodels` convention
#'
#' @inheritParams performance::model_performance
#'
#' @importFrom performance model_performance
#'
#' @examples
#' model <- lm(mpg ~ wt + cyl, data = mtcars)
#' tidy_model_parameters(model)
#' @export

tidy_model_performance <- function(model, ...) {
  performance::model_performance(model, verbose = FALSE, ...) %>%
    parameters::standardize_names(data = ., style = "broom") %>%
    as_tibble(.)
}
