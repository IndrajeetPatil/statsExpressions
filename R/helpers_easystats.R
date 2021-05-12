#' @name tidy_model_parameters
#' @title Convert `parameters` package output to `tidyverse` conventions
#'
#' @inheritParams parameters::model_parameters
#'
#' @importFrom parameters model_parameters standardize_names
#' @importFrom dplyr select matches rename_all recode contains
#' @importFrom tidyr fill
#'
#' @examples
#' model <- lm(mpg ~ wt + cyl, data = mtcars)
#' tidy_model_parameters(model)
#' @export

tidy_model_parameters <- function(model, ...) {
  parameters::model_parameters(model, verbose = FALSE, ...) %>%
    dplyr::select(-dplyr::matches("Difference")) %>%
    parameters::standardize_names(style = "broom") %>%
    dplyr::rename_all(.funs = dplyr::recode, "bayes.factor" = "bf10") %>%
    tidyr::fill(dplyr::matches("^prior|^bf"), .direction = "updown") %>%
    as_tibble(.)
}


#' @name tidy_model_effectsize
#' @title Convert `effectsize` package output to `tidyverse` conventions
#'
#' @param data Dataframe returned by `effectsize` functions.
#'
#' @importFrom effectsize get_effectsize_label
#' @importFrom purrr compose attr_getter
#' @importFrom dplyr select mutate contains rename_with
#'
#' @examples
#' df <- effectsize::cohens_d(sleep$extra, sleep$group)
#' tidy_model_effectsize(df)
#' @export

tidy_model_effectsize <- function(data) {
  dplyr::bind_cols(
    data %>%
      dplyr::mutate(effectsize = stats::na.omit(effectsize::get_effectsize_label(colnames(.)))[[1]]) %>%
      parameters::standardize_names(style = "broom") %>%
      dplyr::select(-dplyr::contains("term")) %>%
      as_tibble(.),
    dplyr::rename_with(get_ci_method(data), ~ paste0("conf.", .x))
  )
}

#' helper to get ci-related info stored as attributes in `effectsize` outputs
#' @noRd

get_ci_method <- purrr::compose(as_tibble, purrr::attr_getter("ci_method"))
