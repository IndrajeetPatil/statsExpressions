#' @name tidy_model_parameters
#' @title Convert `{parameters}` package output to `{tidyverse}` conventions
#'
#' @inheritParams parameters::model_parameters
#'
#' @autoglobal
#'
#' @examples
#' model <- lm(mpg ~ wt + cyl, data = mtcars)
#' tidy_model_parameters(model)
#'
#' @template citation
#'
#' @export
tidy_model_parameters <- function(model, ...) {
  stats_df <- model_parameters(model, verbose = FALSE, ...) %>%
    mutate(conf.method = . %@% "ci_method") %>%
    select(-matches("Difference")) %>%
    standardize_names(style = "broom") %>%
    rename_all(~ gsub("cramers.|omega2.|eta2.", "", .x)) %>%
    rename_with(recode, bayes.factor = "bf10") %>%
    tidyr::fill(matches("^prior|^bf"), .direction = "updown") %>%
    mutate(across(matches("bf10"), ~ log(.x), .names = "log_e_{.col}"))

  if (!"estimate" %in% colnames(stats_df)) stats_df %<>% select(-matches("^conf"))

  # Bayesian ANOVA designs -----------------------------------

  if ("method" %in% names(stats_df) && stats_df$method[[1]] == "Bayes factors for linear models") {
    # for within-subjects design, retain only marginal component
    df_r2 <- performance::r2_bayes(model, average = TRUE, verbose = FALSE, ci = stats_df$conf.level[[1]]) %>%
      as_tibble() %>%
      standardize_names(style = "broom") %>%
      rename(estimate = r.squared) %>%
      filter(if_any(matches("component"), ~ (.x == "conditional")))

    # remove estimates and CIs and use R2 data frame instead
    stats_df %<>%
      select(-matches("^est|^conf|^comp")) %>%
      filter(if_any(matches("effect"), ~ (.x == "fixed"))) %>%
      bind_cols(df_r2)
  }

  as_tibble(stats_df)
}


#' @name tidy_model_effectsize
#' @title Convert `{effectsize}` package output to `{tidyverse}` conventions
#'
#' @param data A data frame returned by `{effectsize}` functions.
#' @param ... Currently ignored.
#'
#' @autoglobal
#'
#' @examples
#' df <- effectsize::cohens_d(sleep$extra, sleep$group)
#' tidy_model_effectsize(df)
#' @noRd
tidy_model_effectsize <- function(data, ...) {
  data %>%
    mutate(effectsize = stats::na.omit(effectsize::get_effectsize_label(colnames(.)))) %>%
    standardize_names(style = "broom") %>%
    select(-contains("term")) %>%
    bind_cols(rename_with(as_tibble(data %@% "ci_method"), ~ paste0("conf.", .x)))
}
