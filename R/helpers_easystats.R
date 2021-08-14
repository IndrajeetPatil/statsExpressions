#' @name tidy_model_parameters
#' @title Convert `parameters` package output to `tidyverse` conventions
#'
#' @inheritParams parameters::model_parameters
#'
#' @examples
#' model <- lm(mpg ~ wt + cyl, data = mtcars)
#' tidy_model_parameters(model)
#' @export

tidy_model_parameters <- function(model, ...) {
  stats_df <- parameters::model_parameters(model, verbose = FALSE, ...) %>%
    select(-matches("Difference")) %>%
    parameters::standardize_names(style = "broom") %>%
    rename_all(.funs = recode, "bayes.factor" = "bf10") %>%
    tidyr::fill(matches("^prior|^bf"), .direction = "updown") %>%
    mutate(across(matches("bf10"), ~ log(.x), .names = "log_e_{.col}"))

  # Bayesian ANOVA designs -----------------------------------

  if ("method" %in% names(stats_df)) {
    if (stats_df$method[[1]] == "Bayes factors for linear models") {
      # dataframe with posterior estimates for R-squared
      df_r2 <- performance::r2_bayes(model, average = TRUE, ci = stats_df$conf.level[[1]]) %>%
        as_tibble(.) %>%
        parameters::standardize_names(style = "broom") %>%
        rename_with(.fn = ~ paste0("r2.", .x), .cols = matches("^conf|^comp"))

      # for within-subjects design, retain only marginal component
      if ("r2.component" %in% names(df_r2)) df_r2 %<>% filter(r2.component == "conditional")

      # combine everything
      stats_df %<>% bind_cols(df_r2)
    }
  }

  as_tibble(stats_df)
}


#' @name tidy_model_effectsize
#' @title Convert `effectsize` package output to `tidyverse` conventions
#'
#' @param data Dataframe returned by `effectsize` functions.
#' @param ... Currently ignored.
#'
#' @examples
#' df <- effectsize::cohens_d(sleep$extra, sleep$group)
#' tidy_model_effectsize(df)
#' @noRd

tidy_model_effectsize <- function(data, ...) {
  bind_cols(
    data %>%
      mutate(effectsize = stats::na.omit(effectsize::get_effectsize_label(colnames(.)))) %>%
      parameters::standardize_names(style = "broom") %>%
      select(-contains("term")),
    rename_with(as_tibble(data %@% "ci_method"), ~ paste0("conf.", .x))
  )
}
