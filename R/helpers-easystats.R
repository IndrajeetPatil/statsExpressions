#' @name tidy_model_parameters
#' @title Convert `{parameters}` package output to `{tidyverse}` conventions
#'
#' @description
#' Runs [`parameters::model_parameters()`] on a model object and standardizes
#' the result to `{broom}`-style column names (e.g. `estimate`, `std.error`,
#' `conf.low`, `conf.high`, `statistic`, `df.error`, `p.value`). Bayes factors
#' are returned in a `bf10` column, along with their natural logarithm in
#' `log_e_bf10`. For Bayesian ANOVA designs (`{BayesFactor}` linear models),
#' the estimate is replaced by the Bayesian *R*-squared.
#'
#' @inheritParams parameters::model_parameters
#'
#' @returns A tibble with one row per parameter.
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
  params <- model_parameters(model, verbose = FALSE, ...)
  stats_df <- params |>
    mutate(conf.method = attr(params, "ci_method")) |>
    select(-matches("Difference")) |>
    standardize_names(style = "broom") |>
    rename_with(\(x) gsub("cramers.|omega2.|eta2.", "", x)) |>
    rename(any_of(c(bf10 = "bayes.factor"))) |>
    tidyr::fill(matches("^prior|^bf"), .direction = "updown") |>
    mutate(across(matches("bf10"), log, .names = "log_e_{.col}"))

  if (!"estimate" %in% colnames(stats_df)) {
    stats_df <- select(stats_df, -matches("^conf"))
  }

  # Bayesian ANOVA designs -----------------------------------

  if (
    "method" %in%
      names(stats_df) &&
      stats_df$method[[1]] == "Bayes factors for linear models"
  ) {
    # for within-subjects design, retain only conditional component
    df_r2 <- performance::r2_bayes(
      model,
      average = TRUE,
      verbose = FALSE,
      ci = stats_df$conf.level[[1]]
    ) |>
      as_tibble() |>
      standardize_names(style = "broom") |>
      rename(estimate = r.squared) |>
      filter(if_all(matches("component"), \(x) x == "conditional"))

    # remove estimates and CIs and use R2 data frame instead
    stats_df <- stats_df |>
      select(-matches("^est|^conf|^comp")) |>
      filter(if_all(matches("effect"), \(x) x == "fixed"))

    # replicate df_r2 to match stats_df rows for bind_cols
    df_r2 <- df_r2[rep(1L, nrow(stats_df)), ]

    stats_df <- bind_cols(stats_df, df_r2)
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
  effectsize_labels <- effectsize::get_effectsize_label(colnames(data))
  ci_method <- rename_with(
    as_tibble(attr(data, "ci_method")),
    \(x) paste0("conf.", x)
  )

  data |>
    mutate(effectsize = stats::na.omit(effectsize_labels)) |>
    standardize_names(style = "broom") |>
    select(-contains("term")) |>
    bind_cols(ci_method)
}
