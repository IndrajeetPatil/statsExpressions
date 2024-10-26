#' @title Data frame and expression for distribution properties
#' @name centrality_description
#'
#' @details
#'
#' This function describes a distribution for `y` variable for each level of the
#' grouping variable in `x` by a set of indices (e.g., measures of centrality,
#' dispersion, range, skewness, kurtosis, etc.). It additionally returns an
#' expression containing a specified centrality measure. The function internally
#' relies on [`datawizard::describe_distribution()`] function.
#'
#' @description
#' Parametric, non-parametric, robust, and Bayesian measures of centrality.
#'
#' @section Centrality measures:
#'
#' ```{r child="man/rmd-fragments/table_intro.Rmd"}
#' ```
#'
#' ```{r child="man/rmd-fragments/centrality_description.Rmd"}
#' ```
#'
#' @param x The grouping (or independent) variable in `data`.
#' @inheritParams oneway_anova
#' @param ... Currently ignored.
#'
#' @autoglobal
#'
#' @example man/examples/examples-centrality-description.R
#'
#' @template citation
#'
#' @export
centrality_description <- function(
  data,
  x,
  y,
  type = "parametric",
  conf.level = NULL,
  tr = 0.2,
  digits = 2L,
  ...
) {

  # styler: off
  centrality <- case_match(
    extract_stats_type(type),
    "parametric"    ~ "mean",
    "nonparametric" ~ "median",
    "robust"        ~ "trimmed",
    "bayes"         ~ "MAP"
  )
  # styler: on

  select(data, {{ x }}, {{ y }}) %>%
    tidyr::drop_na() %>%
    group_by({{ x }}) %>%
    group_modify(
      .f = ~ standardize_names(
        data = datawizard::describe_distribution(
          x          = pull(., {{ y }}),
          centrality = centrality,
          threshold  = tr,
          verbose    = FALSE,
          ci         = conf.level
        ),
        style = "broom"
      )
    ) %>%
    ungroup() %>%
    mutate(
      expression = glue("list(widehat(mu)[{centrality}]=='{format_value(estimate, digits)}')"),
      n.expression = paste0({{ x }}, "\n(n = ", .prettyNum(n.obs), ")")
    ) %>%
    arrange({{ x }}) %>%
    select({{ x }}, !!as.character(ensym(y)) := estimate, everything()) %>%
    .glue_to_expression()
}
