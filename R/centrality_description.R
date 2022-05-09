#' @title Dataframe and expression for distribution properties
#' @name centrality_description
#'
#' @details
#'
#' This function describes a distribution for `y` variable for each level of the
#' grouping variable in `x` by a set of indices (e.g., measures of centrality,
#' dispersion, range, skewness, kurtosis, etc.). It additionally returns an
#' expression containing a specified centrality measure. The function internally
#' relies on `datawizard::describe_distribution()` function.
#'
#' @description
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
#' @examples
#'
#' set.seed(123)
#'
#' # parametric -----------------------
#' centrality_description(iris, Species, Sepal.Length)
#'
#' # non-parametric -------------------
#' centrality_description(mtcars, am, wt, type = "n")
#'
#' # robust ---------------------------
#' centrality_description(ToothGrowth, supp, len, type = "r")
#'
#' # Bayesian -------------------------
#' centrality_description(sleep, group, extra, type = "b")
#'
#' @export
centrality_description <- function(data,
                                   x,
                                   y,
                                   type = "parametric",
                                   tr = 0.2,
                                   k = 2L,
                                   ...) {

  # measure -------------------------------------

  # standardize
  type <- stats_type_switch(type)

  # styler: off
  # which centrality measure?
  centrality <- case_when(
    type == "parametric"    ~ "mean",
    type == "nonparametric" ~ "median",
    type == "robust"        ~ "trimmed",
    type == "bayes"         ~ "MAP"
  )
  # styler: on

  # dataframe -------------------------------------

  select(data, {{ x }}, {{ y }}) %>%
    tidyr::drop_na(.) %>%
    group_by({{ x }}) %>%
    group_modify(
      .f = ~ standardize_names(
        data = datawizard::describe_distribution(
          x          = pull(., {{ y }}),
          centrality = centrality,
          threshold  = tr,
          verbose    = FALSE,
          ci         = 0.95 # TODO: https://github.com/easystats/bayestestR/issues/429
        ),
        style = "broom"
      )
    ) %>%
    ungroup() %>%
    mutate(
      expression = glue("list(widehat(mu)[{centrality}]=='{format_value(estimate, k)}')"),
      n.expression = paste0({{ x }}, "\n(n = ", .prettyNum(n.obs), ")")
    ) %>%
    arrange({{ x }}) %>%
    select({{ x }}, !!as.character(ensym(y)) := estimate, everything()) %>%
    .glue_to_expression()
}
