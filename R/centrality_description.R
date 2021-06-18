#' @title Dataframe and expression for distribution properties
#' @name centrality_description
#'
#' @details
#'
#' This function describes a distribution for `y` variable for each level of the
#' grouping variable in `x` by a set of indices (e.g., measures of centrality,
#' dispersion, range, skewness, kurtosis). It additionally returns an expression
#' containing a specified centrality measure. The function internally relies on
#' `parameters::describe_distribution` function.
#'
#' @param x The grouping (or independent) variable from the dataframe data.
#' @inheritParams oneway_anova
#' @param ... Currently ignored.
#'
#' @importFrom rlang !! enquo ensym exec
#' @importFrom parameters describe_distribution
#' @importFrom insight standardize_names format_value
#' @importFrom dplyr select group_by mutate rowwise group_modify arrange ungroup
#' @importFrom rlang !! enquo ensym :=
#' @importFrom tidyr drop_na
#'
#' @examples
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
#' @export

centrality_description <- function(data,
                                   x,
                                   y,
                                   type = "parametric",
                                   tr = 0.2,
                                   k = 2L,
                                   ...) {

  # ------------------------ measure -------------------------------------

  # standardize
  type <- stats_type_switch(type)

  # which centrality measure?
  centrality <- dplyr::case_when(
    type == "parametric" ~ "mean",
    type == "nonparametric" ~ "median",
    type == "robust" ~ "trimmed",
    type == "bayes" ~ "MAP"
  )

  # ------------------------ dataframe -------------------------------------

  # creating the dataframe
  dplyr::select(data, {{ x }}, {{ y }}) %>%
    tidyr::drop_na(.) %>%
    dplyr::mutate({{ x }} := droplevels(as.factor({{ x }}))) %>%
    dplyr::group_by({{ x }}) %>%
    dplyr::group_modify(
      .f = ~ parameters::standardize_names(
        data = parameters::describe_distribution(
          x = .,
          centrality = centrality,
          threshold = tr,
          # verbose = FALSE,
          ci = 0.95 # TODO: https://github.com/easystats/bayestestR/issues/429
        ),
        style = "broom"
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::rowwise() %>%
    dplyr::mutate(expression = paste0("list(~widehat(mu)[", centrality, "]=='", format_value(estimate, k), "')")) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(n_label = paste0({{ x }}, "\n(n = ", .prettyNum(n), ")")) %>%
    dplyr::arrange({{ x }}) %>%
    dplyr::select({{ x }}, !!as.character(rlang::ensym(y)) := estimate,
      n_obs = n, dplyr::everything()
    )
}
