#' @title Pairwise contingency table analyses
#' @name pairwise_contingency_table
#'
#' @description
#'
#' Pairwise Fisher's exact tests as post hoc tests for contingency table
#' analyses, with effect sizes (Cramer's *V*) and *p*-value adjustment for
#' multiple comparisons.
#'
#' @inheritParams contingency_table
#' @inheritParams pairwise_comparisons
#'
#' @section Pairwise contingency table tests:
#'
#' ```{r child="man/rmd-fragments/table_intro.Rmd"}
#' ```
#'
#' ```{r child="man/rmd-fragments/pairwise_contingency_table.Rmd"}
#' ```
#'
#' @returns
#'
#' ```{r child="man/rmd-fragments/return.Rmd"}
#' ```
#'
#' @autoglobal
#'
#' @examplesIf identical(Sys.getenv("NOT_CRAN"), "true")
#' # for reproducibility
#' set.seed(123)
#' library(statsExpressions)
#'
#' # pairwise Fisher's exact tests with Holm adjustment
#' pairwise_contingency_table(
#'   data = mtcars,
#'   x = cyl,
#'   y = am,
#'   p.adjust.method = "holm"
#' )
#'
#' # with counts data and Bonferroni adjustment
#' pairwise_contingency_table(
#'   data = as.data.frame(Titanic),
#'   x = Class,
#'   y = Survived,
#'   counts = Freq,
#'   p.adjust.method = "bonferroni"
#' )
#'
#' # no p-value adjustment
#' pairwise_contingency_table(
#'   data = mtcars,
#'   x = cyl,
#'   y = am,
#'   p.adjust.method = "none"
#' )
#'
#' @template citation
#'
#' @export
pairwise_contingency_table <- function(
  data,
  x,
  y,
  counts = NULL,
  p.adjust.method = "holm",
  digits = 2L,
  conf.level = 0.95,
  alternative = "two.sided",
  ...
) {
  # data -------------------------------------------

  x <- ensym(x)
  y <- ensym(y)

  data <- data |>
    select({{ x }}, {{ y }}, .counts = {{ counts }}) |>
    filter(!if_any(everything(), is.na))

  if (".counts" %in% names(data)) {
    data <- tidyr::uncount(data, weights = .counts)
  }

  data <- mutate(data, {{ x }} := droplevels(as.factor({{ x }})))

  # pairwise comparisons -------------------------------------------

  x_levels <- levels(pull(data, {{ x }}))
  pair_list <- utils::combn(x_levels, 2L, simplify = FALSE)

  df_pair <- map_dfr(pair_list, function(pair) {
    data_sub <- filter(data, {{ x }} %in% pair) |>
      mutate(across(where(is.factor), droplevels))

    xtab <- table(data_sub)

    fisher_result <- stats::fisher.test(xtab, alternative = alternative, ...)
    es_result <- effectsize::cramers_v(
      xtab,
      ci = conf.level,
      alternative = alternative
    )

    bind_cols(
      tibble(
        group1 = pair[[1L]],
        group2 = pair[[2L]],
        p.value = fisher_result$p.value
      ),
      tidy_model_effectsize(es_result)
    )
  })

  # p-value adjustment and expression -------------------------------------------

  df_pair <- df_pair |>
    arrange(group1, group2) |>
    .pairwise_p_adjust_expr(p.adjust.method, digits, "Fisher's exact test")

  select(
    df_pair,
    group1,
    group2,
    p.value,
    p.value.adj,
    everything(),
    -matches("^method$")
  ) |>
    .glue_to_expression()
}
