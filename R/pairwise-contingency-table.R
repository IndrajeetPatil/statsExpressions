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
#' @example man/examples/examples-pairwise-contingency-table.R
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

  c(x, y) %<-% c(ensym(x), ensym(y))

  data %<>%
    select({{ x }}, {{ y }}, .counts = {{ counts }}) %>%
    tidyr::drop_na()

  if (".counts" %in% names(data)) {
    data %<>% tidyr::uncount(weights = .counts)
  }

  data %<>% mutate({{ x }} := droplevels(as.factor({{ x }})))

  # pairwise comparisons -------------------------------------------

  x_levels <- levels(pull(data, {{ x }}))
  pair_list <- utils::combn(x_levels, 2L, simplify = FALSE)

  df_pair <- map_dfr(pair_list, function(pair) {
    data_sub <- filter(data, {{ x }} %in% pair) %>%
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

  df_pair %<>%
    arrange(group1, group2) %>%
    mutate(
      p.value.adj = stats::p.adjust(p = p.value, method = p.adjust.method),
      p.adjust.method = p_adjust_text(p.adjust.method),
      test = "Fisher's exact test",
      expression = case_when(
        p.adjust.method == "None" ~ glue(
          "list(italic(p)[unadj.]=='{format_value(p.value.adj, digits)}')"
        ),
        .default = glue(
          "list(italic(p)['{p.adjust.method}'-adj.]=='{format_value(p.value.adj, digits)}')"
        )
      )
    )

  select(
    df_pair,
    group1,
    group2,
    p.value,
    p.value.adj,
    everything(),
    -matches("^method$")
  ) %>%
    .glue_to_expression()
}
