#' @title Contingency table analyses
#' @name contingency_table
#'
#' @description
#' Parametric and Bayesian one-way and two-way contingency table analyses.
#'
#' @section Contingency table analyses:
#'
#' ```{r child="man/rmd-fragments/table_intro.Rmd"}
#' ```
#'
#' ```{r child="man/rmd-fragments/contingency_table.Rmd"}
#' ```
#'
#' @returns
#'
#' ```{r child="man/rmd-fragments/return.Rmd"}
#' ```
#'
#' @param x The variable to use as the **rows** in the contingency table.
#' @param y The variable to use as the **columns** in the contingency table.
#'   Default is `NULL`. If `NULL`, one-sample proportion test (a goodness of fit
#'   test) will be run for the `x` variable.
#' @param counts The variable in data containing counts, or `NULL` if each row
#'   represents a single observation.
#' @param paired Logical indicating whether data came from a within-subjects or
#'   repeated measures design study (Default: `FALSE`). Only relevant for
#'   two-way tables (i.e., when `y` is supplied), and paired designs are only
#'   supported for frequentist tests (McNemar's test). For a two-way table with
#'   `type = "bayes"`, `paired` is ignored and the Bayesian test of
#'   independence for unpaired data is run instead. For one-way tables
#'   (`y = NULL`), `paired` has no effect.
#' @param sampling.plan Character describing the sampling plan. Possible options:
#'   - `"indepMulti"` (independent multinomial; default)
#'   - `"poisson"`
#'   - `"jointMulti"` (joint multinomial)
#'   - `"hypergeom"` (hypergeometric).
#'
#'   Only used for Bayesian two-way tables. For more, see
#'   [`BayesFactor::contingencyTableBF()`].
#' @param fixed.margin For the independent multinomial sampling plan, which
#'   margin is fixed (`"rows"` or `"cols"`). Defaults to `"rows"`. Only used
#'   for Bayesian two-way tables.
#' @param prior.concentration Specifies the prior concentration parameter, set
#'   to `1` by default. It indexes the expected deviation from the null
#'   hypothesis under the alternative, and corresponds to Gunel and Dickey's
#'   (1974) `"a"` parameter. Only used for Bayesian analyses.
#' @param ratio A vector of proportions: the expected proportions for the
#'   proportion test (should sum to `1`). Default is `NULL`, which means the null
#'   is equal theoretical proportions across the levels of the nominal variable.
#'   E.g., `ratio = c(0.5, 0.5)` for two levels,
#'   `ratio = c(0.25, 0.25, 0.25, 0.25)` for four levels, etc.
#' @param ... Additional arguments (currently ignored).
#' @inheritParams stats::chisq.test
#' @inheritParams oneway_anova
#' @inheritParams parameters::model_parameters.htest
#'
#' @autoglobal
#'
#' @examples
#' \dontshow{if (identical(Sys.getenv("NOT_CRAN"), "true")) withAutoprint(\{ # examplesIf}
#' @example man/examples/examples-contingency-table.R
#' @examples
#' \dontshow{\}) # examplesIf}
#'
#' @export
contingency_table <- function(
  data,
  x,
  y = NULL,
  paired = FALSE,
  type = "parametric",
  counts = NULL,
  ratio = NULL,
  alternative = "two.sided",
  digits = 2L,
  conf.level = 0.95,
  sampling.plan = "indepMulti",
  fixed.margin = "rows",
  prior.concentration = 1.0,
  ...
) {
  # data -------------------------------------------

  type <- extract_stats_type(type)
  test <- ifelse(quo_is_null(enquo(y)), "1way", "2way")

  data <- .untable_by_counts(data, {{ x }}, {{ y }}, {{ counts }})

  xtab <- table(data)
  ratio <- ratio %||% rep(1 / length(xtab), length(xtab))

  # non-Bayesian ---------------------------------------

  if (type != "bayes" && test == "2way") {
    if (paired) {
      .f <- stats::mcnemar.test
      .f.es <- effectsize::cohens_g
    } else {
      .f <- stats::chisq.test
      .f.es <- effectsize::cramers_v
    }
    .f.args <- list(x = xtab, correct = FALSE)
  }

  if (type != "bayes" && test == "1way") {
    .f <- stats::chisq.test
    .f.es <- effectsize::pearsons_c
    .f.args <- list(x = xtab, p = ratio, correct = FALSE)
  }

  if (type != "bayes") {
    stats_df <- bind_cols(
      tidy_model_parameters(exec(.f, !!!.f.args)),
      tidy_model_effectsize(exec(
        .f.es,
        !!!.f.args,
        ci = conf.level,
        alternative = alternative
      ))
    )
  }

  # Bayesian ---------------------------------------

  if (type == "bayes" && test == "2way") {
    stats_df <- BayesFactor::contingencyTableBF(
      xtab,
      sampleType = sampling.plan,
      fixedMargin = fixed.margin,
      priorConcentration = prior.concentration
    ) |>
      tidy_model_parameters(
        ci = conf.level,
        es_type = "cramers_v",
        alternative = alternative
      )
  }

  if (type == "bayes" && test == "1way") {
    return(.one_way_bayesian_table(xtab, prior.concentration, ratio, digits))
  }

  add_expression_col(stats_df, paired = paired, n = nrow(data), digits = digits)
}

.one_way_bayesian_table <- function(xtab, prior.concentration, ratio, digits) {
  # nocov start
  # a one-cell table is a degenerate case for this computation
  if (length(as.vector(xtab)) == 1L) {
    return(NULL)
  }
  # nocov end

  p1s <- rdirichlet(n = 100000L, alpha = prior.concentration * ratio)
  pr_h1 <- map_dbl(
    1:100000L,
    ~ stats::dmultinom(as.matrix(xtab), prob = p1s[.x, ], log = TRUE)
  )

  # BF = (log) prob of data under alternative - (log) prob of data under null
  # computing Bayes Factor and formatting the results
  tibble(
    bf10 = exp(
      BayesFactor::logMeanExpLogs(pr_h1) -
        stats::dmultinom(as.matrix(xtab), NULL, ratio, TRUE)
    ),
    prior.scale = prior.concentration,
    method = "Bayesian one-way contingency table analysis"
  ) |>
    mutate(
      expression = glue(
        "list(
            log[e]*(BF['01'])=='{format_value(-log(bf10), digits)}',
            {prior_switch(method)}=='{format_value(prior.scale, digits)}')"
      )
    ) |>
    .glue_to_expression()
}

#' @title Select response and (optional) counts columns, then untable
#' @description Selects the grouping/response (and optional counts) columns,
#'   drops rows with `NA` in them and, when a counts column is present, expands
#'   the frequency-weighted rows into one row per observation. Shared by
#'   [contingency_table()] and [pairwise_contingency_table()].
#' @autoglobal
#' @noRd
.untable_by_counts <- function(data, x, y, counts) {
  data <- data |>
    select({{ x }}, {{ y }}, .counts = {{ counts }}) |>
    tidyr::drop_na()

  # untable the data frame based on the counts for each observation (if present)
  if (".counts" %in% names(data)) {
    data <- tidyr::uncount(data, weights = .counts)
  }

  data
}

#' @title Random draws from a Dirichlet distribution
#' @note `rdirichlet()` function from `{MCMCpack}`
#' @noRd
rdirichlet <- function(n, alpha) {
  l <- length(alpha)
  x <- matrix(stats::rgamma(l * n, alpha), ncol = l, byrow = TRUE)
  x / as.vector(x %*% rep(1, l))
}
