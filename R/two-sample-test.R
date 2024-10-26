#' @title Two-sample tests
#' @name two_sample_test
#'
#' @description
#' Parametric, non-parametric, robust, and Bayesian two-sample tests.
#'
#' @inheritParams long_to_wide_converter
#' @inheritParams extract_stats_type
#' @inheritParams one_sample_test
#' @inheritParams oneway_anova
#' @inheritParams stats::t.test
#' @inheritParams add_expression_col
#'
#' @section Two-sample tests:
#'
#' ```{r child="man/rmd-fragments/table_intro.Rmd"}
#' ```
#'
#' ```{r child="man/rmd-fragments/two_sample_test.Rmd"}
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
#' @example man/examples/examples-two-sample-test-within.R
#' @example man/examples/examples-two-sample-test-between.R
#'
#' @template citation
#'
#' @export
two_sample_test <- function(
  data,
  x,
  y,
  subject.id = NULL,
  type = "parametric",
  paired = FALSE,
  alternative = "two.sided",
  digits = 2L,
  conf.level = 0.95,
  effsize.type = "g",
  var.equal = FALSE,
  bf.prior = 0.707,
  tr = 0.2,
  nboot = 100L,
  ...
) {
  # data -------------------------------------------

  type <- extract_stats_type(type)
  c(x, y) %<-% c(ensym(x), ensym(y))

  data %<>% long_to_wide_converter(
    x          = {{ x }},
    y          = {{ y }},
    subject.id = {{ subject.id }},
    paired     = paired,
    spread     = ifelse(type %in% c("bayes", "robust"), paired, TRUE)
  )

  # parametric ---------------------------------------

  if (type == "parametric") {
    # styler: off
    digits.df <- ifelse(paired || var.equal, 0L, digits)
    .f   <- stats::t.test
    if (effsize.type %in% c("unbiased", "g")) .f.es <- effectsize::hedges_g
    if (effsize.type %in% c("biased", "d")) .f.es   <- effectsize::cohens_d
    # styler: on
  }

  # non-parametric ------------------------------------

  if (type == "nonparametric") c(.f, .f.es) %<-% c(stats::wilcox.test, effectsize::rank_biserial)

  if (type %in% c("parametric", "nonparametric")) {
    .f.args <- list(x = data[[2L]], y = data[[3L]], paired = paired, alternative = alternative)
    stats_df <- exec(.f, !!!.f.args, var.equal = var.equal, exact = FALSE) %>% tidy_model_parameters()
    ez_df <- exec(.f.es, !!!.f.args, pooled_sd = FALSE, ci = conf.level, verbose = FALSE) %>% tidy_model_effectsize()
  }

  # robust ---------------------------------------

  if (type == "robust") {
    digits.df <- ifelse(paired, 0L, digits)

    # styler: off
    if (!paired) c(.f, .f.es) %<-% c(WRS2::yuen, WRS2::akp.effect)
    if (paired) c(.f, .f.es)  %<-% c(WRS2::yuend, WRS2::dep.effect)

    .f.args    <- list(formula = new_formula(y, x), data = data, x = data[[2L]], y = data[[3L]])
    .f.es.args <- list(EQVAR = FALSE, nboot = nboot, alpha = 1.0 - conf.level, tr = tr)

    ez_df    <- tidy_model_parameters(exec(.f.es, !!!.f.args, !!!.f.es.args), keep = "AKP")
    stats_df <- tidy_model_parameters(exec(.f, !!!.f.args, !!!.f.es.args))
    # styler: on
  }

  if (type != "bayes") {
    stats_df <- bind_cols(select(stats_df, -matches("^est|^eff|conf|^ci")), select(ez_df, -matches("term")))
  }

  # Bayesian ---------------------------------------

  if (type == "bayes") {
    # styler: off
    if (!paired) .f.args <- list(formula = new_formula(y, x), data = as.data.frame(data), paired = paired)
    if (paired) .f.args  <- list(x = data[[2L]], y = data[[3L]], paired = paired)
    # styler: on

    stats_df <- exec(BayesFactor::ttestBF, rscale = bf.prior, !!!.f.args) %>% tidy_model_parameters(ci = conf.level)
  }

  # expression ---------------------------------------

  add_expression_col(
    data = if (type == "bayes") stats_df else .standardize_two_sample_terms(stats_df, as_name(x), as_name(y)),
    paired = paired,
    n = ifelse(paired, length(unique(data$.rowid)), nrow(data)),
    digits = digits,
    digits.df = digits.df
  )
}

#' @noRd
.standardize_two_sample_terms <- function(data, x_name, y_name) {
  data %>%
    mutate(
      across(matches("^parameter1$|^term$"), ~y_name),
      across(matches("^parameter2$|^group$"), ~x_name)
    )
}
