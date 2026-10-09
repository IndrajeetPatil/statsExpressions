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
#' @examples
#' \dontshow{if (identical(Sys.getenv("NOT_CRAN"), "true")) withAutoprint(\{ # examplesIf}
#' @example man/examples/examples-two-sample-test.R
#' @examples
#' \dontshow{\}) # examplesIf}
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
  exact = FALSE,
  ...
) {
  # data -------------------------------------------

  type <- extract_stats_type(type)
  x <- ensym(x)
  y <- ensym(y)

  data <- long_to_wide_converter(
    data,
    x = {{ x }},
    y = {{ y }},
    subject.id = {{ subject.id }},
    paired = paired,
    spread = ifelse(type %in% c("bayes", "robust"), paired, TRUE)
  )

  # parametric & non-parametric ------------------------------------

  if (type == "parametric") {
    digits.df <- ifelse(paired || var.equal, 0L, digits)
  }

  if (type %in% c("parametric", "nonparametric")) {
    fns <- .mean_difference_fns(type, effsize.type)

    .f.args <- list(
      x = data[[2L]],
      y = data[[3L]],
      paired = paired,
      alternative = alternative
    )
    stats_df <- exec(
      fns$test,
      !!!.f.args,
      var.equal = var.equal,
      exact = exact
    ) |>
      tidy_model_parameters()
    ez_df <- exec(
      fns$es,
      !!!.f.args,
      pooled_sd = FALSE,
      ci = conf.level,
      verbose = FALSE
    ) |>
      tidy_model_effectsize()
  }

  # robust ---------------------------------------

  if (type == "robust") {
    digits.df <- ifelse(paired, 0L, digits)

    if (paired) {
      effect_model <- WRS2::dep.effect(
        x = data[[2L]],
        y = data[[3L]],
        tr = tr,
        nboot = nboot
      )
      test_model <- WRS2::yuend(x = data[[2L]], y = data[[3L]], tr = tr)
    } else {
      effect_model <- WRS2::akp.effect(
        formula = new_formula(y, x),
        data = data,
        EQVAR = FALSE,
        tr = tr,
        nboot = nboot,
        alpha = 1.0 - conf.level
      )
      test_model <- WRS2::yuen(new_formula(y, x), data, tr = tr)
    }

    ez_df <- tidy_model_parameters(effect_model, keep = "AKP")
    stats_df <- tidy_model_parameters(test_model)
  }

  if (type != "bayes") {
    stats_df <- bind_cols(
      select(stats_df, -matches("^est|^eff|conf|^ci")),
      select(ez_df, -matches("term"))
    ) |>
      .standardize_two_sample_terms(as_name(x), as_name(y))
  }

  # Bayesian ---------------------------------------

  if (type == "bayes") {
    # styler: off
    if (paired) {
      .f.args <- list(x = data[[2L]], y = data[[3L]], paired = paired)
    } else {
      .f.args <- list(
        formula = new_formula(y, x),
        data = as.data.frame(data),
        paired = paired
      )
    }
    # styler: on

    stats_df <- exec(BayesFactor::ttestBF, rscale = bf.prior, !!!.f.args) |>
      tidy_model_parameters(ci = conf.level)
  }

  # expression ---------------------------------------

  add_expression_col(
    data = stats_df,
    paired = paired,
    n = .n_obs(data, paired),
    digits = digits,
    digits.df = digits.df
  )
}

#' @noRd
.standardize_two_sample_terms <- function(data, x_name, y_name) {
  data |>
    mutate(
      across(matches("^parameter1$|^term$"), \(x) y_name),
      across(matches("^parameter2$|^group$"), \(x) x_name)
    )
}
