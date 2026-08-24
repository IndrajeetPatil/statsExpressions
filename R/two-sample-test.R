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
#' # ----------------------- within-subjects -------------------------------------
#'
#' # data
#' df <- dplyr::filter(bugs_long, condition %in% c("LDLF", "LDHF"))
#'
#' # for reproducibility
#' set.seed(123)
#'
#' # ----------------------- parametric ---------------------------------------
#'
#' two_sample_test(
#'   df,
#'   condition,
#'   desire,
#'   subject.id = subject,
#'   paired = TRUE,
#'   type = "parametric"
#' )
#'
#' # ----------------------- non-parametric -----------------------------------
#'
#' two_sample_test(
#'   df,
#'   condition,
#'   desire,
#'   subject.id = subject,
#'   paired = TRUE,
#'   type = "nonparametric"
#' )
#'
#' # ----------------------- robust --------------------------------------------
#'
#' two_sample_test(
#'   df,
#'   condition,
#'   desire,
#'   subject.id = subject,
#'   paired = TRUE,
#'   type = "robust"
#' )
#'
#' # ----------------------- Bayesian ---------------------------------------
#'
#' two_sample_test(
#'   df,
#'   condition,
#'   desire,
#'   subject.id = subject,
#'   paired = TRUE,
#'   type = "bayes"
#' )
#'
#' # ----------------------- between-subjects -------------------------------------
#'
#' # for reproducibility
#' set.seed(123)
#'
#' # ----------------------- parametric ---------------------------------------
#'
#' # unequal variance
#' two_sample_test(ToothGrowth, supp, len, type = "parametric")
#'
#' # equal variance
#' two_sample_test(ToothGrowth, supp, len, type = "parametric", var.equal = TRUE)
#'
#' # biased (Cohen's d) effect size
#' two_sample_test(ToothGrowth, supp, len, type = "parametric", effsize.type = "d")
#'
#' # ----------------------- non-parametric -----------------------------------
#'
#' two_sample_test(ToothGrowth, supp, len, type = "nonparametric")
#'
#' # ----------------------- robust --------------------------------------------
#'
#' two_sample_test(ToothGrowth, supp, len, type = "robust")
#'
#' # ----------------------- Bayesian ---------------------------------------
#'
#' two_sample_test(ToothGrowth, supp, len, type = "bayes")
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
    .f <- fns$test
    .f.es <- fns$es

    .f.args <- list(
      x = data[[2L]],
      y = data[[3L]],
      paired = paired,
      alternative = alternative
    )
    stats_df <- exec(.f, !!!.f.args, var.equal = var.equal, exact = exact) |>
      tidy_model_parameters()
    ez_df <- exec(
      .f.es,
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
    )
  }

  # Bayesian ---------------------------------------

  if (type == "bayes") {
    # styler: off
    if (!paired) {
      .f.args <- list(
        formula = new_formula(y, x),
        data = as.data.frame(data),
        paired = paired
      )
    }
    if (paired) {
      .f.args <- list(x = data[[2L]], y = data[[3L]], paired = paired)
    }
    # styler: on

    stats_df <- exec(BayesFactor::ttestBF, rscale = bf.prior, !!!.f.args) |>
      tidy_model_parameters(ci = conf.level)
  }

  # expression ---------------------------------------

  add_expression_col(
    data = if (type == "bayes") {
      stats_df
    } else {
      .standardize_two_sample_terms(stats_df, as_name(x), as_name(y))
    },
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
