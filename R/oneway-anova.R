#' @title One-way analysis of variance (ANOVA)
#' @name oneway_anova
#'
#' @description
#' Parametric, non-parametric, robust, and Bayesian one-way ANOVA.
#'
#' @section One-way ANOVA:
#'
#' ```{r child="man/rmd-fragments/table_intro.Rmd"}
#' ```
#'
#' ```{r child="man/rmd-fragments/oneway_anova.Rmd"}
#' ```
#'
#' @returns
#'
#' ```{r child="man/rmd-fragments/return.Rmd"}
#' ```
#'
#' @inheritParams long_to_wide_converter
#' @inheritParams extract_stats_type
#' @param conf.level Scalar between `0` and `1` (default: `95%`
#' confidence/credible intervals, `0.95`). If `NULL`, no confidence intervals
#' will be computed.
#' @param effsize.type Type of effect size needed for *parametric* tests. The
#'   argument can be `"eta"` (partial eta-squared) or `"omega"` (partial
#'   omega-squared).
#' @param tr Trim level for the mean when carrying out `robust` tests. In case
#'   of an error, try reducing the value of `tr`, which is by default set to
#'   `0.2`. Lowering the value might help.
#' @param nboot Number of bootstrap samples for computing confidence interval
#'   for the effect size (Default: `100L`).
#' @param bf.prior A number between `0.5` and `2` (default `0.707`), the prior
#'   width to use in calculating Bayes factors and posterior estimates. In
#'   addition to numeric arguments, several named values are also recognized:
#'   `"medium"`, `"wide"`, and `"ultrawide"`, corresponding to *r* scale values
#'   of 1/2, sqrt(2)/2, and 1, respectively. In case of an ANOVA, this value
#'   corresponds to scale for fixed effects.
#' @inheritParams two_sample_test
#' @inheritParams add_expression_col
#' @param ... Additional arguments (currently ignored).
#' @inheritParams stats::oneway.test
#'
#' @autoglobal
#'
#' @examplesIf identical(Sys.getenv("NOT_CRAN"), "true") && getRversion() >= "4.4.0"
#' # for reproducibility
#' set.seed(123)
#' library(statsExpressions)
#'
#' # ----------------------- parametric -------------------------------------
#'
#' # between-subjects
#' oneway_anova(
#'   data = mtcars,
#'   x    = cyl,
#'   y    = wt
#' )
#'
#' # within-subjects design
#' oneway_anova(
#'   data       = iris_long,
#'   x          = condition,
#'   y          = value,
#'   subject.id = id,
#'   paired     = TRUE
#' )
#'
#' # ----------------------- non-parametric ----------------------------------
#'
#' # between-subjects
#' oneway_anova(
#'   data = mtcars,
#'   x    = cyl,
#'   y    = wt,
#'   type = "np"
#' )
#'
#' # within-subjects design
#' oneway_anova(
#'   data       = iris_long,
#'   x          = condition,
#'   y          = value,
#'   subject.id = id,
#'   paired     = TRUE,
#'   type       = "np"
#' )
#'
#' # ----------------------- robust -------------------------------------
#'
#' # between-subjects
#' oneway_anova(
#'   data = mtcars,
#'   x    = cyl,
#'   y    = wt,
#'   type = "r"
#' )
#'
#' # within-subjects design
#' oneway_anova(
#'   data       = iris_long,
#'   x          = condition,
#'   y          = value,
#'   subject.id = id,
#'   paired     = TRUE,
#'   type       = "r"
#' )
#'
#' @examplesIf identical(Sys.getenv("NOT_CRAN"), "true") && requireNamespace("rstantools") && getRversion() >= "4.4.0"
#'
#' # ----------------------- Bayesian -------------------------------------
#'
#' # between-subjects
#' oneway_anova(
#'   data = mtcars,
#'   x    = cyl,
#'   y    = wt,
#'   type = "bayes"
#' )
#'
#' # within-subjects design
#' oneway_anova(
#'   data       = iris_long,
#'   x          = condition,
#'   y          = value,
#'   subject.id = id,
#'   paired     = TRUE,
#'   type       = "bayes"
#' )
#'
#' @template citation
#'
#' @export
oneway_anova <- function(
    data,
    x,
    y,
    subject.id = NULL,
    type = "parametric",
    paired = FALSE,
    digits = 2L,
    conf.level = 0.95,
    effsize.type = "omega",
    var.equal = FALSE,
    bf.prior = 0.707,
    tr = 0.2,
    nboot = 100L,
    ...) {
  # data -------------------------------------------

  type <- extract_stats_type(type)
  c(x, y) %<-% c(ensym(x), ensym(y))

  data %<>% long_to_wide_converter(
    x          = {{ x }},
    y          = {{ y }},
    subject.id = {{ subject.id }},
    paired     = paired,
    spread     = FALSE
  ) %>%
    mutate(.rowid = as.factor(.rowid))

  #  parametric ---------------------------------------

  if (type == "parametric") {
    c(digits.df, digits.df.error) %<-% c(ifelse(paired, digits, 0L), ifelse(!paired && var.equal, 0L, digits))

    # styler: off
    if (effsize.type %in% c("unbiased", "omega")) .f.es <- effectsize::omega_squared
    if (effsize.type %in% c("biased", "eta"))     .f.es <- effectsize::eta_squared
    # styler: on

    if (paired) {
      mod <- afex::aov_ez(
        id          = ".rowid",
        dv          = as_string(y),
        data        = data,
        within      = as_string(x),
        include_aov = TRUE
      )
    }

    if (!paired) mod <- stats::oneway.test(new_formula(y, x), data, var.equal = var.equal)

    stats_df <- bind_cols(
      tidy_model_parameters(mod),
      exec(.f.es, model = mod, ci = conf.level, verbose = FALSE) %>% tidy_model_effectsize()
    )
  }

  # non-parametric ------------------------------------

  if (type == "nonparametric") {
    c(digits.df, digits.df.error) %<-% c(0L, 0L)

    # styler: off
    if (paired) {
      c(.f, .f.es) %<-% c(stats::friedman.test, effectsize::kendalls_w)
      .f.args       <- list(formula = new_formula({{ enexpr(y) }}, expr(!!enexpr(x) | .rowid)))
      .f.es.args    <- list(x = new_formula({{ enexpr(y) }}, expr(!!enexpr(x) | .rowid)))
    }

    if (!paired) {
      c(.f, .f.es) %<-% c(stats::kruskal.test, effectsize::rank_epsilon_squared)
      .f.args       <- list(formula = new_formula(y, x))
      .f.es.args    <- list(x = new_formula(y, x))
    }
    # styler: on

    stats_df <- tidy_model_parameters(exec(.f, !!!.f.args, data = data))

    ez_df <- exec(
      .fn        = .f.es,
      data       = data,
      ci         = conf.level,
      iterations = nboot,
      verbose    = FALSE,
      !!!.f.es.args
    ) %>%
      tidy_model_effectsize()

    stats_df <- bind_cols(stats_df, ez_df)
  }

  # robust ---------------------------------------

  if (type == "robust") {
    c(digits.df, digits.df.error) %<-% c(ifelse(paired, digits, 0L), digits)

    if (paired) {
      mod <- WRS2::rmanova(
        y      = data[[as_name(y)]],
        groups = data[[as_name(x)]],
        blocks = data[[".rowid"]],
        tr     = tr
      )
    }

    if (!paired) {
      mod <- WRS2::t1way(
        formula = new_formula(y, x),
        data    = data,
        tr      = tr,
        alpha   = 1.0 - conf.level,
        nboot   = nboot
      )
    }

    stats_df <- tidy_model_parameters(mod)

    if (paired) {
      ez_df <- long_to_wide_converter(data, {{ x }}, {{ y }}) %>%
        WRS2::wmcpAKP(select(-.rowid), tr = tr, nboot = nboot) %>%
        tidy_model_parameters()

      stats_df <- bind_cols(stats_df, ez_df)
    }
  }

  # Bayesian ---------------------------------------

  if (type == "bayes") {
    if (!paired) .f.args <- list(formula = new_formula(y, x), rscaleFixed = bf.prior)
    if (paired) {
      .f.args <- list(
        formula      = new_formula(enexpr(y), expr(!!enexpr(x) + .rowid)),
        rscaleFixed  = bf.prior,
        whichRandom  = ".rowid",
        rscaleRandom = 1.0
      )
    }

    stats_df <- exec(BayesFactor::anovaBF, data = as.data.frame(data), progress = FALSE, !!!.f.args) %>%
      tidy_model_parameters(ci = conf.level)
  }

  # expression ---------------------------------------

  add_expression_col(
    data = stats_df,
    n = ifelse(paired, length(unique(data$.rowid)), nrow(data)),
    paired = paired,
    digits = digits,
    digits.df = digits.df,
    digits.df.error = digits.df.error
  )
}
