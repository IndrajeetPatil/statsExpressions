#' @title Two-sample tests
#' @name two_sample_test
#'
#' @description
#' Parametric, non-parametric, robust, and Bayesian two-sample tests.
#'
#' @inheritParams long_to_wide_converter
#' @inheritParams stats_type_switch
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
#' @return
#'
#' ```{r child="man/rmd-fragments/return.Rmd"}
#' ```
#'
#' @examplesIf identical(Sys.getenv("NOT_CRAN"), "true")
#' # for reproducibility
#' set.seed(123)
#' library(statsExpressions)
#'
#' # parametric -------------------------------------
#'
#' # between-subjects design
#' two_sample_test(
#'   data = sleep,
#'   x    = group,
#'   y    = extra,
#'   type = "p"
#' )
#'
#' # within-subjects design
#' two_sample_test(
#'   data       = dplyr::filter(bugs_long, condition %in% c("HDHF", "HDLF")),
#'   x          = condition,
#'   y          = desire,
#'   paired     = TRUE,
#'   subject.id = subject,
#'   type       = "p"
#' )
#'
#' # non-parametric ----------------------------------
#'
#' # between-subjects design
#' two_sample_test(
#'   data = sleep,
#'   x    = group,
#'   y    = extra,
#'   type = "np"
#' )
#'
#' # within-subjects design
#' two_sample_test(
#'   data       = dplyr::filter(bugs_long, condition %in% c("HDHF", "HDLF")),
#'   x          = condition,
#'   y          = desire,
#'   paired     = TRUE,
#'   subject.id = subject,
#'   type       = "np"
#' )
#'
#' # robust ----------------------------------
#'
#' # between-subjects design
#' two_sample_test(
#'   data = sleep,
#'   x    = group,
#'   y    = extra,
#'   type = "r"
#' )
#'
#' # within-subjects design
#' two_sample_test(
#'   data       = dplyr::filter(bugs_long, condition %in% c("HDHF", "HDLF")),
#'   x          = condition,
#'   y          = desire,
#'   paired     = TRUE,
#'   subject.id = subject,
#'   type       = "r"
#' )
#'
#' #' # Bayesian ------------------------------
#'
#' # between-subjects design
#' two_sample_test(
#'   data = sleep,
#'   x    = group,
#'   y    = extra,
#'   type = "bayes"
#' )
#'
#' # within-subjects design
#' two_sample_test(
#'   data       = dplyr::filter(bugs_long, condition %in% c("HDHF", "HDLF")),
#'   x          = condition,
#'   y          = desire,
#'   paired     = TRUE,
#'   subject.id = subject,
#'   type       = "bayes"
#' )
#' @export
two_sample_test <- function(data,
                            x,
                            y,
                            subject.id = NULL,
                            type = "parametric",
                            paired = FALSE,
                            alternative = "two.sided",
                            k = 2L,
                            conf.level = 0.95,
                            effsize.type = "g",
                            var.equal = FALSE,
                            bf.prior = 0.707,
                            tr = 0.2,
                            nboot = 100L,
                            ...) {
  type <- stats_type_switch(type)
  c(x, y) %<-% c(ensym(x), ensym(y))

  data %<>% long_to_wide_converter(
    x          = {{ x }},
    y          = {{ y }},
    subject.id = {{ subject.id }},
    paired     = paired,
    spread     = ifelse(type %in% c("bayes", "robust"), paired, FALSE)
  )

  # parametric ---------------------------------------

  if (type == "parametric") {
    # styler: off
    k.df <- ifelse(paired || var.equal, 0L, k)
    .f   <- stats::t.test
    if (effsize.type %in% c("unbiased", "g")) .f.es <- effectsize::hedges_g
    if (effsize.type %in% c("biased", "d")) .f.es   <- effectsize::cohens_d
    # styler: on
  }

  # non-parametric ------------------------------------

  if (type == "nonparametric") c(.f, .f.es) %<-% c(stats::wilcox.test, effectsize::rank_biserial)

  if (type %in% c("parametric", "nonparametric")) {
    stats_df <- exec(
      .f,
      formula     = new_formula(y, x),
      data        = data,
      paired      = paired,
      alternative = alternative,
      var.equal   = var.equal,
      exact       = FALSE
    ) %>%
      tidy_model_parameters()

    effsize_df <- exec(
      .f.es,
      x           = new_formula(y, x),
      data        = data,
      paired      = paired,
      pooled_sd   = FALSE,
      ci          = conf.level,
      verbose     = FALSE
    ) %>%
      tidy_model_effectsize()
  }

  # robust ---------------------------------------

  if (type == "robust") {
    k.df <- ifelse(paired, 0L, k)

    # styler: off
    if (!paired) c(.f, .f.es) %<-% c(WRS2::yuen, WRS2::akp.effect)
    if (paired) c(.f, .f.es)  %<-% c(WRS2::yuend, WRS2::dep.effect)

    .f.args    <- list(formula = new_formula(y, x), data = data, x = data[[2L]], y = data[[3L]])
    .f.es.args <- list(EQVAR = FALSE, nboot = nboot, alpha = 1.0 - conf.level, tr = tr)

    effsize_df <- tidy_model_parameters(exec(.f.es, !!!.f.args, !!!.f.es.args), keep = "AKP")
    stats_df   <- tidy_model_parameters(exec(.f,    !!!.f.args, !!!.f.es.args))
    # styler: on
  }

  if (type != "bayes") {
    stats_df <- bind_cols(select(stats_df, -matches("^est|^eff|conf|^ci")), select(effsize_df, -matches("term")))
  }

  # Bayesian ---------------------------------------

  if (type == "bayes") {
    # styler: off
    if (!paired) .f.args <- list(formula = new_formula(y, x), paired = paired)
    if (paired) .f.args  <- list(x = data[[2L]], y = data[[3L]], paired = paired)
    # styler: on

    stats_df <- exec(BayesFactor::ttestBF, data = as.data.frame(data), rscale = bf.prior, !!!.f.args) %>%
      tidy_model_parameters(ci = conf.level)
  }

  # expression ---------------------------------------

  add_expression_col(
    data   = stats_df,
    paired = paired,
    n      = ifelse(paired, length(unique(data$.rowid)), nrow(data)),
    k      = k,
    k.df   = k.df
  )
}
