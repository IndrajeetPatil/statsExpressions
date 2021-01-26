#' @title Expression and dataframe for two-sample *t*-test
#' @name expr_t_twosample
#'
#' @inheritParams ipmisc::long_to_wide_converter
#' @param effsize.type Type of effect size needed for *parametric* tests. The
#'   argument can be `"d"` (for Cohen's *d*) or `"g"` (for Hedge's *g*).
#' @inheritParams expr_t_onesample
#' @inheritParams expr_oneway_anova
#' @inheritParams stats::t.test
#' @inheritParams expr_template
#'
#' @importFrom dplyr select rename_all recode mutate
#' @importFrom rlang !!! expr enexpr ensym exec new_formula
#' @importFrom tidyr drop_na
#' @importFrom stats t.test  wilcox.test
#' @importFrom BayesFactor ttestBF
#' @importFrom WRS2 yuen yuen.effect.ci yuend dep.effect
#' @importFrom effectsize cohens_d hedges_g rank_biserial
#'
#' @return Expression containing details from results of a two-sample test and
#'   effect size plus confidence intervals.
#'
#' @note The *stats::wilcox.test* function does not follow the same convention
#'   as *stats::t.test*. The sign of the *V* test statistic will always be
#'   positive since it is **the sum of the positive signed ranks**. Therefore,
#'   *V* will vary in magnitude but not significance based solely on the order
#'   of the grouping variable. Consider manually reordering your factor levels
#'   if appropriate as shown in the second example below.
#'
#' @references For more details, see-
#' \url{https://indrajeetpatil.github.io/statsExpressions/articles/stats_details.html}
#'
#' @examples
#' \donttest{
#' # for reproducibility
#' set.seed(123)
#' library(statsExpressions)
#'
#' # ----------------------- parametric -------------------------------------
#'
#' # between-subjects design
#' expr_t_twosample(
#'   data = sleep,
#'   x = group,
#'   y = extra,
#'   type = "p"
#' )
#'
#' # within-subjects design
#' expr_t_twosample(
#'   data = VR_dilemma,
#'   x = modality,
#'   y = score,
#'   paired = TRUE,
#'   subject.id = id,
#'   type = "p",
#'   output = "dataframe"
#' )
#'
#' # ----------------------- non-parametric ----------------------------------
#'
#' # between-subjects design
#' expr_t_twosample(
#'   data = sleep,
#'   x = group,
#'   y = extra,
#'   type = "np"
#' )
#'
#' # within-subjects design
#' expr_t_twosample(
#'   data = VR_dilemma,
#'   x = modality,
#'   y = score,
#'   paired = TRUE,
#'   subject.id = id,
#'   type = "np",
#'   output = "dataframe"
#' )
#'
#' # ------------------------------ robust ----------------------------------
#'
#' # between-subjects design
#' expr_t_twosample(
#'   data = sleep,
#'   x = group,
#'   y = extra,
#'   type = "r"
#' )
#'
#' # within-subjects design
#' expr_t_twosample(
#'   data = VR_dilemma,
#'   x = modality,
#'   y = score,
#'   paired = TRUE,
#'   subject.id = id,
#'   type = "r",
#'   output = "dataframe"
#' )
#'
#' #' # ------------------------------ Bayesian ------------------------------
#'
#' # between-subjects design
#' expr_t_twosample(
#'   data = sleep,
#'   x = group,
#'   y = extra,
#'   type = "bayes"
#' )
#'
#' # within-subjects design
#' expr_t_twosample(
#'   data = VR_dilemma,
#'   x = modality,
#'   y = score,
#'   paired = TRUE,
#'   subject.id = id,
#'   type = "bayes",
#'   output = "dataframe"
#' )
#' }
#' @export

# function body
expr_t_twosample <- function(data,
                             x,
                             y,
                             subject.id = NULL,
                             type = "parametric",
                             paired = FALSE,
                             k = 2L,
                             conf.level = 0.95,
                             effsize.type = "g",
                             var.equal = FALSE,
                             bf.prior = 0.707,
                             tr = 0.1,
                             nboot = 100,
                             top.text = NULL,
                             output = "expression",
                             ...) {
  # standardize the type of statistics
  type <- ipmisc::stats_type_switch(type)

  # make sure both quoted and unquoted arguments are supported
  c(x, y) %<-% c(rlang::ensym(x), rlang::ensym(y))

  # whether to switch from long to wide depends on the test
  spread <- ifelse(type %in% c("bayes", "robust"), paired, FALSE)

  # properly removing NAs if it's a paired design
  data %<>%
    long_to_wide_converter(
      data = .,
      x = {{ x }},
      y = {{ y }},
      subject.id = {{ subject.id }},
      paired = paired,
      spread = spread
    )

  # ----------------------- parametric ---------------------------------------

  if (type == "parametric") {
    # preparing expression parameters
    no.parameters <- 1L
    .f <- stats::t.test
    k.parameter <- ifelse(isTRUE(paired) || isTRUE(var.equal), 0L, k)
    if (effsize.type %in% c("unbiased", "g")) .f.es <- effectsize::hedges_g
    if (effsize.type %in% c("biased", "d")) .f.es <- effectsize::cohens_d
  }

  # ----------------------- non-parametric ------------------------------------

  if (type == "nonparametric") {
    # preparing expression parameters
    no.parameters <- 0L
    c(.f, .f.es) %<-% c(stats::wilcox.test, effectsize::rank_biserial)
  }

  # preparing expression
  if (type %in% c("parametric", "nonparametric")) {
    # extracting test details
    stats_df <-
      rlang::exec(
        .fn = .f,
        formula = rlang::new_formula(y, x),
        data = data,
        paired = paired,
        var.equal = var.equal,
        na.action = na.omit,
        exact = FALSE
      ) %>%
      tidy_model_parameters(.)

    # extracting effect size details
    effsize_df <-
      rlang::exec(
        .fn = .f.es,
        x = rlang::new_formula(y, x),
        data = data,
        paired = paired,
        ci = conf.level,
        iterations = nboot
      ) %>%
      tidy_model_effectsize(.)

    # these can be really big values
    if (type == "nonparametric") stats_df %<>% dplyr::mutate(statistic = log(statistic))
  }

  # ----------------------- robust ---------------------------------------

  if (type == "robust") {
    # expression parameters
    c(no.parameters, k.parameter) %<-% c(1L, k)

    # running robust analysis
    if (isFALSE(paired)) {
      # computing effect size and its confidence interval
      mod2 <-
        WRS2::yuen.effect.ci(
          formula = rlang::new_formula(y, x),
          data = data,
          tr = tr,
          nboot = nboot,
          alpha = 1 - conf.level
        )

      # Yuen's test for trimmed means
      mod <- WRS2::yuen(formula = rlang::new_formula(y, x), data = data, tr = tr)

      # tidying it up
      stats_df <- tidy_model_parameters(mod)
      effsize_df <-
        tibble(
          estimate = mod2$effsize[[1]],
          conf.low = mod2$CI[[1]],
          conf.high = mod2$CI[[2]],
          ci.width = conf.level,
          effectsize = "Explanatory measure of effect size"
        )
    }

    if (isTRUE(paired)) {
      # expression parameters
      c(k.parameter, conf.level) %<-% c(0L, 0.95)

      # running robust paired t-test and its effect size
      mod <- WRS2::yuend(x = data[2], y = data[3], tr = tr)
      mod2 <- WRS2::dep.effect(x = data[2], y = data[3], tr = tr, nboot = nboot)

      # tidying it up
      stats_df <- tidy_model_parameters(mod)
      effsize_df <-
        as_tibble(as.data.frame(mod2), rownames = "effectsize") %>%
        dplyr::filter(effectsize == "AKP") %>%
        dplyr::mutate(
          effectsize = "Algina-Keselman-Penfield robust standardized difference",
          ci.width = 0.95
        ) %>%
        dplyr::select(estimate = Est, conf.low = ci.low, conf.high = ci.up, ci.width, effectsize)
    }
  }

  # final returns
  if (type != "bayes") {
    # combining dataframes
    stats_df <- dplyr::bind_cols(dplyr::select(stats_df, -dplyr::matches("^est|^eff|conf|^ci")), effsize_df)

    # expression
    expression <-
      expr_template(
        no.parameters = no.parameters,
        stats.df = stats_df,
        paired = paired,
        n = ifelse(isTRUE(paired), length(unique(data$rowid)), nrow(data)),
        k = k,
        k.parameter = k.parameter
      )
  }

  # ----------------------- Bayesian ---------------------------------------

  # running Bayesian t-test
  if (type == "bayes") {
    if (!paired) .f.args <- list(formula = new_formula(y, x), rscale = bf.prior, paired = paired)
    if (paired) .f.args <- list(x = data[[2]], y = data[[3]], rscale = bf.prior, paired = paired)

    # creating a `BayesFactor` object
    bf_object <- rlang::exec(.fn = BayesFactor::ttestBF, data = as.data.frame(data), !!!.f.args)

    # final return
    expression <- stats_df <- bf_extractor(bf_object, conf.level, k = k, top.text = top.text, output = output)
  }

  # return the output
  switch(output, "dataframe" = as_tibble(stats_df), expression)
}
