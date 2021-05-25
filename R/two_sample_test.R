#' @title Two-sample tests
#' @name two_sample_test
#'
#' @inheritParams ipmisc::long_to_wide_converter
#' @inheritParams ipmisc::stats_type_switch
#' @inheritParams one_sample_test
#' @inheritParams oneway_anova
#' @inheritParams stats::t.test
#' @inheritParams expr_template
#'
#' @importFrom dplyr select rename_all recode mutate
#' @importFrom rlang !!! expr enexpr ensym exec new_formula
#' @importFrom tidyr drop_na
#' @importFrom stats t.test  wilcox.test
#' @importFrom BayesFactor ttestBF
#' @importFrom WRS2 yuen akp.effect yuend dep.effect
#' @importFrom effectsize cohens_d hedges_g rank_biserial
#'
#' @description
#'
#'  A dataframe containing results from a two-sample test and effect size plus
#'  confidence intervals.
#'
#'  To see details about functions which are internally used to carry out these
#'  analyses, see the following vignette-
#'  \url{https://indrajeetpatil.github.io/statsExpressions/articles/stats_details.html}
#'
#' @examples
#' \donttest{
#' # for reproducibility
#' set.seed(123)
#' library(statsExpressions)
#' options(tibble.width = Inf, pillar.bold = TRUE, pillar.neg = TRUE)
#'
#' # ----------------------- parametric -------------------------------------
#'
#' # between-subjects design
#' two_sample_test(
#'   data = sleep,
#'   x = group,
#'   y = extra,
#'   type = "p"
#' )
#'
#' # within-subjects design
#' two_sample_test(
#'   data = VR_dilemma,
#'   x = modality,
#'   y = score,
#'   paired = TRUE,
#'   subject.id = id,
#'   type = "p"
#' )
#'
#' # ----------------------- non-parametric ----------------------------------
#'
#' # between-subjects design
#' two_sample_test(
#'   data = sleep,
#'   x = group,
#'   y = extra,
#'   type = "np"
#' )
#'
#' # within-subjects design
#' two_sample_test(
#'   data = VR_dilemma,
#'   x = modality,
#'   y = score,
#'   paired = TRUE,
#'   subject.id = id,
#'   type = "np"
#' )
#'
#' # ------------------------------ robust ----------------------------------
#'
#' # between-subjects design
#' two_sample_test(
#'   data = sleep,
#'   x = group,
#'   y = extra,
#'   type = "r"
#' )
#'
#' # within-subjects design
#' two_sample_test(
#'   data = VR_dilemma,
#'   x = modality,
#'   y = score,
#'   paired = TRUE,
#'   subject.id = id,
#'   type = "r"
#' )
#'
#' #' # ------------------------------ Bayesian ------------------------------
#'
#' # between-subjects design
#' two_sample_test(
#'   data = sleep,
#'   x = group,
#'   y = extra,
#'   type = "bayes"
#' )
#'
#' # within-subjects design
#' two_sample_test(
#'   data = VR_dilemma,
#'   x = modality,
#'   y = score,
#'   paired = TRUE,
#'   subject.id = id,
#'   type = "bayes"
#' )
#' }
#' @export

# function body
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
                            top.text = NULL,
                            ...) {
  # standardize the type of statistics
  type <- ipmisc::stats_type_switch(type)

  # make sure both quoted and unquoted arguments are supported
  c(x, y) %<-% c(rlang::ensym(x), rlang::ensym(y))

  # properly removing NAs if it's a paired design
  data %<>%
    long_to_wide_converter(
      x = {{ x }},
      y = {{ y }},
      subject.id = {{ subject.id }},
      paired = paired,
      spread = ifelse(type %in% c("bayes", "robust"), paired, FALSE)
    )

  # ----------------------- parametric ---------------------------------------

  if (type == "parametric") {
    # preparing expression parameters
    c(no.parameters, k.df) %<-% c(1L, ifelse(isTRUE(paired) || isTRUE(var.equal), 0L, k))
    .f <- stats::t.test

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
    stats_df <- rlang::exec(
      .f,
      formula = rlang::new_formula(y, x),
      data = data,
      paired = paired,
      alternative = alternative,
      var.equal = var.equal,
      exact = FALSE
    ) %>%
      tidy_model_parameters(.)

    # extracting effect size details
    effsize_df <- rlang::exec(
      .f.es,
      x = rlang::new_formula(y, x),
      data = data,
      paired = paired,
      pooled_sd = FALSE,
      ci = conf.level,
      verbose = FALSE
    ) %>%
      tidy_model_effectsize(.)

    # these can be really big values
    if (type == "nonparametric") stats_df %<>% dplyr::mutate(statistic = log(statistic))
  }

  # ----------------------- robust ---------------------------------------

  if (type == "robust") {
    # expression parameters
    c(no.parameters, k.df) %<-% c(1L, ifelse(isTRUE(paired), 0L, k))

    # running robust analysis
    if (isFALSE(paired)) {
      # computing effect size and its confidence interval
      effsize_df <- WRS2::akp.effect(
        formula = rlang::new_formula(y, x),
        data = data,
        EQVAR = FALSE,
        tr = tr,
        nboot = nboot,
        alpha = 1 - conf.level
      ) %>%
        tidy_model_parameters(.)

      # Yuen's test for trimmed means
      stats_df <- WRS2::yuen(formula = rlang::new_formula(y, x), data = data, tr = tr) %>%
        tidy_model_parameters(.)
    }

    if (isTRUE(paired)) {
      # Yuen's paired test for trimmed means
      stats_df <- WRS2::yuend(x = data[2], y = data[3], tr = tr) %>%
        tidy_model_parameters(.)

      # computing effect size and its confidence interval
      effsize_df <- WRS2::dep.effect(x = data[2], y = data[3], tr = tr, nboot = nboot) %>%
        as_tibble(as.data.frame(.), rownames = "effectsize") %>%
        dplyr::filter(effectsize == "AKP") %>%
        dplyr::mutate(
          effectsize = "Algina-Keselman-Penfield robust standardized difference",
          conf.level = 0.95
        ) %>%
        dplyr::select(estimate = Est, conf.low = ci.low, conf.high = ci.up, conf.level, effectsize)
    }
  }

  # final returns
  if (type != "bayes") {
    # combining dataframes
    stats_df <- dplyr::bind_cols(dplyr::select(stats_df, -dplyr::matches("^est|^eff|conf|^ci")), effsize_df)

    # expression
    stats_df %<>%
      dplyr::mutate(expression = list(expr_template(
        no.parameters = no.parameters,
        data = .,
        paired = paired,
        n = ifelse(isTRUE(paired), length(unique(data$rowid)), nrow(data)),
        k = k,
        k.df = k.df
      )))
  }

  # ----------------------- Bayesian ---------------------------------------

  # running Bayesian t-test
  if (type == "bayes") {
    if (!paired) .f.args <- list(formula = new_formula(y, x), rscale = bf.prior, paired = paired)
    if (paired) .f.args <- list(x = data[[2]], y = data[[3]], rscale = bf.prior, paired = paired)

    # creating a `BayesFactor` object
    bf_object <- rlang::exec(BayesFactor::ttestBF, data = as.data.frame(data), !!!.f.args)

    # final return
    stats_df <- bf_extractor(bf_object, conf.level, k = k, top.text = top.text)
  }

  # return the output
  as_tibble(stats_df)
}
