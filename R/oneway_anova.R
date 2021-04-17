#' @title One-way analysis of variance (ANOVA)
#' @name oneway_anova
#'
#' @description
#'
#' A dataframe containing results for one-way ANOVA.
#'
#' For more details, see-
#' \url{https://indrajeetpatil.github.io/statsExpressions/articles/stats_details.html}
#'
#' @inheritParams ipmisc::long_to_wide_converter
#' @param type A character specifying the type of statistical approach.
#' Four possible options:
#'
#' \itemize{
#'   \item `"parametric"`
#'   \item `"nonparametric"`
#'   \item `"robust"`
#'   \item `"bayes"`
#' }
#'
#'   Corresponding abbreviations are also accepted: `"p"` (for parametric),
#'   `"np"` (for nonparametric), `"r"` (for robust), or `"bf"` (for Bayesian).
#' @param conf.level Scalar between `0` and `1`. If unspecified, the defaults
#'   return `95%` confidence/credible intervals (`0.95`).
#' @param effsize.type Type of effect size needed for *parametric* tests. The
#'   argument can be `"eta"` (partial eta-squared) or `"omega"` (partial
#'   omega-squared).
#' @param tr Trim level for the mean when carrying out `robust` tests. In case
#'   of an error, try reducing the value of `tr`, which is by default set to
#'   `0.2`. Lowering the value might help.
#' @param nboot Number of bootstrap samples for computing confidence interval
#'   for the effect size (Default: `100`).
#' @param bf.prior A number between `0.5` and `2` (default `0.707`), the prior
#'   width to use in calculating Bayes factors and posterior estimates.
#' @inheritParams two_sample_test
#' @inheritParams expr_template
#' @inheritParams bf_extractor
#' @param ... Additional arguments (currently ignored).
#' @inheritParams stats::oneway.test
#'
#' @note
#' 1. Please note that the function expects that the data is
#'   already sorted by subject/repeated measures ID.
#'
#' 2. To carry out Bayesian analysis for ANOVA designs, you will need to install
#' the development version of `BayesFactor` (`0.9.12-4.3`). You can download it
#' by running:
#' `remotes::install_github("richarddmorey/BayesFactor/pkg/BayesFactor")`.
#'
#' @importFrom dplyr select rename matches
#' @importFrom rlang !! !!! quo_is_null eval_tidy expr enexpr ensym exec new_formula
#' @importFrom stats oneway.test
#' @importFrom WRS2 t1way rmanova wmcpAKP
#' @importFrom stats friedman.test kruskal.test na.omit
#' @importFrom effectsize rank_epsilon_squared kendalls_w
#' @importFrom effectsize omega_squared eta_squared
#' @importFrom ipmisc long_to_wide_converter
#' @importFrom BayesFactor ttestBF anovaBF
#' @importFrom parameters model_parameters
#' @importFrom performance model_performance
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
#' # between-subjects
#' oneway_anova(
#'   data = ggplot2::msleep,
#'   x = vore,
#'   y = sleep_rem
#' )
#'
#' if (require("afex", quietly = TRUE)) {
#'   # within-subjects design
#'   oneway_anova(
#'     data = iris_long,
#'     x = condition,
#'     y = value,
#'     subject.id = id,
#'     paired = TRUE
#'   )
#' }
#'
#' # ----------------------- non-parametric ----------------------------------
#'
#' # between-subjects
#' oneway_anova(
#'   data = ggplot2::msleep,
#'   x = vore,
#'   y = sleep_rem,
#'   type = "np"
#' )
#'
#' # within-subjects design
#' oneway_anova(
#'   data = iris_long,
#'   x = condition,
#'   y = value,
#'   subject.id = id,
#'   paired = TRUE,
#'   type = "np"
#' )
#'
#' # ----------------------- robust -------------------------------------
#'
#' # between-subjects
#' oneway_anova(
#'   data = ggplot2::msleep,
#'   x = vore,
#'   y = sleep_rem,
#'   type = "r"
#' )
#'
#' # within-subjects design
#' oneway_anova(
#'   data = iris_long,
#'   x = condition,
#'   y = value,
#'   subject.id = id,
#'   paired = TRUE,
#'   type = "r"
#' )
#'
#' # ----------------------- Bayesian -------------------------------------
#'
#' # between-subjects
#' oneway_anova(
#'   data = ggplot2::msleep,
#'   x = vore,
#'   y = sleep_rem,
#'   type = "bayes"
#' )
#'
#' # within-subjects design
#' # needs `BayesFactor 0.9.12-4.3` or above
#' if (utils::packageVersion("BayesFactor") >= package_version("0.9.12-4.3")) {
#'   oneway_anova(
#'     data = iris_long,
#'     x = condition,
#'     y = value,
#'     subject.id = id,
#'     paired = TRUE,
#'     type = "bayes"
#'   )
#' }
#' }
#' @export

# function body
oneway_anova <- function(data,
                         x,
                         y,
                         subject.id = NULL,
                         type = "parametric",
                         paired = FALSE,
                         k = 2L,
                         conf.level = 0.95,
                         effsize.type = "omega",
                         var.equal = FALSE,
                         bf.prior = 0.707,
                         tr = 0.2,
                         nboot = 100,
                         top.text = NULL,
                         ...) {

  # standardize the type of statistics
  type <- ipmisc::stats_type_switch(type)

  # make sure both quoted and unquoted arguments are supported
  c(x, y) %<-% c(rlang::ensym(x), rlang::ensym(y))

  # data cleanup
  data %<>%
    ipmisc::long_to_wide_converter(
      x = {{ x }},
      y = {{ y }},
      subject.id = {{ subject.id }},
      paired = paired,
      spread = FALSE
    ) %>%
    dplyr::mutate(rowid = as.factor(rowid))

  # ----------------------- parametric ---------------------------------------

  if (type == "parametric") {
    # which effect size?
    if (effsize.type %in% c("unbiased", "omega")) .f.es <- effectsize::omega_squared
    if (effsize.type %in% c("biased", "eta")) .f.es <- effectsize::eta_squared

    if (isTRUE(paired)) {
      # check if `afex` is installed
      if (!requireNamespace("afex", quietly = TRUE)) stop("Package 'afex' needs to be installed.")

      # Fisher's ANOVA
      mod <-
        afex::aov_ez(
          id = "rowid",
          dv = rlang::as_string(y),
          data = data,
          within = rlang::as_string(x)
        )
    }

    if (isFALSE(paired)) {
      # Welch's ANOVA
      mod <-
        stats::oneway.test(
          formula = rlang::new_formula(y, x),
          data = data,
          var.equal = var.equal
        )
    }

    # tidying it up
    stats_df <- tidy_model_parameters(mod)
    effsize_df <-
      suppressWarnings(rlang::exec(
        .fn = .f.es,
        model = mod,
        ci = conf.level,
        verbose = FALSE
      )) %>%
      tidy_model_effectsize(.)

    # combining dataframes
    stats_df <- dplyr::bind_cols(stats_df, effsize_df)

    # expression details
    if (isTRUE(paired)) var.equal <- TRUE
    k.df <- ifelse(isFALSE(paired), 0L, k)
    k.df.error <- ifelse(isFALSE(paired) && isTRUE(var.equal), 0L, k)
    no.parameters <- 2L
  }

  # ----------------------- non-parametric ------------------------------------

  if (type == "nonparametric") {
    # Friedman test
    if (isTRUE(paired)) {
      c(.f, .f.es) %<-% c(stats::friedman.test, effectsize::kendalls_w)
      .f.args <- list(formula = new_formula({{ enexpr(y) }}, expr(!!enexpr(x) | rowid)))
      .f.es.args <- list(x = new_formula({{ enexpr(y) }}, expr(!!enexpr(x) | rowid)))
    }

    # Kruskal-Wallis test
    if (isFALSE(paired)) {
      c(.f, .f.es) %<-% c(stats::kruskal.test, effectsize::rank_epsilon_squared)
      .f.args <- list(formula = rlang::new_formula(y, x))
      .f.es.args <- list(x = rlang::new_formula(y, x))
    }

    # extracting test details
    stats_df <-
      rlang::exec(.fn = .f, !!!.f.args, data = data) %>%
      tidy_model_parameters(.)

    # computing respective effect sizes
    effsize_df <-
      rlang::exec(
        .fn = .f.es,
        data = data,
        ci = conf.level,
        iterations = nboot,
        verbose = FALSE,
        !!!.f.es.args
      ) %>%
      tidy_model_effectsize(.)

    # dataframe
    stats_df <- dplyr::bind_cols(stats_df, effsize_df)

    # expression details
    c(no.parameters, k.df, k.df.error) %<-% c(1L, 0L, 0L)
  }

  # ----------------------- robust ---------------------------------------

  if (type == "robust") {
    # heteroscedastic one-way repeated measures ANOVA for trimmed means
    if (isTRUE(paired)) {
      # test
      mod <-
        WRS2::rmanova(
          y = data[[rlang::as_name(y)]],
          groups = data[[rlang::as_name(x)]],
          blocks = data[["rowid"]],
          tr = tr
        )
    }

    # heteroscedastic one-way ANOVA for trimmed means
    if (isFALSE(paired)) {
      mod <-
        WRS2::t1way(
          formula = rlang::new_formula(y, x),
          data = data,
          tr = tr,
          alpha = 1 - conf.level,
          nboot = nboot
        )
    }

    # parameter extraction
    stats_df <- tidy_model_parameters(mod)

    # for paired designs, WRS2 currently doesn't return effect size
    if (isTRUE(paired)) {
      effsize_df <-
        ipmisc::long_to_wide_converter(data, {{ x }}, {{ y }}) %>%
        wAKPavg(dplyr::select(-rowid), tr = tr, nboot = nboot) %>%
        dplyr::mutate(effectsize = "Algina-Keselman-Penfield robust standardized difference average")

      # combine dataframes
      stats_df <- dplyr::bind_cols(stats_df, effsize_df)
    }

    # expression details
    c(no.parameters, k.df, k.df.error) %<-% c(2L, ifelse(isTRUE(paired), k, 0L), k)
  }

  # final returns
  if (type != "bayes") {
    stats_df %<>%
      dplyr::mutate(expression = list(expr_template(
        no.parameters = no.parameters,
        data = .,
        n = ifelse(isTRUE(paired), length(unique(data$rowid)), nrow(data)),
        paired = paired,
        k = k,
        k.df = k.df,
        k.df.error = k.df.error
      )))
  }

  # ----------------------- Bayesian ---------------------------------------

  # running Bayesian t-test
  if (type == "bayes") {
    if (!paired) .f.args <- list(formula = new_formula(y, x), rscaleFixed = bf.prior)
    if (paired) {
      .f.args <- list(
        formula = new_formula(rlang::enexpr(y), expr(!!rlang::enexpr(x) + rowid)),
        rscaleFixed = bf.prior, whichRandom = "rowid", rscaleRandom = 1
      )
    }

    # creating a `BayesFactor` object
    bf_object <- rlang::exec(
      .fn = BayesFactor::anovaBF,
      data = as.data.frame(data),
      progress = FALSE,
      !!!.f.args
    )

    # final return
    stats_df <- bf_extractor(bf_object, conf.level, k = k, top.text = top.text)
  }

  as_tibble(stats_df)
}

#' @noRd

wAKPavg <- function(x, tr = 0.2, nboot = 100, ...) {
  A <- WRS2::wmcpAKP(x, tr, nboot)
  tibble("estimate" = A[[1]], "conf.low" = A[[2]], "conf.high" = A[[3]], "conf.level" = 0.95)
}
