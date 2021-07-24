#' @title One-way analysis of variance (ANOVA)
#' @name oneway_anova
#'
#' @description
#'
#' A dataframe containing results for one-way ANOVA.
#'
#' To see details about functions which are internally used to carry out these
#' analyses, see the following vignette-
#' <https://indrajeetpatil.github.io/statsExpressions/articles/stats_details.html>
#'
#' @inheritParams long_to_wide_converter
#' @inheritParams stats_type_switch
#' @param conf.level Scalar between `0` and `1`. If unspecified, the defaults
#'   return `95%` confidence/credible intervals (`0.95`).
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
#' @inheritParams expr_template
#' @param ... Additional arguments (currently ignored).
#' @inheritParams stats::oneway.test
#'
#' @note
#' To carry out Bayesian posterior estimation for ANOVA designs, you will need
#' to install the development version of `BayesFactor` (`0.9.12-4.3`). You can
#' download it by running:
#' `remotes::install_github("richarddmorey/BayesFactor/pkg/BayesFactor")`.
#'
#' @importFrom dplyr select rename
#' @importFrom rlang !! !!! expr enexpr ensym exec new_formula
#' @importFrom stats oneway.test
#' @importFrom WRS2 t1way rmanova wmcpAKP
#' @importFrom stats friedman.test kruskal.test
#' @importFrom effectsize rank_epsilon_squared kendalls_w omega_squared eta_squared
#' @importFrom BayesFactor ttestBF anovaBF
#' @importFrom parameters model_parameters
#' @importFrom performance model_performance
#' @importFrom insight check_if_installed
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
                         nboot = 100L,
                         top.text = NULL,
                         ...) {

  # standardize the type of statistics
  type <- stats_type_switch(type)

  # make sure both quoted and unquoted arguments are supported
  c(x, y) %<-% c(rlang::ensym(x), rlang::ensym(y))

  # data cleanup
  data %<>%
    long_to_wide_converter(
      x = {{ x }},
      y = {{ y }},
      subject.id = {{ subject.id }},
      paired = paired,
      spread = FALSE
    ) %>%
    dplyr::mutate(rowid = as.factor(rowid))

  #  parametric ---------------------------------------

  if (type == "parametric") {
    # expression details
    k.df <- ifelse(!paired, 0L, k)
    k.df.error <- ifelse(!paired && var.equal, 0L, k)
    no.parameters <- 2L

    # which effect size?
    if (effsize.type %in% c("unbiased", "omega")) .f.es <- effectsize::omega_squared
    if (effsize.type %in% c("biased", "eta")) .f.es <- effectsize::eta_squared

    if (paired) {
      # check if `afex` is installed
      insight::check_if_installed("afex", minimum_version = "1.0-0")

      # Fisher's ANOVA
      mod <- afex::aov_ez(
        id = "rowid",
        dv = rlang::as_string(y),
        data = data,
        within = rlang::as_string(x),
        include_aov = TRUE
      )
    }

    if (!paired) {
      # Welch's ANOVA
      mod <- stats::oneway.test(
        formula = rlang::new_formula(y, x),
        data = data,
        var.equal = var.equal
      )
    }

    # tidying it up
    stats_df <- tidy_model_parameters(mod)
    effsize_df <- exec(.f.es, model = mod, ci = conf.level, verbose = FALSE) %>%
      tidy_model_effectsize(.)

    # combining dataframes
    stats_df <- dplyr::bind_cols(stats_df, effsize_df)
  }

  # non-parametric ------------------------------------

  if (type == "nonparametric") {
    # expression details
    c(no.parameters, k.df, k.df.error) %<-% c(1L, 0L, 0L)

    # Friedman test
    if (paired) {
      c(.f, .f.es) %<-% c(stats::friedman.test, effectsize::kendalls_w)
      .f.args <- list(formula = new_formula({{ enexpr(y) }}, expr(!!enexpr(x) | rowid)))
      .f.es.args <- list(x = new_formula({{ enexpr(y) }}, expr(!!enexpr(x) | rowid)))
    }

    # Kruskal-Wallis test
    if (!paired) {
      c(.f, .f.es) %<-% c(stats::kruskal.test, effectsize::rank_epsilon_squared)
      .f.args <- list(formula = rlang::new_formula(y, x))
      .f.es.args <- list(x = rlang::new_formula(y, x))
    }

    # extracting test details
    stats_df <- tidy_model_parameters(exec(.f, !!!.f.args, data = data))

    # computing respective effect sizes
    effsize_df <- exec(
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
  }

  # robust ---------------------------------------

  if (type == "robust") {
    # expression details
    c(no.parameters, k.df, k.df.error) %<-% c(2L, ifelse(isTRUE(paired), k, 0L), k)

    # heteroscedastic one-way repeated measures ANOVA for trimmed means
    if (paired) {
      mod <- WRS2::rmanova(
        y = data[[rlang::as_name(y)]],
        groups = data[[rlang::as_name(x)]],
        blocks = data[["rowid"]],
        tr = tr
      )
    }

    # heteroscedastic one-way ANOVA for trimmed means
    if (!paired) {
      mod <- WRS2::t1way(
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
    if (paired) {
      effsize_df <- long_to_wide_converter(data, {{ x }}, {{ y }}) %>%
        WRS2::wmcpAKP(dplyr::select(-rowid), tr = tr, nboot = nboot) %>%
        tidy_model_parameters(.)

      # combine dataframes
      stats_df <- dplyr::bind_cols(stats_df, effsize_df)
    }
  }

  # Bayesian ---------------------------------------

  # running Bayesian t-test
  if (type == "bayes") {
    if (!paired) .f.args <- list(formula = new_formula(y, x), rscaleFixed = bf.prior)
    if (paired) {
      .f.args <- list(
        formula = new_formula(rlang::enexpr(y), expr(!!rlang::enexpr(x) + rowid)),
        rscaleFixed = bf.prior, whichRandom = "rowid", rscaleRandom = 1
      )
    }

    # extract a dataframe
    stats_df <- exec(
      BayesFactor::anovaBF,
      data = as.data.frame(data),
      progress = FALSE,
      !!!.f.args
    ) %>%
      tidy_model_parameters(ci = conf.level)
  }

  # expression ---------------------------------------

  as_tibble(stats_df) %>%
    dplyr::mutate(expression = list(expr_template(
      data = .,
      no.parameters = no.parameters,
      n = ifelse(isTRUE(paired), length(unique(data$rowid)), nrow(data)),
      paired = paired,
      k = k,
      k.df = k.df,
      k.df.error = k.df.error,
      top.text = top.text,
      bayesian = ifelse(type == "bayes", TRUE, FALSE)
    )))
}
