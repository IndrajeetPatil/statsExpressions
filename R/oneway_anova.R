#' @title Expression and dataframe for one-way ANOVA
#' @name expr_oneway_anova
#'
#' @return For more details, see-
#' \url{https://indrajeetpatil.github.io/statsExpressions/articles/stats_details.html}
#'
#' @inheritParams ipmisc::long_to_wide_converter
#' @param type Type of statistic expected (`"parametric"` or `"nonparametric"`
#'   or `"robust"` or `"bayes"`).Corresponding abbreviations are also accepted:
#'   `"p"` (for parametric), `"np"` (nonparametric), `"r"` (robust), or
#'   `"bf"`resp.
#' @param effsize.type Type of effect size needed for *parametric* tests. The
#'   argument can be `"eta"` (partial eta-squared) or `"omega"` (partial
#'   omega-squared).
#' @param output If `"expression"`, will return expression with statistical
#'   details, while `"dataframe"` will return a dataframe containing the
#'   results.
#' @param tr Trim level for the mean when carrying out `robust` tests. If you
#'   get error stating "Standard error cannot be computed because of Winsorized
#'   variance of 0 (e.g., due to ties). Try to decrease the trimming level.",
#'   try to play around with the value of `tr`, which is by default set to
#'   `0.1`. Lowering the value might help.
#' @param nboot Number of bootstrap samples for computing confidence interval
#'   for the effect size (Default: `100`).
#' @param bf.prior A number between `0.5` and `2` (default `0.707`), the prior
#'   width to use in calculating Bayes factors.
#' @inheritParams expr_t_twosample
#' @inheritParams expr_template
#' @inheritParams bf_extractor
#' @param ... Additional arguments (currently ignored).
#' @inheritParams stats::oneway.test
#'
#' @importFrom dplyr select rename matches
#' @importFrom rlang !! !!! quo_is_null eval_tidy expr enexpr ensym exec new_formula
#' @importFrom stats oneway.test
#' @importFrom afex aov_ez
#' @importFrom WRS2 t1way rmanova
#' @importFrom stats friedman.test kruskal.test na.omit
#' @importFrom effectsize rank_epsilon_squared kendalls_w
#' @importFrom effectsize omega_squared eta_squared
#' @importFrom ipmisc long_to_wide_converter format_num
#' @importFrom BayesFactor ttestBF anovaBF
#' @importFrom parameters model_parameters
#' @importFrom performance model_performance
#'
#' @examples
#' \donttest{
#' # for reproducibility
#' set.seed(123)
#' library(statsExpressions)
#'
#' # ----------------------- parametric -------------------------------------
#'
#' # between-subjects
#' expr_oneway_anova(
#'   data = ggplot2::msleep,
#'   x = vore,
#'   y = sleep_rem
#' )
#'
#' # within-subjects design
#' expr_oneway_anova(
#'   data = iris_long,
#'   x = condition,
#'   y = value,
#'   subject.id = id,
#'   paired = TRUE,
#'   output = "dataframe"
#' )
#'
#' # ----------------------- non-parametric ----------------------------------
#'
#' # between-subjects
#' expr_oneway_anova(
#'   data = ggplot2::msleep,
#'   x = vore,
#'   y = sleep_rem,
#'   type = "np"
#' )
#'
#' # within-subjects design
#' expr_oneway_anova(
#'   data = iris_long,
#'   x = condition,
#'   y = value,
#'   subject.id = id,
#'   paired = TRUE,
#'   type = "np",
#'   output = "dataframe"
#' )
#'
#' # ----------------------- robust -------------------------------------
#'
#' # between-subjects
#' expr_oneway_anova(
#'   data = ggplot2::msleep,
#'   x = vore,
#'   y = sleep_rem,
#'   type = "r"
#' )
#'
#' # within-subjects design
#' expr_oneway_anova(
#'   data = iris_long,
#'   x = condition,
#'   y = value,
#'   subject.id = id,
#'   paired = TRUE,
#'   type = "r",
#'   output = "dataframe"
#' )
#'
#' # ----------------------- Bayesian -------------------------------------
#'
#' # between-subjects
#' expr_oneway_anova(
#'   data = ggplot2::msleep,
#'   x = vore,
#'   y = sleep_rem,
#'   type = "bayes"
#' )
#'
#' # within-subjects design
#' # needs `BayesFactor 0.9.12-4.3` or above
#' if (utils::packageVersion("BayesFactor") >= package_version("0.9.12-4.3")) {
#'   expr_oneway_anova(
#'     data = iris_long,
#'     x = condition,
#'     y = value,
#'     subject.id = id,
#'     paired = TRUE,
#'     type = "bayes",
#'     output = "dataframe"
#'   )
#' }
#' }
#' @export

# function body
expr_oneway_anova <- function(data,
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
                              tr = 0.1,
                              nboot = 100,
                              top.text = NULL,
                              output = "expression",
                              ...) {

  # standardize the type of statistics
  type <- ipmisc::stats_type_switch(type)

  # make sure both quoted and unquoted arguments are supported
  c(x, y) %<-% c(rlang::ensym(x), rlang::ensym(y))

  # data cleanup
  data %<>%
    ipmisc::long_to_wide_converter(
      data = .,
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

    # Fisher's ANOVA
    if (isTRUE(paired)) {
      mod <-
        afex::aov_ez(
          id = "rowid",
          dv = rlang::as_string(y),
          data = data,
          within = rlang::as_string(x)
        )
    }

    # Welch's ANOVA
    if (isFALSE(paired)) {
      mod <-
        stats::oneway.test(
          formula = rlang::new_formula(y, x),
          data = data,
          na.action = na.omit,
          var.equal = var.equal
        )
    }

    # tidying it up
    stats_df <- tidy_model_parameters(mod)
    effsize_df <-
      suppressWarnings(rlang::exec(
        .fn = .f.es,
        model = mod,
        ci = conf.level
      )) %>%
      tidy_model_effectsize(.)

    # combining dataframes
    stats_df <- dplyr::bind_cols(stats_df, effsize_df)

    # expression details
    if (isTRUE(paired)) var.equal <- TRUE
    k.parameter <- ifelse(isFALSE(paired), 0L, k)
    k.parameter2 <- ifelse(isFALSE(paired) && isTRUE(var.equal), 0L, k)
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
      rlang::exec(
        .fn = .f,
        !!!.f.args,
        data = data,
        na.action = na.omit
      ) %>%
      tidy_model_parameters(.)

    # computing respective effect sizes
    effsize_df <-
      rlang::exec(
        .fn = .f.es,
        data = data,
        ci = conf.level,
        iterations = nboot,
        !!!.f.es.args
      ) %>%
      tidy_model_effectsize(.)

    # dataframe
    stats_df <- dplyr::bind_cols(stats_df, effsize_df)

    # expression details
    c(no.parameters, k.parameter, k.parameter2) %<-% c(1L, 0L, 0L)
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
        ipmisc::long_to_wide_converter(data, {{ x }}, {{ y }}, paired = TRUE, spread = TRUE) %>%
        wAKPavg(dplyr::select(-rowid), tr = tr, nboot = nboot) %>%
        dplyr::mutate(effectsize = "Algina-Keselman-Penfield robust standardized difference average")

      # combine dataframes
      stats_df <- dplyr::bind_cols(stats_df, effsize_df)
    }

    # expression details
    c(no.parameters, k.parameter, k.parameter2) %<-% c(2L, ifelse(isTRUE(paired), k, 0L), k)
  }

  # final returns
  if (type != "bayes") {
    expression <-
      expr_template(
        no.parameters = no.parameters,
        stats.df = stats_df,
        n = ifelse(isTRUE(paired), length(unique(data$rowid)), nrow(data)),
        paired = paired,
        k = k,
        k.parameter = k.parameter,
        k.parameter2 = k.parameter2
      )
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
    expression <- stats_df <- bf_extractor(bf_object, conf.level, k = k, top.text = top.text, output = output)
  }

  # return the output
  switch(output, "dataframe" = as_tibble(stats_df), expression)
}


#' @name wAKPavg
#' @note Adapted from Rand Wilcox's script
#'
#' @param x A dataframe in wide format.
#'
#' @importFrom WRS2 dep.effect
#'
#' @examples
#' before <- c(190, 210, 300, 240, 280, 170, 280, 250, 240, 220)
#' now <- c(170, 280, 250, 240, 190, 260, 180, 200, 100, 200)
#' after <- c(210, 210, 340, 190, 260, 180, 200, 220, 230, 200)
#' df <- data.frame(before, now, after)
#' wAKPavg(df)
#' @noRd

wAKPavg <- function(x, tr = 0.2, nboot = 100, ...) {
  x <- as.list(x) # dataframe to a list
  J <- length(x)
  C <- (J^2 - J) / 2
  A <- matrix(NA, nrow = C, ncol = 3)
  dimnames(A) <- list(NULL, c("estimate", "conf.low", "conf.high"))
  ic <- 0
  for (j in 1:J) {
    for (k in 1:J) {
      if (j < k) {
        ic <- ic + 1
        A[ic, 1] <- WRS2::dep.effect(x[[j]], x[[k]], tr = tr, nboot = nboot)[5]
        A[ic, 2] <- WRS2::dep.effect(x[[j]], x[[k]], tr = tr, nboot = nboot)[21]
        A[ic, 3] <- WRS2::dep.effect(x[[j]], x[[k]], tr = tr, nboot = nboot)[25]
      }
    }
  }

  # return as a dataframe
  tibble(
    "estimate" = mean(A[, 1]),
    "conf.low" = mean(A[, 2]),
    "conf.high" = mean(A[, 3]),
    "ci.width" = 0.95
  )
}
