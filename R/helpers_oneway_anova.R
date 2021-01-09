#' @title Expression containing ANOVA results
#' @name expr_oneway_anova
#'
#' @return For more details, see-
#' \url{https://indrajeetpatil.github.io/statsExpressions/articles/stats_details.html}
#'
#' @inheritParams ipmisc::long_to_wide_converter
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
#' @inheritParams expr_t_twosample
#' @inheritParams expr_template
#' @param ... Additional arguments (currently ignored).
#' @inheritParams stats::oneway.test
#'
#' @importFrom dplyr select rename matches
#' @importFrom rlang !! !!! enquo eval_tidy expr enexpr ensym exec new_formula
#' @importFrom stats oneway.test
#' @importFrom afex aov_ez
#' @importFrom WRS2 t1way rmanova
#' @importFrom stats friedman.test kruskal.test na.omit
#' @importFrom effectsize rank_epsilon_squared kendalls_w
#' @importFrom effectsize omega_squared eta_squared
#' @importFrom ipmisc long_to_wide_converter format_num
#' @importFrom tidyBF bf_oneway_anova
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
#' expr_oneway_anova(
#'   data = iris_long,
#'   x = condition,
#'   y = value,
#'   subject.id = id,
#'   paired = TRUE,
#'   type = "bayes",
#'   output = "dataframe"
#' )
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
                              output = "expression",
                              ...) {

  # standardize the type of statistics
  stats.type <- ipmisc::stats_type_switch(type)

  # make sure both quoted and unquoted arguments are supported
  c(x, y) %<-% c(rlang::ensym(x), rlang::ensym(y))

  # data cleanup
  if (stats.type != "bayes") {
    data %<>%
      ipmisc::long_to_wide_converter(
        data = .,
        x = {{ x }},
        y = {{ y }},
        subject.id = {{ subject.id }},
        paired = paired,
        spread = FALSE
      )
  }

  # ----------------------- parametric ---------------------------------------

  if (stats.type == "parametric") {
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
    if (!"method" %in% names(stats_df)) {
      stats_df %<>% dplyr::mutate(method = "One-way analysis of means")
    }
    stats_df <- dplyr::bind_cols(stats_df, effsize_df)

    # expression details
    if (isTRUE(paired)) var.equal <- TRUE
    k.parameter <- ifelse(isFALSE(paired), 0L, k)
    k.parameter2 <- ifelse(isFALSE(paired) && isTRUE(var.equal), 0L, k)
    sample_size <- ifelse(isTRUE(paired), length(unique(data$rowid)), nrow(data))
    no.parameters <- 2L
  }

  # ----------------------- non-parametric ------------------------------------

  if (stats.type == "nonparametric") {
    # Friedman test
    if (isTRUE(paired)) {
      .f <- stats::friedman.test
      .f.args <- list(formula = new_formula({{ enexpr(y) }}, expr(!!enexpr(x) | rowid)))
      .f.es <- effectsize::kendalls_w
      .f.es.args <- list(x = new_formula({{ enexpr(y) }}, expr(!!enexpr(x) | rowid)))
    }

    # Kruskal-Wallis test
    if (isFALSE(paired)) {
      .f <- stats::kruskal.test
      .f.args <- list(formula = rlang::new_formula(y, x))
      .f.es <- effectsize::rank_epsilon_squared
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
    sample_size <- ifelse(isTRUE(paired), length(unique(data$rowid)), nrow(data))
  }

  # ----------------------- robust ---------------------------------------

  if (stats.type == "robust") {
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

    # expression details
    c(no.parameters, sample_size, k.parameter, k.parameter2) %<-% c(2L, nrow(data), 0L, k)
  }

  # final returns
  if (stats.type != "bayes") {
    # preparing the expression
    if (stats.type == "robust" && isTRUE(paired)) {
      expression <-
        substitute(
          expr = paste(
            italic("F")["trimmed-means"],
            "(",
            df1,
            ",",
            df2,
            ") = ",
            statistic,
            ", ",
            italic("p"),
            " = ",
            p.value,
            ", ",
            italic("n")["pairs"],
            " = ",
            n
          ),
          env = list(
            statistic = format_num(stats_df$statistic[[1]], k = k),
            df1 = format_num(stats_df$df[[1]], k = k),
            df2 = format_num(stats_df$df.error[[1]], k = k),
            p.value = format_num(stats_df$p.value[[1]], k = k, p.value = TRUE),
            n = .prettyNum(length(unique(data$rowid)))
          )
        )
    } else {
      expression <-
        expr_template(
          no.parameters = no.parameters,
          stats.df = stats_df,
          n = sample_size,
          paired = paired,
          conf.level = conf.level,
          k = k,
          k.parameter = k.parameter,
          k.parameter2 = k.parameter2
        )
    }
  }

  # ----------------------- Bayesian ---------------------------------------

  # running Bayesian t-test
  if (stats.type == "bayes") {
    stats_df <-
      tidyBF::bf_oneway_anova(
        data = data,
        x = {{ x }},
        y = {{ y }},
        subject.id = {{ subject.id }},
        paired = paired,
        bf.prior = bf.prior,
        k = k,
        output = output,
        ...
      )

    expression <- stats_df
  }

  # return the output
  switch(output, "dataframe" = as_tibble(stats_df), expression)
}
