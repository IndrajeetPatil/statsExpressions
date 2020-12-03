#' @title Expression containing parametric ANOVA results
#' @name expr_anova_parametric
#'
#' @return For more details, see-
#' \url{https://indrajeetpatil.github.io/statsExpressions/articles/stats_details.html}
#'
#' @description The effect sizes and their confidence intervals are computed
#'   using `effectsize::eta_squared` and `effectsize::omega_squared` functions.
#'
#' @inheritParams ipmisc::long_to_wide_converter
#' @param conf.level Scalar between 0 and 1. If unspecified, the defaults return
#'   `95%` confidence/credible intervals (`0.95`).
#' @param effsize.type Type of effect size needed for *parametric* tests. The
#'   argument can be `"eta"` (partial eta-squared) or `"omega"` (partial
#'   omega-squared).
#' @param output If `"expression"`, will return expression with statistical
#'   details, while `"dataframe"` will return a dataframe containing the
#'   results.
#' @inheritParams expr_corr_test
#' @inheritParams expr_template
#' @param ... Additional arguments (currently ignored).
#' @inheritParams stats::oneway.test
#'
#' @importFrom dplyr select rename matches
#' @importFrom rlang !! enquo eval_tidy expr ensym exec
#' @importFrom stats oneway.test
#' @importFrom ez ezANOVA
#' @importFrom effectsize effectsize
#' @importFrom ipmisc long_to_wide_converter specify_decimal_p
#'
#' @examples
#' # for reproducibility
#' set.seed(123)
#' library(statsExpressions)
#'
#' # -------------------- between-subjects ------------------------------
#'
#' # to get expression
#' expr_anova_parametric(
#'   data = ggplot2::msleep,
#'   x = vore,
#'   y = sleep_rem
#' )
#'
#' # -------------------- repeated measures ------------------------------
#'
#' # to get dataframe
#' expr_anova_parametric(
#'   data = iris_long,
#'   x = condition,
#'   y = value,
#'   subject.id = id,
#'   paired = TRUE,
#'   output = "dataframe"
#' )
#' @export

# function body
expr_anova_parametric <- function(data,
                                  x,
                                  y,
                                  subject.id = NULL,
                                  paired = FALSE,
                                  k = 2L,
                                  conf.level = 0.95,
                                  effsize.type = "omega",
                                  var.equal = FALSE,
                                  output = "expression",
                                  ...) {
  # make sure both quoted and unquoted arguments are allowed
  c(x, y) %<-% c(rlang::ensym(x), rlang::ensym(y))

  # for paired designs, variance is going to be equal across grouping levels
  if (isTRUE(paired)) var.equal <- TRUE

  # determine number of decimal places for both degrees of freedom
  k.df1 <- ifelse(isFALSE(paired), 0L, k)
  k.df2 <- ifelse(isFALSE(paired) && isTRUE(var.equal), 0L, k)

  # effsize text
  if (effsize.type == "omega") effsize.text <- quote(widehat(omega["p"]^2))
  if (effsize.type == "eta") effsize.text <- quote(widehat(eta["p"]^2))

  # --------------------- data preparation --------------------------------

  # have a proper cleanup with NA removal
  data %<>%
    ipmisc::long_to_wide_converter(
      data = .,
      x = {{ x }},
      y = {{ y }},
      subject.id = {{ subject.id }},
      paired = paired,
      spread = FALSE
    )

  # -------------- within-subjects design --------------------------------

  # properly removing NAs if it's a paired design
  if (isTRUE(paired)) {
    # sample size
    sample_size <- length(unique(data$rowid))
    n.text <- quote(italic("n")["pairs"])

    # run the ANOVA
    ez_df <-
      rlang::eval_tidy(rlang::expr(
        ez::ezANOVA(
          data = dplyr::mutate_if(.tbl = data, .predicate = is.character, .funs = as.factor) %>%
            dplyr::mutate(.data = ., rowid = as.factor(rowid)),
          dv = !!rlang::ensym(y),
          wid = rowid,
          within = !!rlang::ensym(x),
          detailed = TRUE,
          return_aov = TRUE
        )
      ))

    # no sphericity correction applied
    if (sample_size < nlevels(as.factor(data %>% dplyr::pull({{ x }})))) {
      c(k.df1, k.df2) %<-% c(0L, 0L)
      e_corr <- 1
      p.value <- ez_df$ANOVA$p[2]
    } else {
      e_corr <- ez_df$`Sphericity Corrections`$GGe
      p.value <- ez_df$`Sphericity Corrections`$`p[GG]`[[1]]
    }

    # combine into a dataframe
    stats_df <-
      tibble(
        statistic = ez_df$ANOVA$F[2],
        parameter1 = e_corr * ez_df$ANOVA$DFn[2],
        parameter2 = e_corr * ez_df$ANOVA$DFd[2],
        p.value = p.value
      )

    # creating a standardized dataframe with effect size and its CIs
    mod <- ez_df$aov
  }

  # ------------------- between-subjects design ------------------------------

  if (isFALSE(paired)) {
    # sample size
    sample_size <- nrow(data)
    n.text <- quote(italic("n")["obs"])

    # Welch's ANOVA run by default
    mod <-
      stats::oneway.test(
        formula = rlang::new_formula(y, x),
        data = data,
        na.action = na.omit,
        var.equal = var.equal
      )

    # tidy up the stats object
    stats_df <-
      suppressMessages(tidy_model_parameters(mod)) %>%
      dplyr::select(statistic, parameter1 = df, parameter2 = df.error, dplyr::everything())
  }

  # ------------------- effect size computation ------------------------------

  # computing effect size
  effsize_df <-
    suppressWarnings(rlang::exec(
      .fn = effectsize::effectsize,
      model = mod,
      type = effsize.type,
      ci = conf.level
    )) %>%
    parameters::standardize_names(data = ., style = "broom")

  # test details
  if (isTRUE(paired) || isTRUE(var.equal)) {
    statistic.text <- quote(italic("F")["Fisher"])
  } else {
    statistic.text <- quote(italic("F")["Welch"])
  }

  # combining dataframes
  stats_df <- dplyr::bind_cols(stats_df, effsize_df)

  # preparing expression
  expression <-
    expr_template(
      no.parameters = 2L,
      stats.df = stats_df,
      statistic.text = statistic.text,
      effsize.text = effsize.text,
      n = sample_size,
      n.text = n.text,
      conf.level = conf.level,
      k = k,
      k.parameter = k.df1,
      k.parameter2 = k.df2
    )

  # return the output
  switch(output, "dataframe" = stats_df, expression)
}

#' @title Making text expression for non-parametric ANOVA.
#' @name expr_anova_nonparametric
#'
#' @details For paired designs, the effect size is Kendall's coefficient of
#'   concordance (*W*), while for between-subjects designs, the effect size is
#'   epsilon-squared (for more, see `?rcompanion::epsilonSquared` and
#'   `?rcompanion::kendallW`).
#'
#' @return For more details, see-
#' \url{https://indrajeetpatil.github.io/statsExpressions/articles/stats_details.html}
#'
#' @param conf.type A vector of character strings representing the type of
#'   intervals required. The value should be any subset of the values `"norm"`,
#'   `"basic"`, `"perc"`, `"bca"`. For more, see `?boot::boot.ci`.
#' @param nboot Number of bootstrap samples for computing confidence interval
#'   for the effect size (Default: `100`).
#' @inheritParams expr_anova_parametric
#' @inheritParams tidyBF::bf_oneway_anova
#' @inheritParams expr_template
#'
#' @importFrom dplyr select
#' @importFrom rlang !! enquo
#' @importFrom stats friedman.test kruskal.test na.omit
#' @importFrom rcompanion epsilonSquared kendallW
#'
#' @examples
#' # setup
#' set.seed(123)
#' library(statsExpressions)
#'
#' # -------------- within-subjects design --------------------------------
#'
#' # creating the expression
#' expr_anova_nonparametric(
#'   data = bugs_long,
#'   x = condition,
#'   y = desire,
#'   paired = TRUE,
#'   conf.level = 0.99,
#'   k = 2
#' )
#'
#' # -------------- between-subjects design --------------------------------
#'
#' expr_anova_nonparametric(
#'   data = ggplot2::msleep,
#'   x = vore,
#'   y = sleep_rem,
#'   paired = FALSE,
#'   conf.level = 0.99,
#'   conf.type = "perc"
#' )
#' @export

# function body
expr_anova_nonparametric <- function(data,
                                     x,
                                     y,
                                     subject.id = NULL,
                                     paired = FALSE,
                                     k = 2L,
                                     conf.level = 0.95,
                                     conf.type = "perc",
                                     nboot = 100L,
                                     output = "expression",
                                     ...) {

  # make sure both quoted and unquoted arguments are allowed
  c(x, y) %<-% c(rlang::ensym(x), rlang::ensym(y))

  # ============================ data preparation ==========================

  # have a proper cleanup with NA removal
  data %<>%
    ipmisc::long_to_wide_converter(
      data = .,
      x = {{ x }},
      y = {{ y }},
      subject.id = {{ subject.id }},
      paired = paired,
      spread = FALSE
    )

  # ------------------- within-subjects design ------------------------------

  # properly removing NAs if it's a paired design
  if (isTRUE(paired)) {
    # setting up the anova model (`y ~ x | id`) and getting its summary
    mod <-
      stats::friedman.test(
        formula = rlang::new_formula(
          {{ rlang::enexpr(y) }}, rlang::expr(!!rlang::enexpr(x) | rowid)
        ),
        data = data,
        na.action = na.omit
      )

    # details for expression creator
    .f <- rcompanion::kendallW
    arg_list <-
      list(
        x = dplyr::select(ipmisc::long_to_wide_converter(data, {{ x }}, {{ y }}), -rowid),
        correct = TRUE,
        na.rm = TRUE
      )
    sample_size <- length(unique(data$rowid))
    n.text <- quote(italic("n")["pairs"])
    statistic.text <- quote(chi["Friedman"]^2)
    effsize.text <- quote(widehat(italic("W"))["Kendall"])
  }

  # ------------------- between-subjects design ------------------------------

  if (isFALSE(paired)) {
    # setting up the anova model and getting its summary
    mod <-
      stats::kruskal.test(
        formula = rlang::new_formula({{ y }}, {{ x }}),
        data = data,
        na.action = na.omit
      )

    # details for expression creator
    .f <- rcompanion::epsilonSquared
    arg_list <-
      list(
        x = data %>% dplyr::pull({{ y }}),
        g = data %>% dplyr::pull({{ x }}),
        group = "row",
        reportIncomplete = FALSE
      )
    sample_size <- nrow(data)
    n.text <- quote(italic("n")["obs"])
    statistic.text <- quote(chi["Kruskal-Wallis"]^2)
    effsize.text <- quote(widehat(epsilon^2)["ordinal"])
  }

  # computing respective effect sizes
  effsize_df <-
    rlang::exec(
      .fn = .f,
      !!!arg_list,
      ci = TRUE,
      conf = conf.level,
      type = conf.type,
      R = nboot,
      digits = 5
    ) %>%
    rcompanion_cleaner(.)

  # combining dataframes
  stats_df <- dplyr::bind_cols(tidy_model_parameters(mod), effsize_df)

  # preparing expression
  expression <-
    expr_template(
      no.parameters = 1L,
      stats.df = stats_df,
      statistic.text = statistic.text,
      effsize.text = effsize.text,
      n = sample_size,
      n.text = n.text,
      conf.level = conf.level,
      k = k
    )

  # return the output
  switch(output, "dataframe" = stats_df, expression)
}

#' @title Expression containing results from heteroscedastic one-way ANOVA for
#'   trimmed means
#' @name expr_anova_robust
#'
#' @return For more details, see-
#' \url{https://indrajeetpatil.github.io/statsExpressions/articles/stats_details.html}
#'
#' @param tr Trim level for the mean when carrying out `robust` tests. If you
#'   get error stating "Standard error cannot be computed because of Winsorized
#'   variance of 0 (e.g., due to ties). Try to decrease the trimming level.",
#'   try to play around with the value of `tr`, which is by default set to
#'   `0.1`. Lowering the value might help.
#' @inheritParams expr_anova_nonparametric
#' @inheritParams expr_corr_test
#' @inheritParams expr_template
#'
#' @importFrom dplyr select
#' @importFrom rlang !! enquo ensym as_name
#' @importFrom WRS2 rmanova t1way
#'
#' @examples
#' # for reproducibility
#' set.seed(123)
#' library(statsExpressions)
#'
#' # ------------------------ between-subjects -----------------------------
#'
#' expr_anova_robust(
#'   data = ggplot2::midwest,
#'   x = state,
#'   y = percbelowpoverty
#' )
#'
#' # ------------------------ within-subjects -----------------------------
#'
#' expr_anova_robust(
#'   data = iris_long,
#'   x = condition,
#'   y = value,
#'   paired = TRUE,
#'   k = 3
#' )
#' @export

# function body
expr_anova_robust <- function(data,
                              x,
                              y,
                              subject.id = NULL,
                              paired = FALSE,
                              k = 2L,
                              conf.level = 0.95,
                              tr = 0.1,
                              nboot = 100L,
                              output = "expression",
                              ...) {

  # make sure both quoted and unquoted arguments are allowed
  c(x, y) %<-% c(rlang::ensym(x), rlang::ensym(y))

  # ============================ data preparation ==========================

  # have a proper cleanup with NA removal
  data %<>%
    ipmisc::long_to_wide_converter(
      data = .,
      x = {{ x }},
      y = {{ y }},
      subject.id = {{ subject.id }},
      paired = paired,
      spread = FALSE
    )

  # -------------- within-subjects design --------------------------------

  # properly removing NAs if it's a paired design
  if (isTRUE(paired)) {
    # sample size
    sample_size <- length(unique(data$rowid))

    # test
    mod <-
      WRS2::rmanova(
        y = data[[rlang::as_name(y)]],
        groups = data[[rlang::as_name(x)]],
        blocks = data[["rowid"]],
        tr = tr
      )

    # create a dataframe
    stats_df <-
      tibble(
        statistic = mod$test[[1]],
        parameter1 = mod$df1[[1]],
        parameter2 = mod$df2[[1]],
        p.value = mod$p.value[[1]]
      )

    # preparing the expression
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
          statistic = specify_decimal_p(x = stats_df$statistic[[1]], k = k),
          df1 = specify_decimal_p(x = stats_df$parameter1[[1]], k = k),
          df2 = specify_decimal_p(x = stats_df$parameter2[[1]], k = k),
          p.value = specify_decimal_p(x = stats_df$p.value[[1]], k = k, p.value = TRUE),
          n = sample_size
        )
      )
  }

  # -------------- between-subjects design --------------------------------

  if (isFALSE(paired)) {
    # sample size
    sample_size <- nrow(data)
    n.text <- quote(italic("n")["obs"])

    # heteroscedastic one-way ANOVA for trimmed means
    mod <-
      WRS2::t1way(
        formula = rlang::new_formula({{ y }}, {{ x }}),
        data = data,
        tr = tr,
        alpha = 1 - conf.level,
        nboot = nboot
      )

    # create a dataframe
    stats_df <-
      tibble(
        statistic = mod$test[[1]],
        parameter1 = mod$df1[[1]],
        parameter2 = mod$df2[[1]],
        p.value = mod$p.value[[1]],
        estimate = mod$effsize[[1]],
        conf.low = mod$effsize_ci[[1]],
        conf.high = mod$effsize_ci[[2]]
      )

    # preparing expression
    expression <-
      expr_template(
        no.parameters = 2L,
        stats.df = stats_df,
        statistic.text = quote(italic("F")["trimmed-means"]),
        effsize.text = quote(widehat(italic(xi))),
        n = sample_size,
        n.text = n.text,
        conf.level = conf.level,
        k = k,
        k.parameter2 = k
      )
  }

  # return the output
  switch(output, "dataframe" = stats_df, expression)
}


#' @title Expression containing Bayesian one-way ANOVA results
#' @name expr_anova_bayes
#'
#' @return For more details, see-
#' \url{https://indrajeetpatil.github.io/statsExpressions/articles/stats_details.html}
#'
#' @inheritParams expr_anova_parametric
#' @inheritParams expr_t_bayes
#'
#' @importFrom tidyBF bf_oneway_anova
#' @importFrom parameters model_parameters
#' @importFrom performance model_performance
#'
#' @examples
#' # setup
#' set.seed(123)
#' library(statsExpressions)
#'
#' expr_anova_bayes(
#'   data = ggplot2::msleep,
#'   x = vore,
#'   y = sleep_rem
#' )
#' @export

# function body
expr_anova_bayes <- function(data,
                             x,
                             y,
                             subject.id = NULL,
                             paired = FALSE,
                             bf.prior = 0.707,
                             k = 2L,
                             output = "expression",
                             ...) {
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
}
