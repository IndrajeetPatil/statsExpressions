#' @title Making expression containing parametric ANOVA results
#' @name expr_anova_parametric
#' @author \href{https://github.com/IndrajeetPatil}{Indrajeet Patil}
#'
#' @return For more details, see-
#' \url{https://indrajeetpatil.github.io/statsExpressions/articles/stats_details.html}
#'
#' @note For repeated measures designs (`paired = TRUE`), only omega-squared and
#'   partial eta-squared effect sizes are supported.
#'
#' @inheritParams t1way_ci
#' @param paired Logical that decides whether the experimental design is
#'   repeated measures/within-subjects or between-subjects. The default is
#'   `FALSE`.
#' @param effsize.type Type of effect size needed for *parametric* tests. The
#'   argument can be `"biased"` (equivalent to `"d"` for Cohen's *d* for
#'   **t-test**; `"partial_eta"` for partial eta-squared for **anova**) or
#'   `"unbiased"` (equivalent to `"g"` Hedge's *g* for **t-test**;
#'   `"partial_omega"` for partial omega-squared for **anova**)).
#' @param sphericity.correction Logical that decides whether to apply correction
#'   to account for violation of sphericity in a repeated measures design ANOVA
#'   (Default: `TRUE`).
#' @param messages Decides whether messages references, notes, and warnings are
#'   to be displayed (Default: `TRUE`).
#' @inheritParams expr_template
#' @param ... Additional arguments (currently ignored).
#' @inheritParams stats::oneway.test
#' @inheritParams groupedstats::lm_effsize_standardizer
#'
#'
#' @importFrom dplyr select rename matches
#' @importFrom rlang !! enquo eval_tidy expr ensym
#' @importFrom stats aov oneway.test na.omit
#' @importFrom ez ezANOVA
#'
#' @examples
#' \donttest{
#' # for reproducibility
#' set.seed(123)
#' library(statsExpressions)
#'
#' # -------------------- between-subjects ------------------------------
#'
#' # with defaults
#' statsExpressions::expr_anova_parametric(
#'   data = ggplot2::msleep,
#'   x = vore,
#'   y = sleep_rem,
#'   paired = FALSE,
#'   k = 3
#' )
#'
#' # modifying the defaults
#' statsExpressions::expr_anova_parametric(
#'   data = ggplot2::msleep,
#'   x = vore,
#'   y = sleep_rem,
#'   paired = FALSE,
#'   effsize.type = "biased",
#'   partial = FALSE,
#'   var.equal = TRUE,
#'   nboot = 10
#' )
#'
#' # -------------------- repeated measures ------------------------------
#'
#' statsExpressions::expr_anova_parametric(
#'   data = iris_long,
#'   x = condition,
#'   y = value,
#'   paired = TRUE,
#'   k = 4,
#'   nboot = 10
#' )
#' }
#' @export

# function body
expr_anova_parametric <- function(data,
                                  x,
                                  y,
                                  paired = FALSE,
                                  effsize.type = "unbiased",
                                  partial = TRUE,
                                  conf.level = 0.95,
                                  nboot = 100,
                                  var.equal = FALSE,
                                  sphericity.correction = TRUE,
                                  k = 2,
                                  stat.title = NULL,
                                  messages = TRUE,
                                  ...) {

  # make sure both quoted and unquoted arguments are allowed
  c(x, y) %<-% c(rlang::ensym(x), rlang::ensym(y))

  # for paired designs, variance is going to be equal across grouping levels
  if (isTRUE(paired)) var.equal <- TRUE
  if (isFALSE(paired)) sphericity.correction <- FALSE

  # determine number of decimal places for both degrees of freedom
  k.df1 <- ifelse(isTRUE(paired) && isTRUE(sphericity.correction), k, 0L)
  k.df2 <- ifelse(isTRUE(var.equal) && isFALSE(sphericity.correction), 0L, k)

  # figuring out which effect size to use
  effsize.type <- effsize_type_switch(effsize.type)

  # some of the effect sizes don't work properly for paired designs
  if (isTRUE(paired)) partial <- ifelse(effsize.type == "unbiased", FALSE, TRUE)

  # omega
  if (effsize.type == "unbiased") {
    effsize <- "omega"
    if (isTRUE(partial)) {
      effsize.text <- quote(widehat(omega["p"]^2))
    } else {
      effsize.text <- quote(widehat(omega^2))
    }
  }

  # eta
  if (effsize.type == "biased") {
    effsize <- "eta"
    if (isTRUE(partial)) {
      effsize.text <- quote(widehat(eta["p"]^2))
    } else {
      effsize.text <- quote(widehat(eta^2))
    }
  }

  # creating a dataframe
  data %<>%
    dplyr::select(.data = ., {{ x }}, {{ y }}) %>%
    dplyr::mutate(.data = ., {{ x }} := droplevels(as.factor({{ x }}))) %>%
    tibble::as_tibble(x = .)

  # -------------- within-subjects design --------------------------------

  # properly removing NAs if it's a paired design
  if (isTRUE(paired)) {
    # converting to long format and then getting it back in wide so that the
    # rowid variable can be used as the block variable
    data %<>% df_cleanup_paired(data = ., x = {{ x }}, y = {{ y }})

    # sample size
    sample_size <- length(unique(data$rowid))
    n.text <- quote(italic("n")["pairs"])

    # warn the user if
    if (sample_size < nlevels(as.factor(data %>% dplyr::pull({{ x }})))) {
      # no sphericity correction applied; adjust expr display accordingly
      c(k.df1, k.df2, sphericity.correction) %<-% c(0L, 0L, FALSE)

      # inform the user
      message(cat(
        crayon::red("Warning: "),
        crayon::blue("No. of factor levels is greater than no. of observations per cell.\n"),
        crayon::blue("No sphericity correction applied. Interpret the results with caution.\n")
      ),
      sep = ""
      )
    }

    # run the ANOVA
    ez_df <-
      rlang::eval_tidy(rlang::expr(
        ez::ezANOVA(
          data = data %>%
            dplyr::mutate_if(
              .tbl = .,
              .predicate = is.character,
              .funs = as.factor
            ) %>%
            dplyr::mutate(.data = ., rowid = as.factor(rowid)),
          dv = !!rlang::ensym(y),
          wid = rowid,
          within = !!rlang::ensym(x),
          detailed = TRUE,
          return_aov = TRUE
        )
      ))

    # list with results
    if (isTRUE(sphericity.correction)) {
      e_corr <- ez_df$`Sphericity Corrections`$GGe
      stats_df <-
        tibble::as_tibble(cbind.data.frame(
          statistic = ez_df$ANOVA$F[2],
          parameter1 = e_corr * ez_df$ANOVA$DFn[2],
          parameter2 = e_corr * ez_df$ANOVA$DFd[2],
          p.value = ez_df$`Sphericity Corrections`$`p[GG]`[[1]]
        ))
    } else {
      stats_df <-
        tibble::as_tibble(cbind.data.frame(
          statistic = ez_df$ANOVA$F[2],
          parameter1 = ez_df$ANOVA$DFn[2],
          parameter2 = ez_df$ANOVA$DFd[2],
          p.value = ez_df$ANOVA$p[2]
        ))
    }

    # creating a standardized dataframe with effect size and its CIs
    effsize_object <- ez_df$aov
  }

  # ------------------- between-subjects design ------------------------------

  if (isFALSE(paired)) {
    # remove NAs listwise for between-subjects design
    data %<>% tidyr::drop_na(data = .)

    # sample size
    sample_size <- nrow(data)
    n.text <- quote(italic("n")["obs"])

    # Welch's ANOVA run by default
    stats_obj <-
      stats::oneway.test(
        formula = rlang::new_formula({{ y }}, {{ x }}),
        data = data,
        subset = NULL,
        na.action = na.omit,
        var.equal = var.equal
      )

    # tidy up the stats object
    stats_df <-
      suppressMessages(broomExtra::tidy(stats_obj)) %>%
      dplyr::rename(
        .data = .,
        parameter1 = dplyr::matches("^num"),
        parameter2 = dplyr::matches("^den")
      )

    # creating a standardized dataframe with effect size and its CIs
    effsize_object <-
      stats::aov(
        formula = rlang::new_formula({{ y }}, {{ x }}),
        data = data,
        na.action = na.omit
      )
  }

  # creating a standardized dataframe with effect size and its CIs
  effsize_df <-
    aov_effsize(
      model = effsize_object,
      effsize = effsize,
      partial = partial,
      ci = conf.level,
      iterations = nboot
    )

  # preparing subtitle
  subtitle <-
    expr_template(
      stat.title = stat.title,
      no.parameters = 2L,
      stats.df = stats_df,
      effsize.df = effsize_df,
      statistic.text = quote(italic("F")),
      effsize.text = effsize.text,
      n = sample_size,
      n.text = n.text,
      conf.level = conf.level,
      k = k,
      k.parameter = k.df1,
      k.parameter2 = k.df2
    )

  # message about effect size measure
  if (isTRUE(messages)) effsize_ci_message(nboot, conf.level)

  # return the subtitle
  return(subtitle)
}

#' @title Making text subtitle for nonparametric ANOVA.
#' @name expr_anova_nonparametric
#' @author \href{https://github.com/IndrajeetPatil}{Indrajeet Patil}
#'
#' @details For paired designs, the effect size is Kendall's coefficient of
#'   concordance (*W*), while for between-subjects designs, the effect size is
#'   epsilon-squared (for more, see `?rcompanion::epsilonSquared` and
#'   `?rcompanion::kendallW`).
#'
#' @return For more details, see-
#' \url{https://indrajeetpatil.github.io/statsExpressions/articles/stats_details.html}
#'
#' @inheritParams t1way_ci
#' @inheritParams expr_anova_parametric
#' @inheritParams expr_template
#'
#' @importFrom dplyr select
#' @importFrom rlang !! enquo
#' @importFrom stats friedman.test kruskal.test
#' @importFrom broomExtra tidy
#' @importFrom rcompanion epsilonSquared kendallW
#'
#' @examples
#' \donttest{
#' # setup
#' set.seed(123)
#' library(statsExpressions)
#'
#' # -------------- within-subjects design --------------------------------
#'
#' # creating the subtitle
#' statsExpressions::expr_anova_nonparametric(
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
#' statsExpressions::expr_anova_nonparametric(
#'   data = ggplot2::msleep,
#'   x = vore,
#'   y = sleep_rem,
#'   paired = FALSE,
#'   conf.level = 0.99,
#'   conf.type = "perc"
#' )
#' }
#' @export

# function body
expr_anova_nonparametric <- function(data,
                                     x,
                                     y,
                                     paired = FALSE,
                                     conf.type = "perc",
                                     conf.level = 0.95,
                                     k = 2,
                                     nboot = 100,
                                     stat.title = NULL,
                                     messages = TRUE,
                                     ...) {

  # make sure both quoted and unquoted arguments are allowed
  c(x, y) %<-% c(rlang::ensym(x), rlang::ensym(y))

  # creating a dataframe
  data %<>%
    dplyr::select(.data = ., {{ x }}, {{ y }}) %>%
    dplyr::mutate(.data = ., {{ x }} := droplevels(as.factor({{ x }}))) %>%
    tibble::as_tibble(x = .)

  # ------------------- within-subjects design ------------------------------

  # properly removing NAs if it's a paired design
  if (isTRUE(paired)) {

    # converting to long format and then getting it back in wide so that the
    # rowid variable can be used as the block variable
    data %<>% df_cleanup_paired(data = ., x = {{ x }}, y = {{ y }})

    # setting up the anova model (`y ~ x | id`) and getting its summary
    stats_df <-
      broomExtra::tidy(
        stats::friedman.test(
          formula = rlang::new_formula(
            {{ rlang::enexpr(y) }}, rlang::expr(!!rlang::enexpr(x) | rowid)
          ),
          data = data,
          na.action = na.omit
        )
      )

    # details for expression creator
    .f <- rcompanion::kendallW
    arg_list <- list(
      x = dplyr::select(long_to_wide_converter(data, {{ x }}, {{ y }}), -rowid),
      correct = TRUE,
      na.rm = TRUE
    )
    sample_size <- length(unique(data$rowid))
    n.text <- quote(italic("n")["pairs"])
    effsize.text <- quote(widehat(italic("W"))["Kendall"])
  }

  # ------------------- between-subjects design ------------------------------

  if (isFALSE(paired)) {
    # remove NAs listwise for between-subjects design
    data %<>% tidyr::drop_na(data = .)

    # setting up the anova model and getting its summary
    stats_df <-
      broomExtra::tidy(
        stats::kruskal.test(
          formula = rlang::new_formula({{ y }}, {{ x }}),
          data = data,
          na.action = na.omit
        )
      )

    # details for expression creator
    .f <- rcompanion::epsilonSquared
    arg_list <- list(
      x = data %>% dplyr::pull({{ y }}),
      g = data %>% dplyr::pull({{ x }}),
      group = "row",
      reportIncomplete = FALSE
    )
    sample_size <- nrow(data)
    n.text <- quote(italic("n")["obs"])
    effsize.text <- quote(widehat(epsilon^2))
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
      histogram = FALSE,
      digits = 5
    ) %>%
    rcompanion_cleaner(.)

  # message about effect size measure
  if (isTRUE(messages)) effsize_ci_message(nboot, conf.level)

  # preparing subtitle
  subtitle <-
    expr_template(
      stat.title = stat.title,
      no.parameters = 1L,
      stats.df = stats_df,
      effsize.df = effsize_df,
      statistic.text = quote(italic(chi)^2),
      effsize.text = effsize.text,
      n = sample_size,
      n.text = n.text,
      conf.level = conf.level,
      k = k
    )

  # return the subtitle
  return(subtitle)
}

#' @title Expression containing results from heteroscedastic one-way ANOVA for
#'   trimmed means
#' @name expr_anova_robust
#' @author \href{https://github.com/IndrajeetPatil}{Indrajeet Patil}
#'
#' @return For more details, see-
#' \url{https://indrajeetpatil.github.io/statsExpressions/articles/stats_details.html}
#'
#' @inheritParams t1way_ci
#' @inheritParams expr_anova_nonparametric
#' @inheritParams expr_template
#'
#' @importFrom dplyr select
#' @importFrom rlang !! enquo ensym as_name
#' @importFrom WRS2 rmanova
#'
#' @examples
#'
#' \donttest{
#' # for reproducibility
#' set.seed(123)
#'
#' # ------------------------ between-subjects -----------------------------
#'
#' # going with the defaults
#' statsExpressions::expr_anova_robust(
#'   data = ggplot2::midwest,
#'   x = state,
#'   y = percbelowpoverty,
#'   paired = FALSE,
#'   nboot = 10
#' )
#'
#' # changing defaults
#' expr_anova_robust(
#'   data = ggplot2::midwest,
#'   x = state,
#'   y = percollege,
#'   paired = FALSE,
#'   conf.level = 0.99,
#'   tr = 0.2,
#'   nboot = 10
#' )
#'
#' # ------------------------ within-subjects -----------------------------
#'
#' statsExpressions::expr_anova_robust(
#'   data = iris_long,
#'   x = condition,
#'   y = value,
#'   paired = TRUE,
#'   tr = 0.2,
#'   k = 3,
#'   nboot = 10
#' )
#' }
#' @export

# function body
expr_anova_robust <- function(data,
                              x,
                              y,
                              paired = FALSE,
                              tr = 0.1,
                              nboot = 100,
                              conf.level = 0.95,
                              conf.type = "norm",
                              k = 2,
                              stat.title = NULL,
                              messages = TRUE,
                              ...) {

  # make sure both quoted and unquoted arguments are allowed
  c(x, y) %<-% c(rlang::ensym(x), rlang::ensym(y))

  # creating a dataframe
  data %<>%
    dplyr::select(.data = ., {{ x }}, {{ y }}) %>%
    dplyr::mutate(.data = ., {{ x }} := droplevels(as.factor({{ x }}))) %>%
    tibble::as_tibble(x = .)

  # -------------- within-subjects design --------------------------------

  # properly removing NAs if it's a paired design
  if (isTRUE(paired)) {
    # converting to long format and then getting it back in wide so that the
    # rowid variable can be used as the block variable
    data %<>% df_cleanup_paired(data = ., x = {{ x }}, y = {{ y }})

    # sample size
    sample_size <- length(unique(data$rowid))

    # test
    stats_df <-
      WRS2::rmanova(
        y = data[[rlang::as_name(y)]],
        groups = data[[rlang::as_name(x)]],
        blocks = data[["rowid"]],
        tr = tr
      )

    # preparing the subtitle
    subtitle <-
      substitute(
        expr = paste(
          italic("F"),
          "(",
          df1,
          ",",
          df2,
          ") = ",
          estimate,
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
          estimate = specify_decimal_p(x = stats_df$test[[1]], k = k),
          df1 = specify_decimal_p(x = stats_df$df1[[1]], k = k),
          df2 = specify_decimal_p(x = stats_df$df2[[1]], k = k),
          p.value = specify_decimal_p(x = stats_df$p.value[[1]], k = k, p.value = TRUE),
          n = sample_size
        )
      )
  }

  # -------------- between-subjects design --------------------------------

  if (isFALSE(paired)) {
    # remove NAs listwise for between-subjects design
    data %<>% tidyr::drop_na(data = .)

    # sample size
    sample_size <- nrow(data)
    n.text <- quote(italic("n")["obs"])

    # setting up the Bootstrap version of the heteroscedastic one-way ANOVA for
    # trimmed means
    stats_df <-
      t1way_ci(
        data = data,
        x = {{ x }},
        y = {{ y }},
        tr = tr,
        nboot = nboot,
        conf.level = conf.level,
        conf.type = conf.type
      )

    # effect size dataframe
    effsize_df <- stats_df

    # preparing subtitle
    subtitle <-
      expr_template(
        no.parameters = 2L,
        stat.title = stat.title,
        stats.df = stats_df,
        effsize.df = effsize_df,
        statistic.text = quote(italic("F")),
        effsize.text = quote(widehat(italic(xi))),
        n = sample_size,
        n.text = n.text,
        conf.level = conf.level,
        k = k,
        k.parameter2 = k
      )

    # message about effect size measure
    if (isTRUE(messages)) effsize_ci_message(nboot, conf.level)
  }

  # return the subtitle
  return(subtitle)
}


#' @title Making expression containing Bayesian one-way ANOVA results.
#' @name expr_anova_bayes
#' @author \href{https://github.com/IndrajeetPatil}{Indrajeet Patil}
#'
#' @return For more details, see-
#' \url{https://indrajeetpatil.github.io/statsExpressions/articles/stats_details.html}
#'
#' @inheritParams expr_anova_parametric
#' @inheritParams expr_t_bayes
#'
#' @importFrom dplyr select
#' @importFrom rlang !! enquo
#' @importFrom stats lm oneway.test na.omit
#'
#' @examples
#' \donttest{
#' set.seed(123)
#'
#' # between-subjects ---------------------------------------
#' # with defaults
#' statsExpressions::expr_anova_bayes(
#'   data = ggplot2::msleep,
#'   x = vore,
#'   y = sleep_rem
#' )
#'
#' # modifying the defaults
#' statsExpressions::expr_anova_bayes(
#'   data = ggplot2::msleep,
#'   x = vore,
#'   y = sleep_rem,
#'   k = 3,
#'   bf.prior = 0.8
#' )
#'
#' # repeated measures ---------------------------------------
#' statsExpressions::expr_anova_bayes(
#'   data = WRS2::WineTasting,
#'   x = Wine,
#'   y = Taste,
#'   paired = TRUE,
#'   k = 4
#' )
#' }
#' @export

# function body
expr_anova_bayes <- function(data,
                             x,
                             y,
                             paired = FALSE,
                             bf.prior = 0.707,
                             k = 2,
                             ...) {

  # make sure both quoted and unquoted arguments are allowed
  c(x, y) %<-% c(rlang::ensym(x), rlang::ensym(y))

  # creating a dataframe
  data %<>%
    dplyr::select(.data = ., {{ x }}, {{ y }}) %>%
    dplyr::mutate(.data = ., {{ x }} := droplevels(as.factor({{ x }}))) %>%
    tibble::as_tibble(x = .)

  # properly removing NAs if it's a paired design
  # converting to long format and then getting it back in wide so that the
  # rowid variable can be used as the block variable
  if (isTRUE(paired)) data %<>% df_cleanup_paired(data = ., x = {{ x }}, y = {{ y }})

  # remove NAs listwise for between-subjects design
  if (isFALSE(paired)) data %<>% tidyr::drop_na(.)

  # bayes factor results
  subtitle <-
    bf_oneway_anova(
      data = data,
      x = {{ x }},
      y = {{ y }},
      paired = paired,
      bf.prior = bf.prior,
      k = k,
      caption = NULL,
      output = "h1"
    )

  # return the subtitle
  return(subtitle)
}
