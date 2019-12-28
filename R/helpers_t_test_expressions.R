#' @title Making expression containing *t*-test results
#' @name expr_t_parametric
#' @author \href{https://github.com/IndrajeetPatil}{Indrajeet Patil}, Chuck Powell
#'
#' @param effsize.type Type of effect size needed for *parametric* tests. The
#'   argument can be `"biased"` (`"d"` for Cohen's *d*) or `"unbiased"`
#'   (`"g"` Hedge's *g* for **t-test**). The default is `"g"`.
#' @param effsize.noncentral Logical indicating whether to use non-central
#'   *t*-distributions for computing the confidence interval for Cohen's *d*
#'   or Hedge's *g* (Default: `TRUE`).
#' @inheritParams expr_anova_parametric
#' @inheritParams stats::t.test
#' @inheritParams expr_template
#'
#' @importFrom dplyr select mutate_at matches vars starts_with ends_with
#' @importFrom rlang !! enquo ensym
#' @importFrom stats t.test na.omit qt pt uniroot
#'
#' @return Expression containing details from results of a two-sample test and
#'   effect size plus confidence intervals.
#'
#' @references For more details, see-
#' \url{https://indrajeetpatil.github.io/statsExpressions/articles/stats_details.html}
#'
#' @details Cohen's *d* is calculated in the traditional fashion as the
#'   difference between means or mean minus *mu* divided by the estimated
#'   standardized deviation.  By default Hedge's correction is applied
#'   (*N*-3)/(*N*-2.25) to produce *g*. For independent samples *t*-test, there
#'   are two possibilities implemented. If the *t*-test did not make a
#'   homogeneity of variance assumption, (the Welch test), the variance term
#'   will mirror the Welch test, otherwise a pooled and weighted estimate is
#'   used. If a paired samples *t*-test was requested, then effect size desired
#'   is based on the standard deviation of the differences.
#'
#'   The computation of the confidence intervals defaults to a use of
#'   non-central Student-*t* distributions (`effsize.noncentral = TRUE`);
#'   otherwise a central distribution is used.
#'
#'   When computing confidence intervals the variance of the effect size *d* or
#'   *g* is computed using the conversion formula reported in Cooper et al.
#'   (2009)
#'
#'   - `((n1+n2)/(n1*n2) + .5*d^2/df) * ((n1+n2)/df)` (independent samples)
#'
#'   - `sqrt(((1 / n) + (d^2 / n)) * 2 * (1 - r))`  (paired case)
#'
#' @examples
#' # for reproducibility
#' set.seed(123)
#' \donttest{
#' # creating a smaller dataset
#' msleep_short <- dplyr::filter(
#'   .data = ggplot2::msleep,
#'   vore %in% c("carni", "herbi")
#' )
#'
#' # with defaults
#' statsExpressions::expr_t_parametric(
#'   data = msleep_short,
#'   x = vore,
#'   y = sleep_rem
#' )
#'
#' # changing defaults
#' statsExpressions::expr_t_parametric(
#'   data = msleep_short,
#'   x = vore,
#'   y = sleep_rem,
#'   var.equal = TRUE,
#'   k = 2,
#'   effsize.type = "d"
#' )
#' }
#' @export

# function body
expr_t_parametric <- function(data,
                              x,
                              y,
                              paired = FALSE,
                              effsize.type = "g",
                              effsize.noncentral = TRUE,
                              conf.level = 0.95,
                              var.equal = FALSE,
                              k = 2,
                              stat.title = NULL,
                              ...) {

  # make sure both quoted and unquoted arguments are supported
  x <- rlang::ensym(x)
  y <- rlang::ensym(y)

  # creating a dataframe
  data %<>%
    dplyr::select(.data = ., {{ x }}, {{ y }}) %>%
    dplyr::mutate(.data = ., {{ x }} := droplevels(as.factor({{ x }}))) %>%
    tibble::as_tibble(x = .)

  # properly removing NAs if it's a paired design
  if (isTRUE(paired)) {
    data %<>% df_cleanup_paired(data = ., x = {{ x }}, y = {{ y }})

    # sample size
    sample_size <- length(unique(data$rowid))
    n.text <- quote(italic("n")["pairs"])
  }

  # remove NAs listwise for between-subjects design
  if (isFALSE(paired)) {
    data %<>% tidyr::drop_na(data = .)

    # sample size
    sample_size <- nrow(data)
    n.text <- quote(italic("n")["obs"])
  }

  # deciding which effect size to use (Hedge's g or Cohen's d)
  if (effsize.type %in% c("unbiased", "g")) {
    hedges.correction <- TRUE
    effsize.text <- quote(widehat(italic("g")))
  } else {
    hedges.correction <- FALSE
    effsize.text <- quote(widehat(italic("d")))
  }

  # setting up the t-test model and getting its summary
  tobject <-
    stats::t.test(
      formula = rlang::new_formula({{ y }}, {{ x }}),
      data = data,
      paired = paired,
      alternative = "two.sided",
      var.equal = var.equal,
      na.action = na.omit
    )

  # tidy dataframe from model object
  stats_df <- broomExtra::tidy(tobject)

  # effect size object
  effsize_df <-
    effsize_t_parametric(
      formula = rlang::new_formula({{ y }}, {{ x }}),
      data = data,
      paired = paired,
      hedges.correction = hedges.correction,
      conf.level = conf.level,
      noncentral = effsize.noncentral,
      var.equal = var.equal,
      tobject = tobject
    )

  # when paired samples t-test is run df is going to be integer
  # ditto for when variance is assumed to be equal
  k.df <- ifelse(isTRUE(paired) || isTRUE(var.equal), 0L, k)

  # preparing subtitle
  subtitle <-
    expr_template(
      no.parameters = 1L,
      stat.title = stat.title,
      stats.df = stats_df,
      effsize.df = effsize_df,
      statistic.text = quote(italic("t")),
      effsize.text = effsize.text,
      n = sample_size,
      conf.level = conf.level,
      k = k,
      k.parameter = k.df,
      n.text = n.text
    )

  # return the subtitle
  return(subtitle)
}


#' @title Making expression for Mann-Whitney *U*-test/Wilcoxon test results
#' @author \href{https://github.com/IndrajeetPatil}{Indrajeet Patil}, Chuck Powell
#'
#' @inheritParams expr_anova_parametric
#' @inheritParams expr_t_parametric
#' @inheritParams t1way_ci
#'
#' @importFrom dplyr select
#' @importFrom rlang !! enquo exec
#' @importFrom stats wilcox.test
#' @importFrom psych corr.test
#' @importFrom rcompanion wilcoxonR wilcoxonPairedR
#'
#' @details For the two independent samples case, the Mann-Whitney *U*-test is
#'   calculated and *W* is reported from *stats::wilcox.test*. For the paired
#'   samples case the Wilcoxon signed rank test is run and *V* is reported.
#'
#'   Since there is no single commonly accepted method for reporting effect size
#'   for these tests we are computing and reporting *r* (computed as
#'   \eqn{Z/\sqrt{N}}) along with the confidence intervals associated with the
#'   estimate. Note that *N* here corresponds to total *sample size* for
#'   independent/between-subjects designs, and to total number of *pairs* (and
#'   **not** *observations*) for repeated measures/within-subjects designs.
#'
#'   *Note:* The *stats::wilcox.test* function does not follow the
#'   same convention as *stats::t.test*. The sign of the *V* test statistic
#'   will always be positive since it is **the sum of the positive signed ranks**.
#'   Therefore *V* will vary in magnitude but not significance based solely
#'   on the order of the grouping variable. Consider manually
#'   reordering your factor levels if appropriate as shown in the second example
#'   below.
#'
#' @references For more details, see-
#' \url{https://indrajeetpatil.github.io/statsExpressions/articles/stats_details.html}
#'
#' @examples
#'
#' \donttest{
#' # for reproducibility
#' set.seed(123)
#'
#' # -------------- between-subjects design ------------------------
#' # simple function call
#' statsExpressions::expr_t_nonparametric(
#'   data = sleep,
#'   x = group,
#'   y = extra
#' )
#'
#' # creating a smaller dataset
#' msleep_short <- dplyr::filter(
#'   .data = ggplot2::msleep,
#'   vore %in% c("carni", "herbi")
#' )
#'
#' # modifying few things
#' statsExpressions::expr_t_nonparametric(
#'   data = msleep_short,
#'   x = vore,
#'   y = sleep_rem,
#'   nboot = 200,
#'   conf.level = 0.99,
#'   conf.type = "bca"
#' )
#'
#' # The order of the grouping factor matters when computing *V*
#' # Changing default alphabetical order manually
#' msleep_short$vore <- factor(msleep_short$vore,
#'   levels = c("herbi", "carni")
#' )
#'
#' # note the change in the reported *V* value but the identical
#' # value for *p* and the reversed effect size
#' statsExpressions::expr_t_nonparametric(
#'   data = msleep_short,
#'   x = vore,
#'   y = sleep_rem
#' )
#'
#' # -------------- within-subjects design ------------------------
#' # using dataset included in the package
#' statsExpressions::expr_t_nonparametric(
#'   data = VR_dilemma,
#'   x = modality,
#'   y = score,
#'   paired = TRUE,
#'   conf.level = 0.90,
#'   conf.type = "perc",
#'   nboot = 200,
#'   k = 5
#' )
#' }
#' @export

# function body
expr_t_nonparametric <- function(data,
                                 x,
                                 y,
                                 paired = FALSE,
                                 k = 2,
                                 conf.level = 0.95,
                                 conf.type = "norm",
                                 nboot = 100,
                                 stat.title = NULL,
                                 messages = TRUE,
                                 ...) {

  # make sure both quoted and unquoted arguments are supported
  x <- rlang::ensym(x)
  y <- rlang::ensym(y)

  # creating a dataframe
  data %<>%
    dplyr::select(.data = ., {{ x }}, {{ y }}) %>%
    dplyr::mutate(.data = ., {{ x }} := droplevels(as.factor({{ x }}))) %>%
    tibble::as_tibble(x = .)

  # properly removing NAs if it's a paired design
  if (isTRUE(paired)) {
    data %<>% df_cleanup_paired(data = ., x = {{ x }}, y = {{ y }})

    # sample size
    sample_size <- length(unique(data$rowid))
    n.text <- quote(italic("n")["pairs"])
    .f <- rcompanion::wilcoxonPairedR
    statistic.text <- quote("log"["e"](italic("V")))
  }

  # remove NAs listwise for between-subjects design
  if (isFALSE(paired)) {
    data %<>% tidyr::drop_na(data = .)

    # sample size
    sample_size <- nrow(data)
    n.text <- quote(italic("n")["obs"])
    .f <- rcompanion::wilcoxonR
    statistic.text <- quote("log"["e"](italic("W")))
  }

  # setting up the test and getting its summary
  stats_df <-
    broomExtra::tidy(stats::wilcox.test(
      formula = rlang::new_formula({{ y }}, {{ x }}),
      data = data,
      paired = paired,
      alternative = "two.sided",
      na.action = na.omit,
      exact = FALSE,
      correct = TRUE,
      conf.int = TRUE,
      conf.level = conf.level
    )) %>%
    dplyr::mutate(.data = ., statistic = log(statistic))

  # computing effect size
  effsize_df <-
    rlang::exec(
      .fn = .f,
      x = data %>% dplyr::pull({{ y }}),
      g = data %>% dplyr::pull({{ x }}),
      ci = TRUE,
      conf = conf.level,
      type = conf.type,
      R = nboot,
      histogram = FALSE,
      digits = k
    ) %>%
    rcompanion_cleaner(object = ., estimate.col = "r")

  # message about effect size measure
  if (isTRUE(messages)) effsize_ci_message(nboot, conf.level)

  # preparing subtitle
  subtitle <-
    expr_template(
      no.parameters = 0L,
      stats.df = stats_df,
      effsize.df = effsize_df,
      stat.title = stat.title,
      statistic.text = statistic.text,
      effsize.text = quote(widehat(italic("r"))),
      n = sample_size,
      n.text = n.text,
      conf.level = conf.level,
      k = k
    )

  # return the subtitle
  return(subtitle)
}

#' @title Expression containing results from a robust *t*-test
#' @name expr_t_robust
#' @author \href{https://github.com/IndrajeetPatil}{Indrajeet Patil}
#'
#' @references For more details, see-
#' \url{https://indrajeetpatil.github.io/statsExpressions/articles/stats_details.html}
#'
#' @inheritParams expr_t_parametric
#' @inheritParams yuend_ci
#' @inheritParams expr_anova_parametric
#'
#' @importFrom dplyr select
#' @importFrom rlang !! enquo
#' @importFrom WRS2 yuen yuen.effect.ci
#' @importFrom tibble tribble
#'
#' @examples
#' \donttest{
#' # for reproducibility
#' set.seed(123)
#'
#' # with defaults
#' statsExpressions::expr_t_robust(
#'   data = sleep,
#'   x = group,
#'   y = extra
#' )
#'
#' # changing defaults
#' statsExpressions::expr_t_robust(
#'   data = ToothGrowth,
#'   x = supp,
#'   y = len,
#'   nboot = 10,
#'   k = 1,
#'   tr = 0.2
#' )
#'
#' # within-subjects design
#' statsExpressions::expr_t_robust(
#'   data = dplyr::filter(
#'     statsExpressions::intent_morality,
#'     condition %in% c("accidental", "attempted"),
#'     harm == "Poisoning"
#'   ),
#'   x = condition,
#'   y = rating,
#'   paired = TRUE,
#'   nboot = 25
#' )
#' }
#' @export

# function body
expr_t_robust <- function(data,
                          x,
                          y,
                          tr = 0.1,
                          paired = FALSE,
                          nboot = 100,
                          conf.level = 0.95,
                          conf.type = "norm",
                          k = 2,
                          stat.title = NULL,
                          messages = TRUE,
                          ...) {
  # make sure both quoted and unquoted arguments are supported
  x <- rlang::ensym(x)
  y <- rlang::ensym(y)

  # creating a dataframe
  data %<>%
    dplyr::select(.data = ., {{ x }}, {{ y }}) %>%
    dplyr::mutate(.data = ., {{ x }} := droplevels(as.factor({{ x }}))) %>%
    tibble::as_tibble(x = .)

  # ---------------------------- between-subjects design --------------------

  # running bayesian analysis
  if (isFALSE(paired)) {
    # removing NAs
    data %<>% tidyr::drop_na(.)

    # sample size
    sample_size <- nrow(data)

    # Yuen's test for trimmed means
    stats_obj <-
      WRS2::yuen(
        formula = rlang::new_formula({{ y }}, {{ x }}),
        data = data,
        tr = tr
      )

    # tidying it up
    stats_df <-
      tibble::tribble(
        ~statistic, ~parameter, ~p.value,
        stats_obj$test, stats_obj$df, stats_obj$p.value
      )

    # computing effect size and its confidence interval
    effsize_obj <-
      WRS2::yuen.effect.ci(
        formula = rlang::new_formula({{ y }}, {{ x }}),
        data = data,
        tr = tr,
        nboot = nboot,
        alpha = 1 - conf.level
      )

    # effect size dataframe
    effsize_df <-
      tibble::tribble(
        ~estimate, ~conf.low, ~conf.high,
        effsize_obj$effsize[[1]], effsize_obj$CI[[1]], effsize_obj$CI[[2]]
      )

    # subtitle parameters
    k.parameter <- k
    n.text <- quote(italic("n")["obs"])
  }

  # ---------------------------- within-subjects design -------------------

  if (isTRUE(paired)) {
    # converting to long format and then getting it back in wide so that the
    # rowid variable can be used as the block variable
    data %<>% df_cleanup_paired(data = ., x = {{ x }}, y = {{ y }})

    # sample size
    sample_size <- length(unique(data$rowid))

    # getting dataframe of results from the custom function
    stats_df <-
      yuend_ci(
        data = data,
        x = {{ x }},
        y = {{ y }},
        tr = tr,
        nboot = nboot,
        conf.level = conf.level,
        conf.type = conf.type
      )

    # effect sizes are already in there
    effsize_df <- stats_df

    # subtitle parameters
    k.parameter <- 0L
    n.text <- quote(italic("n")["pairs"])
  }

  # message about effect size measure
  if (isTRUE(messages)) effsize_ci_message(nboot, conf.level)

  # preparing subtitle
  subtitle <-
    expr_template(
      no.parameters = 1L,
      stats.df = stats_df,
      effsize.df = effsize_df,
      stat.title = stat.title,
      statistic.text = quote(italic("t")),
      effsize.text = quote(widehat(italic(xi))),
      n = sample_size,
      n.text = n.text,
      conf.level = conf.level,
      k = k,
      k.parameter = k.parameter
    )

  # return the subtitle
  return(subtitle)
}

#' @title Making expression containing Bayesian *t*-test results
#' @name expr_t_bayes
#' @author \href{https://github.com/IndrajeetPatil}{Indrajeet Patil}
#'
#' @references For more details, see-
#' \url{https://indrajeetpatil.github.io/statsExpressions/articles/stats_details.html}
#'
#' @inheritParams expr_t_parametric
#' @inheritParams expr_anova_parametric
#' @inheritParams bf_ttest
#'
#' @examples
#' \donttest{
#' # for reproducibility
#' set.seed(123)
#'
#' # ------------- between-subjects design --------------------------
#'
#' statsExpressions::expr_t_bayes(
#'   data = mtcars,
#'   x = am,
#'   y = wt,
#'   paired = FALSE
#' )
#'
#' # ------------- within-subjects design -----------------------------
#'
#' statsExpressions::expr_t_bayes(
#'   data = dplyr::filter(
#'     statsExpressions::intent_morality,
#'     condition %in% c("accidental", "attempted"),
#'     harm == "Poisoning"
#'   ),
#'   x = condition,
#'   y = rating,
#'   paired = TRUE
#' )
#' }
#' @export

# function body
expr_t_bayes <- function(data,
                         x,
                         y,
                         bf.prior = 0.707,
                         paired = FALSE,
                         k = 2,
                         ...) {
  # make sure both quoted and unquoted arguments are supported
  x <- rlang::ensym(x)
  y <- rlang::ensym(y)

  # creating a dataframe
  subtitle <-
    dplyr::select(.data = data, {{ x }}, {{ y }}) %>%
    dplyr::mutate(.data = ., {{ x }} := droplevels(as.factor({{ x }}))) %>%
    tibble::as_tibble(.) %>% # preparing the subtitle
    bf_ttest(
      data = data,
      x = {{ x }},
      y = {{ y }},
      paired = paired,
      bf.prior = bf.prior,
      caption = NULL,
      output = "h1",
      k = k
    )

  # return the message
  return(subtitle)
}

# aliases -----------------------------------------------------------------

#' @rdname expr_t_nonparametric
#' @aliases expr_t_nonparametric
#' @export

expr_mann_nonparametric <- expr_t_nonparametric
