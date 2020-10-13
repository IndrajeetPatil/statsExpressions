#' @title Making expression containing *t*-test results
#' @name expr_t_parametric
#'
#' @param effsize.type Type of effect size needed for *parametric* tests. The
#'   argument can be `"d"` (for Cohen's *d*) or `"g"` (for Hedge's *g*).
#' @inheritParams expr_anova_parametric
#' @inheritParams stats::t.test
#' @inheritParams expr_template
#'
#' @importFrom dplyr select rename_all recode mutate
#' @importFrom rlang !! ensym new_formula exec
#' @importFrom tidyr drop_na
#' @importFrom stats t.test
#' @importFrom effectsize cohens_d hedges_g
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
#'   non-central Student-*t* distributions.
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
#' library(statsExpressions)
#'
#' # creating a smaller dataset
#' msleep_short <- dplyr::filter(ggplot2::msleep, vore %in% c("carni", "herbi"))
#'
#' # with defaults
#' statsExpressions::expr_t_parametric(
#'   data = msleep_short,
#'   x = vore,
#'   y = sleep_rem
#' )
#'
#' # changing defaults (getting expression as output)
#' statsExpressions::expr_t_parametric(
#'   data = msleep_short,
#'   x = vore,
#'   y = sleep_rem,
#'   var.equal = TRUE,
#'   effsize.type = "d"
#' )
#' @export

# function body
expr_t_parametric <- function(data,
                              x,
                              y,
                              paired = FALSE,
                              k = 2L,
                              conf.level = 0.95,
                              effsize.type = "g",
                              var.equal = FALSE,
                              ...) {

  # make sure both quoted and unquoted arguments are supported
  c(x, y) %<-% c(rlang::ensym(x), rlang::ensym(y))

  # ============================ data preparation ==========================

  # have a proper cleanup with NA removal
  data %<>%
    long_to_wide_converter(
      data = .,
      x = {{ x }},
      y = {{ y }},
      paired = paired,
      spread = FALSE
    )

  # properly removing NAs if it's a paired design
  if (isTRUE(paired)) {
    # sample size
    sample_size <- length(unique(data$rowid))
    n.text <- quote(italic("n")["pairs"])
  }

  # remove NAs listwise for between-subjects design
  if (isFALSE(paired)) {
    # sample size
    sample_size <- nrow(data)
    n.text <- quote(italic("n")["obs"])
  }

  # deciding which effect size to use (Hedge's g or Cohen's d)
  if (effsize.type %in% c("unbiased", "g")) {
    effsize.text <- quote(widehat(italic("g"))["Hedge"])
    .f <- effectsize::hedges_g
  } else {
    effsize.text <- quote(widehat(italic("d"))["Cohen"])
    .f <- effectsize::cohens_d
  }

  # setting up the t-test model and getting its summary
  stats_df <-
    stats::t.test(
      formula = rlang::new_formula({{ y }}, {{ x }}),
      data = data,
      paired = paired,
      alternative = "two.sided",
      var.equal = var.equal,
      na.action = na.omit
    ) %>%
    broomExtra::tidy(.)

  # effect size object
  effsize_df <-
    rlang::exec(
      .fn = .f,
      x = rlang::new_formula({{ y }}, {{ x }}),
      data = data,
      correction = FALSE,
      paired = paired,
      pooled_sd = var.equal,
      ci = conf.level
    ) %>%
    insight::standardize_names(data = ., style = "broom")

  # when paired samples t-test is run df is going to be integer
  # ditto for when variance is assumed to be equal
  k.df <- ifelse(isTRUE(paired) || isTRUE(var.equal), 0L, k)
  statistic.text <-
    if (isTRUE(paired) || isTRUE(var.equal)) {
      quote(italic("t")["Student"])
    } else {
      quote(italic("t")["Welch"])
    }

  # preparing subtitle
  expr_template(
    no.parameters = 1L,
    stats.df = stats_df,
    effsize.df = effsize_df,
    statistic.text = statistic.text,
    effsize.text = effsize.text,
    n = sample_size,
    conf.level = conf.level,
    k = k,
    k.parameter = k.df,
    n.text = n.text
  )
}


#' @title Making expression for Mann-Whitney *U*-test/Wilcoxon test results
#' @name expr_t_nonparametric
#'
#' @inheritParams expr_anova_parametric
#' @inheritParams expr_t_parametric
#' @inheritParams expr_anova_nonparametric
#'
#' @importFrom dplyr select
#' @importFrom rlang !! enquo exec new_formula
#' @importFrom stats wilcox.test
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
#'   Therefore, *V* will vary in magnitude but not significance based solely
#'   on the order of the grouping variable. Consider manually
#'   reordering your factor levels if appropriate as shown in the second example
#'   below.
#'
#' @references For more details, see-
#' \url{https://indrajeetpatil.github.io/statsExpressions/articles/stats_details.html}
#'
#' @examples
#' # for reproducibility
#' set.seed(123)
#' library(statsExpressions)
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
#' msleep_short$vore <- factor(msleep_short$vore, levels = c("herbi", "carni"))
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
#'
#' # using dataset included in the package
#' statsExpressions::expr_t_nonparametric(
#'   data = VR_dilemma,
#'   x = modality,
#'   y = score,
#'   paired = TRUE,
#'   conf.level = 0.90
#' )
#' @export

# function body
expr_t_nonparametric <- function(data,
                                 x,
                                 y,
                                 paired = FALSE,
                                 k = 2L,
                                 conf.level = 0.95,
                                 conf.type = "norm",
                                 nboot = 100,
                                 ...) {

  # make sure both quoted and unquoted arguments are supported
  c(x, y) %<-% c(rlang::ensym(x), rlang::ensym(y))

  # ============================ data preparation ==========================

  # have a proper cleanup with NA removal
  data %<>%
    long_to_wide_converter(
      data = .,
      x = {{ x }},
      y = {{ y }},
      paired = paired,
      spread = FALSE
    )

  # properly removing NAs if it's a paired design
  if (isTRUE(paired)) {
    # subtitle details
    sample_size <- length(unique(data$rowid))
    n.text <- quote(italic("n")["pairs"])
    .f <- rcompanion::wilcoxonPairedR
    statistic.text <- quote("log"["e"](italic("V")["Wilcoxon"]))
  }

  # remove NAs listwise for between-subjects design
  if (isFALSE(paired)) {
    # subtitle details
    sample_size <- nrow(data)
    n.text <- quote(italic("n")["obs"])
    .f <- rcompanion::wilcoxonR
    statistic.text <- quote("log"["e"](italic("W")["Mann-Whitney"]))
  }

  # setting up the test and getting its summary
  stats_df <-
    stats::wilcox.test(
      formula = rlang::new_formula({{ y }}, {{ x }}),
      data = data,
      paired = paired,
      alternative = "two.sided",
      na.action = na.omit,
      exact = FALSE
    ) %>%
    broomExtra::tidy(.) %>%
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
      digits = k,
      reportIncomplete = TRUE
    ) %>%
    rcompanion_cleaner(.)

  # preparing subtitle
  expr_template(
    no.parameters = 0L,
    stats.df = stats_df,
    effsize.df = effsize_df,
    statistic.text = statistic.text,
    effsize.text = quote(widehat(italic("r"))),
    n = sample_size,
    n.text = n.text,
    conf.level = conf.level,
    k = k
  )
}

#' @title Expression containing results from a robust *t*-test
#' @name expr_t_robust
#'
#' @references For more details, see-
#' \url{https://indrajeetpatil.github.io/statsExpressions/articles/stats_details.html}
#'
#' @inheritParams expr_t_parametric
#' @inheritParams expr_anova_parametric
#' @inheritParams expr_anova_robust
#'
#' @importFrom dplyr select
#' @importFrom rlang !! enquo
#' @importFrom WRS2 yuen yuen.effect.ci yuend dep.effect
#'
#' @examples
#' # for reproducibility
#' set.seed(123)
#' library(statsExpressions)
#'
#' # between-subjects design -----------------------------------------------
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
#'   k = 3,
#'   tr = 0.2
#' )
#'
#' # within-subjects design -----------------------------------------------
#' statsExpressions::expr_t_robust(
#'   data = dplyr::filter(bugs_long, condition %in% c("LDLF", "LDHF")),
#'   x = condition,
#'   y = desire,
#'   paired = TRUE
#' )
#' @export

# function body
expr_t_robust <- function(data,
                          x,
                          y,
                          paired = FALSE,
                          k = 2L,
                          conf.level = 0.95,
                          tr = 0.1,
                          nboot = 100,
                          ...) {
  # make sure both quoted and unquoted arguments are supported
  c(x, y) %<-% c(rlang::ensym(x), rlang::ensym(y))

  # ============================ data preparation ==========================

  # have a proper cleanup with NA removal
  data %<>%
    long_to_wide_converter(
      data = .,
      x = {{ x }},
      y = {{ y }},
      paired = paired,
      spread = paired
    )

  # ---------------------------- between-subjects design --------------------

  # running Bayesian analysis
  if (isFALSE(paired)) {
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
      tibble(
        estimate = effsize_obj$effsize[[1]],
        conf.low = effsize_obj$CI[[1]],
        conf.high = effsize_obj$CI[[2]]
      )

    # Yuen's test for trimmed means
    stats_obj <-
      WRS2::yuen(
        formula = rlang::new_formula({{ y }}, {{ x }}),
        data = data,
        tr = tr
      )

    # tidying it up
    stats_df <-
      tibble(
        statistic = stats_obj$test[[1]],
        parameter = stats_obj$df[[1]],
        p.value = stats_obj$p.value[[1]]
      )

    # subtitle parameters
    k.parameter <- k
    n.text <- quote(italic("n")["obs"])
    effsize.text <- quote(widehat(italic(xi)))
  }

  # ---------------------------- within-subjects design -------------------

  if (isTRUE(paired)) {
    # running robust paired t-test
    fit <- WRS2::yuend(x = data[2], y = data[3], tr = tr)

    # create a dataframe
    stats_df <-
      tibble(
        statistic = fit$test[[1]],
        parameter = fit$df[[1]],
        p.value = fit$p.value[[1]]
      )

    # computing effect size
    fit2 <-
      WRS2::dep.effect(
        x = data[2],
        y = data[3],
        tr = tr,
        nboot = nboot
      )

    # create a dataframe
    effsize_df <-
      as_tibble(as.data.frame(fit2), rownames = "effect_size") %>%
      dplyr::filter(effect_size == "AKP") %>%
      dplyr::rename(estimate = Est, conf.low = ci.low, conf.high = ci.up)

    # subtitle parameters
    k.parameter <- 0L
    n.text <- quote(italic("n")["pairs"])
    conf.level <- 0.95
    effsize.text <- quote(widehat(italic(delta))["R"])
  }

  # preparing subtitle
  expr_template(
    no.parameters = 1L,
    stats.df = stats_df,
    effsize.df = effsize_df,
    statistic.text = quote(italic("t")["Yuen"]),
    effsize.text = effsize.text,
    n = nrow(data),
    n.text = n.text,
    conf.level = conf.level,
    k = k,
    k.parameter = k.parameter
  )
}

#' @title Making expression containing Bayesian *t*-test results
#' @name expr_t_bayes
#'
#' @references For more details, see-
#' \url{https://indrajeetpatil.github.io/statsExpressions/articles/stats_details.html}
#'
#' @importFrom tidyBF bf_ttest
#'
#' @inheritParams expr_t_parametric
#' @inheritParams expr_anova_parametric
#' @inheritParams tidyBF::bf_ttest
#'
#' @examples
#' # for reproducibility
#' set.seed(123)
#' library(statsExpressions)
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
#'   data = dplyr::filter(bugs_long, condition %in% c("LDLF", "LDHF")),
#'   x = condition,
#'   y = desire,
#'   paired = TRUE
#' )
#' @export

# function body
expr_t_bayes <- function(data,
                         x,
                         y,
                         paired = FALSE,
                         k = 2L,
                         bf.prior = 0.707,
                         ...) {
  # prepare subtitle
  tidyBF::bf_ttest(
    data = data,
    x = {{ x }},
    y = {{ y }},
    paired = paired,
    bf.prior = bf.prior,
    output = "h1",
    k = k
  )$expr
}
