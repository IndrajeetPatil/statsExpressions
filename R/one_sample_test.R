#' @title One-sample tests
#' @name one_sample_test
#'
#' @param x A numeric variable from the dataframe `data`.
#' @param test.value A number indicating the true value of the mean (Default:
#'   `0`).
#' @inheritParams ipmisc::long_to_wide_converter
#' @inheritParams expr_template
#' @inheritParams bf_extractor
#' @inheritParams two_sample_test
#' @inheritParams oneway_anova
#'
#' @description
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("stable")}
#'
#' A dataframe containing results from a one-sample test. The exact test and the
#' effect size details contained will depend on the `type` argument.
#'
#' For more details, see-
#' \url{https://indrajeetpatil.github.io/statsExpressions/articles/stats_details.html}
#'
#' @importFrom dplyr select mutate pull rename_all recode
#' @importFrom ipmisc stats_type_switch
#' @importFrom effectsize cohens_d hedges_g rank_biserial
#' @importFrom stats t.test wilcox.test na.omit
#' @importFrom rlang !!! exec
#' @importFrom BayesFactor ttestBF
#'
#' @examples
#' \donttest{
#' # for reproducibility
#' set.seed(123)
#' library(statsExpressions)
#' options(tibble.width = Inf, pillar.bold = TRUE, pillar.neg = TRUE)
#'
#' # ----------------------- parametric ---------------------------------------
#'
#' one_sample_test(
#'   data = ggplot2::msleep,
#'   x = brainwt,
#'   test.value = 0.275,
#'   type = "parametric"
#' )
#'
#' # ----------------------- non-parametric -----------------------------------
#'
#' one_sample_test(
#'   data = ggplot2::msleep,
#'   x = brainwt,
#'   test.value = 0.275,
#'   type = "nonparametric"
#' )
#'
#' # ----------------------- robust --------------------------------------------
#'
#' one_sample_test(
#'   data = ggplot2::msleep,
#'   x = brainwt,
#'   test.value = 0.275,
#'   type = "robust"
#' )
#'
#' # ---------------------------- Bayesian -----------------------------------
#'
#' one_sample_test(
#'   data = ggplot2::msleep,
#'   x = brainwt,
#'   test.value = 0.275,
#'   type = "bayes",
#'   bf.prior = 0.8
#' )
#' }
#' @export

one_sample_test <- function(data,
                            x,
                            type = "parametric",
                            test.value = 0,
                            k = 2L,
                            conf.level = 0.95,
                            tr = 0.2,
                            bf.prior = 0.707,
                            effsize.type = "g",
                            nboot = 100L,
                            top.text = NULL,
                            ...) {
  # standardize the type of statistics
  type <- ipmisc::stats_type_switch(type)

  # preparing the vector
  x_vec <- stats::na.omit(data %>% dplyr::pull({{ x }}))

  # ----------------------- parametric ---------------------------------------

  if (type == "parametric") {
    # preparing expression parameters
    no.parameters <- 1L
    .f <- stats::t.test
    if (effsize.type %in% c("unbiased", "g")) .f.es <- effectsize::hedges_g
    if (effsize.type %in% c("biased", "d")) .f.es <- effectsize::cohens_d
  }

  # ----------------------- non-parametric ---------------------------------------

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
        x = x_vec,
        mu = test.value,
        exact = FALSE
      ) %>%
      tidy_model_parameters(.) %>%
      dplyr::select(-dplyr::matches("^est|^conf|^diff|^term|^ci"))

    # extracting effect size details
    effsize_df <-
      rlang::exec(
        .fn = .f.es,
        x = x_vec - test.value,
        ci = conf.level,
        verbose = FALSE,
        iterations = nboot
      ) %>%
      tidy_model_effectsize(.)

    # these can be really big values
    if (type == "nonparametric") stats_df %<>% dplyr::mutate(statistic = log(statistic))

    # dataframe
    stats_df <- dplyr::bind_cols(stats_df, effsize_df)
  }

  # ----------------------- robust ---------------------------------------

  if (type == "robust") {
    # bootstrap-t method for one-sample test
    no.parameters <- 0L
    stats_df <-
      trimcibt(
        x = x_vec,
        tr = tr,
        nboot = nboot,
        nv = test.value,
        alpha = 1 - conf.level
      )
  }

  # expression
  if (type != "bayes") {
    stats_df %<>%
      dplyr::mutate(expression = list(expr_template(
        no.parameters = no.parameters,
        data = .,
        n = length(x_vec),
        k = k
      )))
  }

  # ----------------------- Bayesian ---------------------------------------

  # running Bayesian one-sample t-test
  if (type == "bayes") {
    bf_object <- BayesFactor::ttestBF(x_vec, rscale = bf.prior, mu = test.value)

    # final return
    stats_df <- bf_extractor(bf_object, conf.level, k = k, top.text = top.text)
  }

  # return the output
  as_tibble(stats_df)
}

#' @rdname one_sample_test
#' @aliases one_sample_test
#' @export

expr_t_onesample <- one_sample_test

#' bootstrap-t method for one-sample test
#' @importFrom WRS2 trimse
#' @noRd

trimcibt <- function(x, tr = 0.2, nboot = 100, nv = 0, alpha = 0.05, ...) {
  test <- (mean(x, tr) - nv) / WRS2::trimse(x, tr)
  data <- matrix(sample(x, size = length(x) * nboot, replace = TRUE), nrow = nboot) - mean(x, tr)
  tval <- sort(abs(apply(data, 1, mean, tr) / apply(data, 1, WRS2::trimse, tr)))
  icrit <- round((1 - alpha) * nboot)

  tibble(
    statistic = test,
    p.value = (sum(abs(test) <= abs(tval))) / nboot,
    method = "Bootstrap-t method for one-sample test",
    estimate = mean(x, tr),
    conf.low = mean(x, tr) - tval[icrit] * WRS2::trimse(x, tr),
    conf.high = mean(x, tr) + tval[icrit] * WRS2::trimse(x, tr),
    conf.level = 1 - alpha,
    effectsize = "Trimmed mean"
  )
}
