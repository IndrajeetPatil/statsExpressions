#' @title Making expression for random-effects meta-analysis
#' @name expr_meta_random
#'
#' @param data A dataframe. It **must** contain columns named `estimate` (effect
#'   sizes or outcomes)  and `std.error` (corresponding standard errors). These
#'   two columns will be used for `yi`  and `sei` arguments in `metafor::rma`
#'   (for parametric analysis) or `metaplus::metaplus` (for robust analysis).
#' @param caption Text to display as caption. This argument is relevant only
#'   when `output = "caption"`.
#' @inheritParams expr_t_onesample
#' @inheritParams tidyBF::bf_meta
#' @inheritParams metaplus::metaplus
#' @inheritParams expr_anova_parametric
#' @param ... Additional arguments passed to the respective meta-analysis
#'   function.
#'
#' @details This analysis is carried out using-
#' \itemize{
#'   \item parametric: `metafor::rma`
#'   \item  robust: `metaplus::metaplus`
#'   \item  Bayesian: `metaBMA::meta_random`
#' }
#'
#' @importFrom metafor rma
#' @importFrom metaplus metaplus
#' @importFrom dplyr rename_all recode mutate
#' @importFrom tidyBF bf_meta_random meta_data_check
#' @importFrom broomExtra tidy_parameters glance_performance
#'
#' @examples
#' \donttest{
#' # setup
#' set.seed(123)
#' library(statsExpressions)
#' library(metaplus)
#'
#' # renaming to what `statsExpressions` expects
#' df <- dplyr::rename(mag, estimate = yi, std.error = sei)
#'
#' # ----------------------- parametric ---------------------------------------
#'
#' # making subtitle
#' expr_meta_random(
#'   data = df,
#'   k = 3
#' )
#'
#' # making caption
#' expr_meta_random(
#'   data = df,
#'   output = "caption"
#' )
#'
#' # ----------------------- random -----------------------------------------
#'
#' # creating expression
#' expr_meta_random(
#'   data = df,
#'   type = "random",
#'   random = "normal",
#'   output = "dataframe"
#' )
#'
#' # ----------------------- Bayes Factor -----------------------------------
#'
#' # making subtitle
#' expr_meta_random(
#'   data = df,
#'   type = "bayes",
#'   k = 3,
#'   # additional arguments given to `metaBMA`
#'   iter = 5000,
#'   summarize = "integrate",
#'   control = list(adapt_delta = 0.99, max_treedepth = 15)
#' )
#' }
#' @export

# function body
expr_meta_random <- function(data,
                             type = "parametric",
                             d = prior("norm", c(mean = 0, sd = 0.3)),
                             tau = prior("invgamma", c(shape = 1, scale = 0.15)),
                             random = "mixture",
                             k = 2L,
                             conf.level = 0.95,
                             caption = NULL,
                             output = "expression",
                             ...) {
  # check the data contains needed column
  tidyBF::meta_data_check(data)
  stats.type <- stats_type_switch(type)

  #----------------------- parametric ------------------------------------

  if (stats.type == "parametric") {
    # object from meta-analysis
    mod <-
      metafor::rma(
        yi = estimate,
        sei = std.error,
        data = data,
        level = conf.level * 100,
        ...
      )

    # model summary
    df_glance <- broomExtra::glance_performance(mod)

    # preparing the subtitle
    caption <-
      substitute(
        atop(displaystyle(top.text),
          expr = paste(
            "Heterogeneity: ",
            italic("Q"),
            "(",
            df,
            ") = ",
            Q,
            ", ",
            italic("p"),
            " = ",
            pvalue,
            ", ",
            tau["REML"]^2,
            " = ",
            tau2,
            ", ",
            "I"^2,
            " = ",
            I2
          )
        ),
        env = list(
          top.text = caption,
          Q = specify_decimal_p(x = df_glance$cochran.qe, k = 0L),
          df = specify_decimal_p(x = df_glance$df.residual, k = 0L),
          pvalue = specify_decimal_p(x = df_glance$p.value.cochran.qe, k = k, p.value = TRUE),
          tau2 = specify_decimal_p(x = df_glance$tau.squared, k = k),
          I2 = paste(specify_decimal_p(x = df_glance$i.squared, k = 2L), "%", sep = "")
        )
      )
  }

  #----------------------- robust ------------------------------------

  if (stats.type == "robust") {
    # object from meta-analysis
    mod <-
      metaplus::metaplus(
        yi = estimate,
        sei = std.error,
        data = data,
        random = random,
        ...
      )

    caption <- NULL
  }

  # clean up
  if (stats.type %in% c("parametric", "robust")) {
    # create a dataframe with coefficients
    stats_df <- dplyr::filter(
      broomExtra::tidy_parameters(mod),
      term %in% c("Overall", "overall")
    )

    # preparing the subtitle
    subtitle <-
      expr_template(
        stats.df = stats_df,
        statistic.text = quote(italic("z")),
        effsize.text = quote(widehat(beta)["summary"]^"meta"),
        n = nrow(data),
        n.text = quote(italic("n")["effects"]),
        no.parameters = 0L,
        conf.level = 0.95,
        k = k
      )
  }

  #---------------------------- Bayes Factor ---------------------------------

  if (stats.type == "bayes") {
    # bayes factor results
    stats_df <-
      tidyBF::bf_meta(
        data = data,
        d = d,
        tau = tau,
        k = k,
        output = output,
        ...
      )

    caption <- NULL
    subtitle <- stats_df
  }

  #---------------------------- return ---------------------------------

  # what needs to be returned?
  return(switch(
    EXPR = output,
    "dataframe" = stats_df,
    "caption" = caption,
    subtitle
  ))
}
