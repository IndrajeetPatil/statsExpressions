#' @title Making expression with frequentist random-effects meta-analysis
#'   results
#' @description This analysis is carried out using the `metafor` package. For
#'   more, see `?metafor::rma`.
#' @name expr_meta_parametric
#'
#' @param data A dataframe. It **must** contain columns named `estimate` (effect
#'   sizes or outcomes)  and `std.error` (corresponding standard errors). These
#'   two columns will be used for `yi`  and `sei` arguments in `metafor::rma`
#'   (for parametric analysis) or `metaplus::metaplus` (for robust analysis).
#' @param output  Character describing the desired output. If `"subtitle"`, a
#'   formatted subtitle with summary effect and statistical details will be
#'   returned, and if `"caption"`, expression containing details from model
#'   summary will be returned.
#' @param caption Text to display as caption. This argument is relevant only
#'   when `output = "caption"`.
#' @inheritParams tidyBF::bf_meta
#' @inheritParams expr_anova_parametric
#' @inheritDotParams metafor::rma -yi -sei -tau2 -level
#'
#' @importFrom metafor rma
#' @importFrom dplyr rename_all recode mutate
#' @importFrom tidyBF meta_data_check
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
#' # making subtitle
#' expr_meta_parametric(
#'   data = df,
#'   k = 3
#' )
#'
#' # making caption
#' expr_meta_parametric(
#'   data = df,
#'   output = "caption"
#' )
#' }
#' @export

# function body
expr_meta_parametric <- function(data,
                                 k = 2L,
                                 conf.level = 0.95,
                                 caption = NULL,
                                 output = "expression",
                                 ...) {
  # check the data contains needed column
  tidyBF::meta_data_check(data)

  #----------------------- meta-analysis ------------------------------------

  # object from meta-analysis
  meta_res <-
    metafor::rma(
      yi = estimate,
      sei = std.error,
      data = data,
      level = conf.level * 100,
      ...
    )

  #----------------------- tidy output and subtitle ---------------------------

  # create a dataframe with coefficients
  stats_df <-
    broomExtra::tidy_parameters(meta_res) %>%
    dplyr::mutate(.data = ., term = "Overall")

  # preparing the subtitle
  subtitle <-
    expr_template(
      stats.df = stats_df,
      statistic.text = quote(italic("z")),
      effsize.text = quote(widehat(beta)["summary"]^"meta"),
      n = nrow(data),
      n.text = quote(italic("n")["effects"]),
      no.parameters = 0L,
      conf.level = conf.level,
      k = k
    )

  #----------------------- model summary ------------------------------------

  # model summary
  df_glance <- broomExtra::glance_performance(meta_res)

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

  #---------------------------- output ---------------------------------------

  # what needs to be returned?
  return(switch(
    EXPR = output,
    "dataframe" = stats_df,
    "subtitle" = subtitle,
    "caption" = caption,
    subtitle
  ))
}


#' @title Making expression with frequentist random-effects robust meta-analysis
#'   results
#' @description This analysis is carried out using the `metaplus` package. For
#'   more, see `?metaplus::metaplus`.
#' @name expr_meta_robust
#'
#' @inheritParams expr_meta_parametric
#' @inheritParams metaplus::metaplus
#' @inheritDotParams metaplus::metaplus -yi -sei
#'
#' @importFrom metaplus metaplus
#' @importFrom dplyr filter inner_join
#'
#' @examples
#' \donttest{
#' # setup
#' set.seed(123)
#' library(metaplus)
#'
#' # renaming to what `statsExpressions` expects
#' df <- dplyr::rename(mag, estimate = yi, std.error = sei)
#'
#' # creating expression (changing few defaults)
#' expr_meta_robust(
#'   data = df,
#'   random = "normal"
#' )
#' }
#' @export

# function body
expr_meta_robust <- function(data,
                             random = "mixture",
                             k = 2L,
                             output = "expression",
                             ...) {
  # check the data contains needed column
  tidyBF::meta_data_check(data)

  #----------------------- meta-analysis ------------------------------------

  # object from meta-analysis
  meta_res <-
    metaplus::metaplus(
      yi = estimate,
      sei = std.error,
      data = data,
      random = random,
      ...
    )

  #----------------------- tidy output and subtitle ---------------------------

  # create a dataframe with coefficients
  stats_df <- dplyr::filter(.data = broomExtra::tidy_parameters(meta_res), term == "Overall")

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

  # return the output
  switch(output,
    "expression" = subtitle,
    "dataframe" = stats_df
  )
}

#' @name expr_meta_bayes
#' @title Making expression containing Bayesian random-effects meta-analysis.
#'
#' @inheritParams tidyBF::bf_meta
#'
#' @examples
#' \donttest{
#' # setup
#' set.seed(123)
#' library(metaplus)
#'
#' # renaming to what `statsExpressions` expects
#' df <- dplyr::rename(mag, estimate = yi, std.error = sei)
#'
#' # making subtitle
#' expr_meta_bayes(
#'   data = df,
#'   k = 3,
#'   # additional arguments given to `metaBMA`
#'   iter = 5000,
#'   summarize = "integrate",
#'   control = list(adapt_delta = 0.99, max_treedepth = 15)
#' )
#' }
#' @export

# function body
expr_meta_bayes <- function(data,
                            d = prior("norm", c(mean = 0, sd = 0.3)),
                            tau = prior("invgamma", c(shape = 1, scale = 0.15)),
                            k = 2L,
                            output = "expression",
                            ...) {
  # check the data contains needed column
  tidyBF::meta_data_check(data)

  # bayes factor results
  stats_df <-
    tidyBF::bf_meta(
      data = data,
      d = d,
      tau = tau,
      k = k,
      output = output
    )

  if (output == "expression") subtitle <- stats_df$expr

  # return the output
  switch(output,
    "expression" = subtitle,
    "dataframe" = stats_df
  )
}
