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
#'   summary will be returned. The other option is to return `"tidy"` data frame
#'   with coefficients or `"glance"` dataframe with model summaries.
#' @param caption Text to display as caption. This argument is relevant only
#'   when `output = "caption"`.
#' @inheritParams expr_anova_parametric
#' @inheritDotParams metafor::rma -yi -sei -tau2 -level
#'
#' @importFrom metafor rma
#' @importFrom dplyr rename_all recode mutate tibble
#'
#' @examples
#' \donttest{
#' # setup
#' set.seed(123)
#' library(statsExpressions)
#'
#' # let's create a dataframe
#' df_results <-
#'   structure(
#'     .Data = list(estimate = c(
#'       0.382047603321706, 0.780783111514665,
#'       0.425607573765058, 0.558365541235078, 0.956473848429961
#'     ), std.error = c(
#'       0.0465576338644502,
#'       0.0330218199731529, 0.0362834986178494, 0.0480571500648261, 0.062215818388157
#'     ), t.value = c(
#'       8.20590677855356, 23.6444603038067, 11.7300588415607,
#'       11.6187818146078, 15.3734833553524
#'     ), conf.low = c(
#'       0.290515146096969,
#'       0.715841986960399, 0.354354575031406, 0.46379116008131, 0.827446138277154
#'     ), conf.high = c(
#'       0.473580060546444, 0.845724236068931, 0.496860572498711,
#'       0.652939922388847, 1.08550155858277
#'     ), p.value = c(
#'       3.28679518728519e-15,
#'       4.04778497135963e-75, 7.59757330804449e-29, 5.45155840151592e-26,
#'       2.99171217913312e-13
#'     ), df.residual = c(
#'       394L, 358L, 622L, 298L,
#'       22L
#'     )),
#'     row.names = c(NA, -5L),
#'     class = c("tbl_df", "tbl", "data.frame")
#'   )
#'
#' # making subtitle
#' expr_meta_parametric(
#'   data = df_results,
#'   k = 3,
#'   messages = FALSE
#' )
#'
#' # getting tidy data frame with coefficients
#' expr_meta_parametric(
#'   data = df_results,
#'   messages = FALSE,
#'   output = "tidy"
#' )
#'
#' # making caption
#' expr_meta_parametric(
#'   data = df_results,
#'   k = 2,
#'   messages = FALSE,
#'   output = "caption"
#' )
#'
#' # getting dataframe with model summary
#' expr_meta_parametric(
#'   data = df_results,
#'   messages = FALSE,
#'   output = "glance"
#' )
#' }
#' @export

# function body
expr_meta_parametric <- function(data,
                                 conf.level = 0.95,
                                 k = 2,
                                 messages = FALSE,
                                 output = "subtitle",
                                 caption = NULL,
                                 ...) {
  # check the data contains needed column
  meta_data_check(data)

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

  # print the results
  if (isTRUE(messages)) print(summary(meta_res))

  #----------------------- tidy output and subtitle ---------------------------

  # create a dataframe with coefficients
  df_tidy <-
    coef(summary(meta_res)) %>%
    tibble::as_tibble(.) %>%
    dplyr::rename_all(
      .tbl = .,
      .funs = dplyr::recode,
      se = "std.error",
      zval = "statistic",
      pval = "p.value",
      ci.lb = "conf.low",
      ci.ub = "conf.high"
    ) %>%
    dplyr::mutate(.data = ., term = "summary effect")

  # preparing the subtitle
  subtitle <-
    expr_template(
      stat.title = "Summary effect: ",
      stats.df = df_tidy,
      effsize.df = df_tidy,
      statistic.text = quote(italic("z")),
      effsize.text = quote(widehat(beta)),
      n = nrow(data),
      n.text = quote(italic("n")["effects"]),
      no.parameters = 0L,
      conf.level = conf.level,
      k = k
    )

  #----------------------- model summary ------------------------------------

  df_glance <-
    with(
      data = meta_res,
      expr = dplyr::tibble(
        tau2 = tau2,
        se.tau2 = se.tau2,
        k = k,
        p = p,
        m = m,
        QE = QE,
        QEp = QEp,
        QM = QM,
        QMp = QMp,
        I2 = I2,
        H2 = H2,
        int.only = int.only
      )
    )

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
        Q = specify_decimal_p(x = df_glance$QE, k = 0L),
        df = specify_decimal_p(x = (df_glance$k - 1), k = 0L),
        pvalue = specify_decimal_p(x = df_glance$QEp, k = k, p.value = TRUE),
        tau2 = specify_decimal_p(x = df_glance$tau2, k = k),
        I2 = paste(specify_decimal_p(x = df_glance$I2, k = 2L), "%", sep = "")
      )
    )

  #---------------------------- output ---------------------------------------

  # what needs to be returned?
  return(switch(
    EXPR = output,
    "subtitle" = subtitle,
    "tidy" = df_tidy,
    "caption" = caption,
    "glance" = df_glance,
    "subtitle"
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
#'   random = "normal",
#'   k = 4,
#'   messages = TRUE,
#'   plotci = TRUE
#' )
#' }
#' @export

# function body
expr_meta_robust <- function(data,
                             random = "mixture",
                             k = 2,
                             messages = FALSE,
                             ...) {
  # check the data contains needed column
  meta_data_check(data)

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

  # print the results
  if (isTRUE(messages)) print(meta_res)

  #----------------------- tidy output and subtitle ---------------------------

  # create a dataframe with coefficients
  df_tidy <-
    meta_res %>% {
      dplyr::inner_join(
        x = tibble::as_tibble(as.data.frame(.$results), rownames = "term"),
        y = tibble::as_tibble(as.data.frame(.$profile@summary@coef), rownames = "term"),
        by = "term"
      ) %>%
        dplyr::rename_all(.tbl = ., .funs = tolower) %>%
        dplyr::select(
          .data = .,
          term,
          estimate,
          conf.low = `95% ci.lb`,
          conf.high = `95% ci.ub`,
          p.value = pvalue,
          statistic = `z value`
        ) %>%
        dplyr::filter(.data = ., term == "muhat")
    }

  # preparing the subtitle
  subtitle <-
    expr_template(
      stat.title = "Summary effect: ",
      stats.df = df_tidy,
      effsize.df = df_tidy,
      statistic.text = quote(italic("z")),
      effsize.text = quote(widehat(beta)),
      n = nrow(data),
      n.text = quote(italic("n")["effects"]),
      no.parameters = 0L,
      conf.level = 0.95,
      k = k
    )

  #---------------------------- output ---------------------------------------

  # what needs to be returned?
  return(subtitle)
}

#' @name expr_meta_bayes
#' @title Making expression containing Bayesian random-effects meta-analysis.
#'
#' @inheritParams bf_meta
#'
#' @examples
#' \donttest{
#' # setup
#' set.seed(123)
#' library(statsExpressions)
#'
#' # let's create a dataframe
#' df_results <-
#'   structure(
#'     .Data = list(estimate = c(
#'       0.382047603321706, 0.780783111514665,
#'       0.425607573765058, 0.558365541235078, 0.956473848429961
#'     ), std.error = c(
#'       0.0465576338644502,
#'       0.0330218199731529, 0.0362834986178494, 0.0480571500648261, 0.062215818388157
#'     ), t.value = c(
#'       8.20590677855356, 23.6444603038067, 11.7300588415607,
#'       11.6187818146078, 15.3734833553524
#'     ), conf.low = c(
#'       0.290515146096969,
#'       0.715841986960399, 0.354354575031406, 0.46379116008131, 0.827446138277154
#'     ), conf.high = c(
#'       0.473580060546444, 0.845724236068931, 0.496860572498711,
#'       0.652939922388847, 1.08550155858277
#'     ), p.value = c(
#'       3.28679518728519e-15,
#'       4.04778497135963e-75, 7.59757330804449e-29, 5.45155840151592e-26,
#'       2.99171217913312e-13
#'     ), df.residual = c(
#'       394L, 358L, 622L, 298L,
#'       22L
#'     )),
#'     row.names = c(NA, -5L),
#'     class = c("tbl_df", "tbl", "data.frame")
#'   )
#'
#' # making subtitle
#' expr_meta_bayes(
#'   data = df_results,
#'   k = 3,
#'   messages = FALSE,
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
                            k = 2,
                            messages = TRUE,
                            ...) {
  # check the data contains needed column
  meta_data_check(data)

  # bayes factor results
  subtitle <-
    bf_meta(
      data = data,
      d = d,
      tau = tau,
      k = k,
      caption = NULL,
      output = "h1",
      messages = messages
    )

  # return the subtitle
  return(subtitle)
}

#' @noRd

meta_data_check <- function(data) {
  # check if the two columns needed are present
  if (sum(c("estimate", "std.error") %in% names(data)) != 2) {
    # inform the user that skipping labels for the same reason
    stop(message(cat(
      crayon::red("Error"),
      crayon::blue(": The dataframe must contain the following two columns:\n"),
      crayon::blue("`estimate` and `std.error`."),
      sep = ""
    )),
    call. = FALSE
    )
  }
}
