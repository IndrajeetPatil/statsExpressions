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
#' @param metaBMA.args A list of additional arguments to be passed to
#'   `metaBMA::meta_random`.
#' @inheritParams metaplus::metaplus
#' @inheritParams expr_oneway_anova
#' @param ... Additional arguments passed to the respective meta-analysis
#'   function.
#'
#' @details This analysis is carried out using-
#' \itemize{
#'   \item parametric: `metafor::rma`
#'   \item robust: `metaplus::metaplus`
#'   \item Bayesian: `metaBMA::meta_random`
#' }
#'
#' @importFrom metafor rma
#' @importFrom metaplus metaplus
#' @importFrom metaBMA meta_random prior
#' @importFrom rlang exec !!!
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
#' # creating expression
#' expr_meta_random(data = df, k = 3)
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
#'   metaBMA.args = list(
#'     iter = 5000,
#'     summarize = "integrate",
#'     control = list(adapt_delta = 0.99, max_treedepth = 15)
#'   )
#' )
#' }
#' @export

# function body
expr_meta_random <- function(data,
                             type = "parametric",
                             metaBMA.args = list(),
                             random = "mixture",
                             k = 2L,
                             conf.level = 0.95,
                             caption = NULL,
                             output = "expression",
                             ...) {
  # check the type of test
  stats.type <- ipmisc::stats_type_switch(type)

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
    df_glance <- tidy_model_performance(mod)

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
          Q = format_num(df_glance$cochransq, k = 0L),
          df = format_num(df_glance$df.error, k = 0L),
          pvalue = format_num(df_glance$p.cochransq, k = k, p.value = TRUE),
          tau2 = format_num(df_glance$tau2, k = k),
          I2 = paste0(format_num(df_glance$i2 * 100, k = 2L), "%")
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
  }

  # clean up
  if (stats.type %in% c("parametric", "robust")) {
    # create a dataframe with coefficients
    stats_df <- tidy_model_parameters(mod, include_studies = FALSE)

    # informative column
    stats_df %<>% dplyr::mutate(effectsize = "meta-analytic summary estimate")

    # preparing the subtitle
    subtitle <-
      expr_template(
        stats.df = stats_df,
        n = nrow(data),
        n.text = quote(italic("n")["effects"]),
        no.parameters = 0L,
        conf.level = 0.95,
        k = k
      )
  }

  #---------------------------- Bayes Factor ---------------------------------

  if (stats.type == "bayes") {
    # extracting results from random-effects meta-analysis
    bf_object <-
      rlang::exec(
        .fn = metaBMA::meta_random,
        y = data$estimate,
        SE = data$std.error,
        !!!metaBMA.args
      )

    # final return
    subtitle <- stats_df <- bf_extractor(bf_object, conf.level, k = k, centrality = "mean", output = output)
  }

  # what needs to be returned?
  switch(output, "dataframe" = as_tibble(stats_df), "caption" = caption, subtitle)
}
