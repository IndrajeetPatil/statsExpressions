#' @title Random-effects meta-analyses
#' @name meta_analysis
#'
#' @param data A dataframe. It **must** contain columns named `estimate` (effect
#'   sizes or outcomes)  and `std.error` (corresponding standard errors). These
#'   two columns will be used:
#'   - as `yi`  and `sei` arguments in `metafor::rma` (for **parametric** test)
#'   or `metaplus::metaplus` (for **robust** test)
#'   - as `y` and `SE` arguments in `metaBMA::meta_random` (for **Bayesian**
#'   test).
#' @inheritParams one_sample_test
#' @inheritParams metaplus::metaplus
#' @inheritParams oneway_anova
#' @param ... Additional arguments passed to the respective meta-analysis
#'   function.
#'
#' @description
#'
#' A dataframe containing results from random-effects meta-analysis.
#'
#' To see details about functions which are internally used to carry out these
#' analyses, see the following vignette-
#' \url{https://indrajeetpatil.github.io/statsExpressions/articles/stats_details.html}
#'
#' @note **Important**: The function assumes that you have already downloaded
#'   the needed package (`metafor`, `metaplus`, or `metaBMA`) for meta-analysis.
#'   If they are not available, you will be asked to install them.
#'
#' @importFrom rlang exec !!! call2
#'
#' @examples
#' \donttest{
#' # run examples only if the needed packages are available
#' if (all(unlist(lapply(
#'   c("metaplus", "metafor", "metaBMA"), # needed packages
#'   require,
#'   character.only = TRUE,
#'   quietly = TRUE,
#'   warn.conflicts = FALSE
#' )))) {
#'   # note that the `print` calls below are not necessary for you to write
#'   # they are in the documentation so that the website renders them
#'
#'   # setup
#'   set.seed(123)
#'   library(statsExpressions)
#'   options(tibble.width = Inf, pillar.bold = TRUE, pillar.neg = TRUE)
#'
#'   # renaming to what `statsExpressions` expects
#'   df <- dplyr::rename(mag, estimate = yi, std.error = sei)
#'
#'   # ----------------------- parametric ---------------------------------------
#'
#'   print(meta_analysis(data = df))
#'
#'   # ----------------------- random -----------------------------------------
#'
#'   print(meta_analysis(
#'     data = df,
#'     type = "random",
#'     random = "normal"
#'   ))
#'
#'   # ----------------------- Bayes Factor -----------------------------------
#'
#'   meta_analysis(
#'     data = df,
#'     type = "bayes",
#'
#'     # additional arguments given to `metaBMA`
#'     iter = 5000,
#'     summarize = "integrate",
#'     control = list(adapt_delta = 0.99, max_treedepth = 15)
#'   )
#' }
#' }
#' @export

# function body
meta_analysis <- function(data,
                          type = "parametric",
                          random = "mixture",
                          k = 2L,
                          conf.level = 0.95,
                          top.text = NULL,
                          ...) {
  # check the type of test
  type <- ipmisc::stats_type_switch(type)

  # additional arguments
  if (type != "bayes") {
    .f.args <- list(random = random, yi = quote(estimate), sei = quote(std.error), ...)
  } else {
    .f.args <- list(y = quote(estimate), SE = quote(std.error), ...)
  }

  # functions
  if (type == "parametric") c(.ns, .fn) %<-% c("metafor", "rma")
  if (type == "robust") c(.ns, .fn) %<-% c("metaplus", "metaplus")
  if (type == "bayes") c(.ns, .fn) %<-% c("metaBMA", "meta_random")

  # create a call and then extract dataframe with coefficients
  suppressMessages(suppressWarnings(stats_df <-
    eval(rlang::call2(.fn = .fn, .ns = .ns, data = data, !!!.f.args)) %>%
    tidy_model_parameters(include_studies = FALSE, ci = conf.level)))

  # new column
  if (type != "bayes") stats_df %<>% dplyr::mutate(effectsize = "meta-analytic summary estimate")
  if (type == "bayes") stats_df %<>% dplyr::mutate(effectsize = "meta-analytic posterior estimate")

  # preparing the expression
  stats_df %<>%
    dplyr::mutate(expression = list(expr_template(
      data = .,
      n = nrow(data),
      n.text = quote(italic("n")["effects"]),
      no.parameters = 0L,
      k = k,
      top.text = top.text,
      bayesian = ifelse(type == "bayes", TRUE, FALSE)
    )))

  # return the output
  as_tibble(stats_df)
}
