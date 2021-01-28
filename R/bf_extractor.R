#' @title Extract Bayes Factors from `BayesFactor` model object.
#' @name bf_extractor
#'
#' @param bf.object An object from `BayesFactor` package.
#' @param conf.level Confidence/Credible Interval (CI) level. Default to `0.95`
#'   (`95%`).
#' @param k Number of digits after decimal point (should be an integer)
#'   (Default: `k = 2L`).
#' @param top.text Text to display on top of the Bayes Factor message. This is
#'   mostly relevant in the context of `ggstatsplot` functions.
#' @param output If `"expression"`, will return expression with statistical
#'   details, while `"dataframe"` will return a dataframe containing the
#'   results.
#' @param ... Additional arguments passed to
#'   [parameters::model_parameters.BFBayesFactor()].
#'
#' @importFrom dplyr mutate filter rename_with matches
#' @importFrom performance r2_bayes
#' @importFrom parameters standardize_names
#'
#' @note *Important*: don't enter `1/bf.object` to extract results for null
#'   hypothesis; doing so will return wrong results.
#'
#' @examples
#' \donttest{
#' # setup
#' library(statsExpressions)
#' set.seed(123)
#'
#' # creating a `BayesFactor` object
#' bf_obj <-
#'   BayesFactor::anovaBF(
#'     formula = Sepal.Length ~ Species,
#'     data = iris,
#'     progress = FALSE
#'   )
#'
#' # extracting Bayes Factors in a dataframe
#' bf_extractor(bf_obj)
#' }
#' @export

# function body
bf_extractor <- function(bf.object,
                         conf.level = 0.95,
                         k = 2L,
                         top.text = NULL,
                         output = "dataframe",
                         ...) {
  # basic parameters dataframe
  stats_df <- suppressMessages(tidy_model_parameters(bf.object, ci = conf.level, ...))

  # ------------------------ ANOVA designs ------------------------------

  if ("method" %in% names(stats_df)) {
    if (stats_df$method[[1]] == "Bayes factors for linear models") {
      # dataframe with posterior estimates for R-squared
      df_r2 <-
        performance::r2_bayes(bf.object, average = TRUE, ci = conf.level) %>%
        as_tibble(.) %>%
        parameters::standardize_names(data = ., style = "broom") %>%
        dplyr::rename_with(.fn = ~ paste0("r2.", .x), .cols = dplyr::matches("^conf|^comp")) %>%
        dplyr::select(-ci.width)

      # for within-subjects design, retain only marginal component
      if ("r2.component" %in% names(df_r2)) df_r2 %<>% dplyr::filter(r2.component == "conditional")

      # combine everything
      stats_df %<>% dplyr::bind_cols(., df_r2)
    }
  }

  # Bayes Factor expression
  expression <-
    expr_template(
      top.text = top.text,
      stats.df = stats_df,
      k = k,
      bayesian = TRUE
    )

  # return the text results or the dataframe with results
  switch(output, "dataframe" = as_tibble(stats_df), expression)
}
