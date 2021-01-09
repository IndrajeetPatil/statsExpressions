#' @name expr_contingency_tab
#' @title Making expression for contingency table analysis
#'
#' @return Expression or a dataframe for contingency analysis (Pearson's
#'   chi-square test for independence for between-subjects design or McNemar's
#'   test for within-subjects design) or goodness of fit test for a single
#'   categorical variable.
#'
#' @references For more details, see-
#' \url{https://indrajeetpatil.github.io/statsExpressions/articles/stats_details.html}
#'
#' @param x The variable to use as the **rows** in the contingency table.
#' @param y The variable to use as the **columns** in the contingency table.
#'   Default is `NULL`. If `NULL`, one-sample proportion test (a goodness of fit
#'   test) will be run for the `x` variable. Otherwise association test will be
#'   carried out.
#' @param counts A string naming a variable in data containing counts, or `NULL`
#'   if each row represents a single observation.
#' @param paired Logical indicating whether data came from a within-subjects or
#'   repeated measures design study (Default: `FALSE`). If `TRUE`, McNemar's
#'   test expression will be returned. If `FALSE`, Pearson's chi-square test will
#'   be returned.
#' @param ratio A vector of proportions: the expected proportions for the
#'   proportion test (should sum to 1). Default is `NULL`, which means the null
#'   is equal theoretical proportions across the levels of the nominal variable.
#'   This means if there are two levels this will be `ratio = c(0.5,0.5)` or if
#'   there are four levels this will be `ratio = c(0.25,0.25,0.25,0.25)`, etc.
#' @param ... Additional arguments (currently ignored).
#' @inheritParams expr_t_twosample
#' @inheritParams stats::chisq.test
#' @inheritParams expr_oneway_anova
#'
#' @importFrom dplyr select mutate rename filter pull
#' @importFrom rlang enquo as_name ensym exec
#' @importFrom tidyr uncount drop_na
#' @importFrom stats mcnemar.test chisq.test
#' @importFrom effectsize cramers_v cohens_g
#' @importFrom parameters standardize_names
#'
#' @examples
#' # for reproducibility
#' set.seed(123)
#' library(statsExpressions)
#'
#' # ------------------------ association tests -----------------------------
#'
#' # without counts and between-subjects
#' expr_contingency_tab(
#'   data = mtcars,
#'   x = am,
#'   y = cyl,
#'   paired = FALSE
#' )
#'
#' # ------------------------ goodness of fit tests ---------------------------
#'
#' # with counts
#' expr_contingency_tab(
#'   data = as.data.frame(HairEyeColor),
#'   x = Eye,
#'   counts = Freq,
#'   ratio = c(0.2, 0.2, 0.3, 0.3)
#' )
#' @export

# function body
expr_contingency_tab <- function(data,
                                 x,
                                 y = NULL,
                                 counts = NULL,
                                 paired = FALSE,
                                 ratio = NULL,
                                 k = 2L,
                                 conf.level = 0.95,
                                 output = "expression",
                                 ...) {
  # one-way or two-way table?
  test <- ifelse(!rlang::quo_is_null(rlang::enquo(y)), "two.way", "one.way")

  # creating a dataframe
  data %<>%
    dplyr::select(.data = ., {{ x }}, {{ y }}, .counts = {{ counts }}) %>%
    tidyr::drop_na(.)

  # untable the dataframe based on the count for each observation
  if (".counts" %in% names(data)) data %<>% tidyr::uncount(data = ., weights = .counts)

  # variables needed for both one-way and two-way analysis
  x_vec <- data %>% dplyr::pull({{ x }})
  if (is.null(ratio)) ratio <- rep(1 / length(table(x_vec)), length(table(x_vec)))

  # ----------------------- arguments ---------------------------------------

  # default functions for analysis (only change for McNemar's test)
  c(.f, .f.es) %<-% c(stats::chisq.test, effectsize::cramers_v)

  # Pearson's or McNemar's test
  if (test == "two.way") {
    if (isTRUE(paired)) c(.f, .f.es) %<-% c(stats::mcnemar.test, effectsize::cohens_g)
    .f.args <- list(x = table(x_vec, data %>% dplyr::pull({{ y }})), correct = FALSE)
  }

  # goodness of fit test
  if (test == "one.way") {
    .f.args <- list(x = table(x_vec), p = ratio, correct = FALSE)
    paired <- FALSE
  }

  # ----------------------- returns ---------------------------------------

  # stats
  stats_df <-
    rlang::exec(.fn = .f, !!!.f.args) %>%
    tidy_model_parameters(.)

  # computing effect size + CI
  effsize_df <-
    rlang::exec(
      .fn = .f.es,
      adjust = TRUE,
      ci = conf.level,
      !!!.f.args
    ) %>%
    tidy_model_effectsize(.)

  # combining dataframes
  stats_df <- dplyr::bind_cols(stats_df, effsize_df)

  # expression
  expression <-
    expr_template(
      no.parameters = 1L,
      stats.df = stats_df,
      n = nrow(data),
      paired = paired,
      conf.level = conf.level,
      k = k
    )

  # return the output
  switch(output, "dataframe" = stats_df, expression)
}
