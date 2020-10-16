#' @name expr_contingency_tab
#' @rdname expr_contingency_tab
#' @title Making expression for contingency table and goodness of fit tests
#'
#' @return Expression for contingency analysis (Pearson's chi-square test for
#'   independence for between-subjects design or McNemar's test for
#'   within-subjects design) or goodness of fit test for a single categorical
#'   variable.
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
#'   test subtitle will be returned. If `FALSE`, Pearson's chi-square test will
#'   be returned.
#' @param ratio A vector of proportions: the expected proportions for the
#'   proportion test (should sum to 1). Default is `NULL`, which means the null
#'   is equal theoretical proportions across the levels of the nominal variable.
#'   This means if there are two levels this will be `ratio = c(0.5,0.5)` or if
#'   there are four levels this will be `ratio = c(0.25,0.25,0.25,0.25)`, etc.
#' @param ... Additional arguments (currently ignored).
#' @inheritParams expr_t_parametric
#' @inheritParams stats::chisq.test
#' @inheritParams expr_anova_parametric
#' @inheritParams expr_anova_nonparametric
#'
#' @importFrom dplyr select mutate rename filter
#' @importFrom rlang !! enquo as_name ensym exec
#' @importFrom tidyr uncount drop_na
#' @importFrom stats mcnemar.test chisq.test
#' @importFrom effectsize cramers_v cohens_g
#' @importFrom insight standardize_names
#'
#' @details For more details about how the effect sizes and their confidence
#'   intervals were computed, see documentation in `?effectsize::cramers_v` and
#'   `?effectsize::cohens_g`.
#'
#' @examples
#' # for reproducibility
#' set.seed(123)
#' library(statsExpressions)
#'
#' # ------------------------ association tests -----------------------------
#'
#' # without counts data
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

  # ensure the variables work quoted or unquoted
  x <- rlang::ensym(x)
  y <- if (!rlang::quo_is_null(rlang::enquo(y))) rlang::ensym(y)
  counts <- if (!rlang::quo_is_null(rlang::enquo(counts))) rlang::ensym(counts)

  # =============================== dataframe ================================

  # creating a dataframe
  data %<>%
    dplyr::select(.data = ., {{ x }}, {{ y }}, {{ counts }}) %>%
    tidyr::drop_na(data = .) %>%
    as_tibble(x = .)

  # x and y need to be factors; drop the unused levels of the factors

  # x
  data %<>% dplyr::mutate(.data = ., {{ x }} := droplevels(as.factor({{ x }})))

  # untable the dataframe based on the count for each observation
  if (!rlang::quo_is_null(rlang::enquo(counts))) {
    data %<>%
      tidyr::uncount(
        data = .,
        weights = {{ counts }},
        .remove = TRUE,
        .id = "id"
      )
  }

  # sample size
  sample_size <- nrow(data)

  # ratio
  if (is.null(ratio)) {
    x_vec <- data %>% dplyr::pull({{ x }})
    ratio <- rep(1 / length(table(x_vec)), length(table(x_vec)))
  }

  # =============================== association tests ========================

  # association tests
  if (!rlang::quo_is_null(rlang::enquo(y))) {
    # drop the unused levels of the column variable
    data %<>% dplyr::mutate(.data = ., {{ y }} := droplevels(as.factor({{ y }})))

    # creating a matrix with frequencies and cleaning it up
    x_arg <- table(data %>% dplyr::pull({{ x }}), data %>% dplyr::pull({{ y }}))

    # ======================== Pearson's test ================================

    if (isFALSE(paired)) {
      # computing stats and effect size + CI
      mod <- stats::chisq.test(x = x_arg, correct = FALSE)
      .f <- effectsize::cramers_v
      effsize.text <- quote(widehat(italic("V"))["Cramer"])

      n.text <- quote(italic("n")["obs"])
    }

    # ======================== McNemar's test ================================

    if (isTRUE(paired)) {
      # computing stats and effect size + CI
      mod <- stats::mcnemar.test(x = x_arg, correct = FALSE)
      .f <- effectsize::cohens_g
      effsize.text <- quote(widehat(italic("g"))["Cohen"])

      n.text <- quote(italic("n")["pairs"])
    }
  }

  # ======================== goodness of fit test ========================

  if (rlang::quo_is_null(rlang::enquo(y))) {
    # frequency table
    x_arg <- table(data %>% dplyr::pull({{ x }}))

    # checking if the chi-squared test can be run
    mod <- stats::chisq.test(x = x_arg, correct = FALSE, p = ratio)

    # effect size text
    .f <- effectsize::cramers_v
    effsize.text <- quote(widehat(italic("V"))["Cramer"])
    n.text <- quote(italic("n")["obs"])
  }

  # which test was carried out?
  if (mod$method == "Chi-squared test for given probabilities") {
    statistic.text <- quote(chi["gof"]^2)
  } else {
    if (isTRUE(paired)) statistic.text <- quote(chi["McNemar"]^2)
    if (isFALSE(paired)) statistic.text <- quote(chi["Pearson"]^2)
  }

  # computing effect size + CI
  effsize_df <-
    rlang::exec(
      .fn = .f,
      x = x_arg,
      adjust = TRUE,
      ci = conf.level
    ) %>%
    insight::standardize_names(data = ., style = "broom")

  # combining dataframes
  stats_df <- dplyr::bind_cols(broomExtra::tidy(mod), effsize_df)

  # expression
  subtitle <-
    expr_template(
      no.parameters = 1L,
      stats.df = stats_df,
      statistic.text = statistic.text,
      effsize.text = effsize.text,
      n = sample_size,
      n.text = n.text,
      conf.level = conf.level,
      k = k
    )

  # return the output
  switch(output,
    "expression" = subtitle,
    "dataframe" = stats_df,
    subtitle
  )
}
