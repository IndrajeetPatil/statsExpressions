#' @name expr_contingency_tab
#' @title Making expression for contingency table and goodness of fit tests
#' @author \href{https://github.com/IndrajeetPatil}{Indrajeet Patil}
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
#' @param y The variable to use as the **columns** in the contingency
#'   table. Default is `NULL`. If `NULL`, one-sample proportion test (a goodness
#'   of fit test) will be run for the `main` variable. Otherwise an appropriate
#'   association test will be run.
#' @param counts A string naming a variable in data containing counts, or `NULL`
#'   if each row represents a single observation (Default).
#' @param paired Logical indicating whether data came from a within-subjects or
#'   repeated measures design study (Default: `FALSE`). If `TRUE`, McNemar's
#'   test subtitle will be returned. If `FALSE`, Pearson's chi-square test will
#'   be returned.
#' @param stat.title Title for the effect being investigated with the chi-square
#'   test. The default is `NULL`, i.e. no title will be added to describe the
#'   effect being shown. An example of a `stat.title` argument will be something
#'   like `"main x condition"` or `"interaction"`.
#' @param bias.correct If `TRUE` (default), a bias correction will be applied to
#'   Cramer's *V*.
#' @param ratio A vector of proportions: the expected proportions for the
#'   proportion test (should sum to 1). Default is `NULL`, which means the null
#'   is equal theoretical proportions across the levels of the nominal variable.
#'   This means if there are two levels this will be `ratio = c(0.5,0.5)` or if
#'   there are four levels this will be `ratio = c(0.25,0.25,0.25,0.25)`, etc.
#' @param legend.title Title text for the legend.
#' @param ... Additional arguments (currently ignored).
#' @inheritParams t1way_ci
#' @inheritParams expr_t_parametric
#' @inheritParams stats::chisq.test
#' @inheritParams expr_anova_parametric
#'
#' @importFrom dplyr select mutate rename filter
#' @importFrom rlang !! enquo as_name ensym exec
#' @importFrom tibble tribble as_tibble
#' @importFrom tidyr uncount drop_na
#' @importFrom stats mcnemar.test chisq.test
#' @importFrom rcompanion cramerV cohenG cramerVFit
#'
#' @details For more details about how the effect sizes and their confidence
#'   intervals were computed, see documentation in `?rcompanion::cramerV`,
#'   `?rcompanion::cramerVFit`, and `?rcompanion::cohenG`.
#'
#' @examples
#'
#' \donttest{
#' # ------------------------ association tests -----------------------------
#'
#' set.seed(123)
#' library(statsExpressions)
#'
#' # without counts data
#' statsExpressions::expr_contingency_tab(
#'   data = mtcars,
#'   x = am,
#'   y = cyl,
#'   paired = FALSE,
#'   nboot = 15
#' )
#'
#' # ------------------------ goodness of fit tests ---------------------------
#'
#' # for reproducibility
#' set.seed(123)
#'
#' # with counts
#' expr_contingency_tab(
#'   data = as.data.frame(HairEyeColor),
#'   x = Eye,
#'   counts = Freq,
#'   ratio = c(0.2, 0.2, 0.3, 0.3)
#' )
#' }
#' @export

# function body
expr_contingency_tab <- function(data,
                                 x,
                                 y = NULL,
                                 counts = NULL,
                                 ratio = NULL,
                                 nboot = 100,
                                 paired = FALSE,
                                 stat.title = NULL,
                                 legend.title = NULL,
                                 conf.level = 0.95,
                                 conf.type = "norm",
                                 bias.correct = TRUE,
                                 k = 2,
                                 messages = TRUE,
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
    tibble::as_tibble(x = .)

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

  # y
  if (!rlang::quo_is_null(rlang::enquo(y))) {
    # drop the unused levels of the column variable
    data %<>% dplyr::mutate(.data = ., {{ y }} := droplevels(as.factor({{ y }})))

    # in case there is no variation, no subtitle will be shown
    if (nlevels(data %>% dplyr::pull({{ y }}))[[1]] == 1L) {
      # display message
      message(cat(
        crayon::red("Error: "),
        crayon::blue("Row variable 'y' contains less than 2 levels.\n"),
        crayon::blue("Chi-squared test can't be run; no subtitle displayed."),
        sep = ""
      ))

      # return early
      return(NULL)
    }
  }

  # =============================== association tests ========================

  # sample size
  sample_size <- nrow(data)

  # ratio
  if (is.null(ratio)) {
    x_vec <- data %>% dplyr::pull({{ x }})
    ratio <- rep(1 / length(table(x_vec)), length(table(x_vec)))
  }

  # association tests
  if (!rlang::quo_is_null(rlang::enquo(y))) {

    # creating a matrix with frequencies and cleaning it up
    x_arg <- as.matrix(table(data %>% dplyr::pull({{ x }}), data %>% dplyr::pull({{ y }})))

    # ======================== Pearson's test ================================

    if (isFALSE(paired)) {
      # object containing stats
      stats_df <- stats::chisq.test(x = x_arg, correct = FALSE)

      # effect size text
      .f <- rcompanion::cramerV
      effsize.text <- quote(widehat(italic("V"))["Cramer"])
      statistic.text <- quote(chi["Pearson"]^2)
      n.text <- quote(italic("n")["obs"])
    }

    # ======================== McNemar's test ================================

    if (isTRUE(paired)) {
      # computing effect size + CI
      stats_df <- stats::mcnemar.test(x = x_arg, correct = FALSE)

      # effect size text
      .f <- rcompanion::cohenG
      effsize.text <- quote(widehat(italic("g"))["Cohen"])
      statistic.text <- quote(chi["McNemar"]^2)
      n.text <- quote(italic("n")["pairs"])
    }

    args_list <- list(x = x_arg, bias.correct = bias.correct)
  }

  # ======================== goodness of fit test ========================

  if (rlang::quo_is_null(rlang::enquo(y))) {
    # frequency table
    x_arg <- table(data %>% dplyr::pull({{ x }}))

    # checking if the chi-squared test can be run
    stats_df <-
      tryCatch(
        expr = stats::chisq.test(x = x_arg, correct = FALSE, p = ratio),
        error = function(x) NULL
      )

    # if the function worked, then return tidy output
    if (is.null(stats_df)) {
      return(NULL)
    }

    # `x` argument for effect size function
    x_arg <- as.vector(table(data %>% dplyr::pull({{ x }})))

    # effect size text
    .f <- rcompanion::cramerVFit
    effsize.text <- quote(widehat(italic("V"))["Cramer"])
    statistic.text <- quote(chi["gof"]^2)
    n.text <- quote(italic("n")["obs"])
    args_list <- list(x = x_arg, p = ratio)
  }

  # computing effect size + CI
  effsize_df <-
    rlang::exec(
      .fn = .f,
      !!!args_list,
      ci = TRUE,
      conf = conf.level,
      type = conf.type,
      R = nboot,
      histogram = FALSE,
      digits = 5,
      reportIncomplete = TRUE
    ) %>%
    rcompanion_cleaner(.)


  # for Cohen's g
  if ("Statistic" %in% names(effsize_df)) {
    effsize_df %<>% dplyr::filter(.data = ., Statistic == "g")
  }

  # preparing subtitle
  subtitle <-
    expr_template(
      no.parameters = 1L,
      stats.df = broomExtra::tidy(stats_df),
      effsize.df = effsize_df,
      stat.title = stat.title,
      statistic.text = statistic.text,
      effsize.text = effsize.text,
      n = sample_size,
      n.text = n.text,
      conf.level = conf.level,
      k = k
    )

  # message about effect size measure
  if (isTRUE(messages)) effsize_ci_message(nboot, conf.level)

  # return the subtitle
  return(subtitle)
}

# aliases -----------------------------------------------------------------

#' @rdname expr_contingency_tab
#' @aliases expr_contingency_tab
#' @export

expr_onesample_proptest <- expr_contingency_tab
