#' @title Contingency table analyses
#' @name contingency_table
#'
#' @description
#'
#' A dataframe containing results from for contingency table analysis or
#' goodness of fit test.
#'
#' To see details about functions which are internally used to carry out these
#' analyses, see the following vignette-
#' <https://indrajeetpatil.github.io/statsExpressions/articles/stats_details.html>
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
#' @param sampling.plan Character describing the sampling plan. Possible options
#'   are `"indepMulti"` (independent multinomial; default), `"poisson"`,
#'   `"jointMulti"` (joint multinomial), `"hypergeom"` (hypergeometric). For
#'   more, see `?BayesFactor::contingencyTableBF()`.
#' @param fixed.margin For the independent multinomial sampling plan, which
#'   margin is fixed (`"rows"` or `"cols"`). Defaults to `"rows"`.
#' @param prior.concentration Specifies the prior concentration parameter, set
#'   to `1` by default. It indexes the expected deviation from the null
#'   hypothesis under the alternative, and corresponds to Gunel and Dickey's
#'   (1974) `"a"` parameter.
#' @param ratio A vector of proportions: the expected proportions for the
#'   proportion test (should sum to 1). Default is `NULL`, which means the null
#'   is equal theoretical proportions across the levels of the nominal variable.
#'   This means if there are two levels this will be `ratio = c(0.5,0.5)` or if
#'   there are four levels this will be `ratio = c(0.25,0.25,0.25,0.25)`, etc.
#' @param ... Additional arguments (currently ignored).
#' @inheritParams two_sample_test
#' @inheritParams stats::chisq.test
#' @inheritParams oneway_anova
#'
#' @importFrom BayesFactor contingencyTableBF logMeanExpLogs
#' @importFrom dplyr pull select rename mutate
#' @importFrom rlang enquo quo_is_null exec !!!
#' @importFrom tidyr uncount drop_na
#' @importFrom stats mcnemar.test chisq.test dmultinom rgamma
#' @importFrom effectsize cramers_v cohens_g
#' @importFrom parameters standardize_names
#' @importFrom insight format_value
#'
#' @examples
#' \donttest{
#' # for reproducibility
#' set.seed(123)
#' library(statsExpressions)
#' options(tibble.width = Inf, pillar.bold = TRUE, pillar.neg = TRUE)
#'
#' # ------------------------ non-Bayesian -----------------------------
#'
#' # association test
#' contingency_table(
#'   data = mtcars,
#'   x = am,
#'   y = cyl,
#'   paired = FALSE
#' )
#'
#' # goodness-of-fit test
#' contingency_table(
#'   data = as.data.frame(HairEyeColor),
#'   x = Eye,
#'   counts = Freq,
#'   ratio = c(0.2, 0.2, 0.3, 0.3)
#' )
#'
#' # ------------------------ Bayesian -----------------------------
#'
#' # association test
#' contingency_table(
#'   data = mtcars,
#'   x = am,
#'   y = cyl,
#'   paired = FALSE,
#'   type = "bayes"
#' )
#'
#' # goodness-of-fit test
#' contingency_table(
#'   data = as.data.frame(HairEyeColor),
#'   x = Eye,
#'   counts = Freq,
#'   ratio = c(0.2, 0.2, 0.3, 0.3),
#'   type = "bayes"
#' )
#' }
#' @export

# function body
contingency_table <- function(data,
                              x,
                              y = NULL,
                              paired = FALSE,
                              type = "parametric",
                              counts = NULL,
                              ratio = NULL,
                              k = 2L,
                              conf.level = 0.95,
                              sampling.plan = "indepMulti",
                              fixed.margin = "rows",
                              prior.concentration = 1,
                              top.text = NULL,
                              ...) {

  # check the data contains needed column
  type <- stats_type_switch(type)

  # one-way or two-way table?
  test <- ifelse(!rlang::quo_is_null(rlang::enquo(y)), "two.way", "one.way")

  # creating a dataframe
  data %<>%
    dplyr::select({{ x }}, {{ y }}, .counts = {{ counts }}) %>%
    tidyr::drop_na(.)

  # untable the dataframe based on the count for each observation
  if (".counts" %in% names(data)) data %<>% tidyr::uncount(weights = .counts)

  # variables needed for both one-way and two-way analysis
  x_vec <- data %>% dplyr::pull({{ x }})
  if (is.null(ratio)) ratio <- rep(1 / length(table(x_vec)), length(table(x_vec)))

  # ----------------------- non-Bayesian ---------------------------------------

  if (type != "bayes") {
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

    # combining dataframes: inferential stats + effect sizes
    stats_df <- dplyr::bind_cols(
      tidy_model_parameters(rlang::exec(.f, !!!.f.args)),
      tidy_model_effectsize(rlang::exec(.f.es, adjust = TRUE, ci = conf.level, !!!.f.args))
    )
  }

  # ----------------------- Bayesian ---------------------------------------

  if (type == "bayes") {
    # two-way table
    if (test == "two.way") {
      # extract a tidy dataframe
      stats_df <- BayesFactor::contingencyTableBF(
        table(data %>% dplyr::pull({{ x }}), data %>% dplyr::pull({{ y }})),
        sampleType = sampling.plan,
        fixedMargin = fixed.margin,
        priorConcentration = prior.concentration
      ) %>%
        tidy_model_parameters(ci = conf.level)
    }

    # one-way table
    if (test == "one.way") {
      xtab <- table(data %>% dplyr::pull({{ x }}))

      # probability can't be exactly 0 or 1
      if (1 / length(as.vector(xtab)) == 0 || 1 / length(as.vector(xtab)) == 1) {
        return(NULL)
      }

      # use it
      p1s <- rdirichlet_int(n = 100000, alpha = prior.concentration * ratio)

      # prob
      tmp_pr_h1 <- sapply(
        X = 1:100000,
        FUN = function(i) stats::dmultinom(x = as.matrix(xtab), prob = p1s[i, ], log = TRUE)
      )

      # BF = (log) prob of data under alternative - (log) prob of data under null
      bf <- BayesFactor::logMeanExpLogs(tmp_pr_h1) -
        stats::dmultinom(as.matrix(xtab), prob = ratio, log = TRUE)

      # computing Bayes Factor and formatting the results
      stats_df <- tibble(bf10 = exp(bf), prior.scale = prior.concentration)

      # final expression
      expression <- substitute(
        atop(
          displaystyle(top.text),
          expr = paste(
            "log"["e"] * "(BF"["01"] * ") = " * bf * ", ",
            italic("a")["Gunel-Dickey"] * " = " * a
          )
        ),
        env = list(
          top.text = top.text,
          bf = format_value(-log(stats_df$bf10[[1]]), k),
          a = format_value(stats_df$prior.scale[[1]], k)
        )
      )

      # the final expression
      if (is.null(top.text)) expression <- expression$expr

      # computing Bayes Factor and formatting the results
      stats_df %<>% dplyr::mutate(expression = list(expression))
    }
  }

  # ----------------------- expression ---------------------------------------

  if (!(type == "bayes" && test == "one.way")) {
    stats_df %<>%
      dplyr::mutate(
        expression = list(expr_template(
          data = .,
          no.parameters = 1L,
          n = nrow(data),
          paired = paired,
          k = k,
          top.text = top.text,
          bayesian = ifelse(type == "bayes", TRUE, FALSE)
        ))
      )
  }

  # return the output
  stats_df
}


#' @title estimate log prob of data under null with Monte Carlo
#' @note `rdirichlet` function from `MCMCpack`
#' @noRd

rdirichlet_int <- function(n, alpha) {
  l <- length(alpha)
  x <- matrix(stats::rgamma(l * n, alpha), ncol = l, byrow = TRUE)
  x / as.vector(x %*% rep(1, l))
}
