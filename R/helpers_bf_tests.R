#' @title Extract Bayes Factors from `BayesFactor` model object.
#' @name bf_extractor
#' @author \href{https://github.com/IndrajeetPatil}{Indrajeet Patil}
#'
#' @param bf.object An object from `BayesFactor` package.
#' @param ... Currently ignored.
#'
#' @importFrom BayesFactor extractBF
#' @importFrom tibble as_tibble enframe
#' @importFrom dplyr rename select mutate
#'
#' @examples
#' # getting only Bayes Factors
#' statsExpressions::bf_extractor(
#'   BayesFactor::anovaBF(
#'     formula = Sepal.Length ~ Species,
#'     data = iris,
#'     progress = FALSE
#'   )
#' )
#' @export

# function body
bf_extractor <- function(bf.object, ...) {
  BayesFactor::extractBF(
    x = bf.object,
    logbf = FALSE,
    onlybf = FALSE
  ) %>%
    tibble::as_tibble(.) %>%
    dplyr::select(.data = ., -time, -code) %>%
    dplyr::rename(.data = ., bf10 = bf) %>%
    bf_formatter(.)
}

#' @title Prepare caption with expression for Bayes Factor results
#' @name bf_expr
#' @description Convenience function to write a caption message with bayes
#'   factors in favor of the null hypothesis.
#'
#' @param bf.df A dataframe containing two columns `log_e_bf01` (for evidence in
#'   favor of null hypothesis) and `bf.prior`. If dataframe contains more than
#'   two rows, only the first row will be used.
#' @param caption Text to display as caption (will be displayed on top of the
#'   Bayes Factor caption/message).
#' @param output Can either be `"null"` (or `"caption"` or `"H0"` or `"h0"`),
#'   which will return expression with evidence in favor of the null hypothesis,
#'   or `"alternative"` (or `"title"` or `"H1"` or `"h1"`), which will return
#'   expression with evidence in favor of the alternative hypothesis, or
#'   `"results"`, which will return a dataframe with results all the details).
#' @param ... Additional arguments (ignored).
#' @inheritParams expr_template
#'
#' @examples
#' \donttest{
#' # for reproducibility
#' set.seed(123)
#'
#' # dataframe containing results
#' bf.df <-
#'   statsExpressions::bf_extractor(BayesFactor::correlationBF(
#'     x = iris$Sepal.Length,
#'     y = iris$Petal.Length
#'   )) %>%
#'   dplyr::mutate(.data = ., bf.prior = 0.707)
#'
#' # creating caption (for null)
#' statsExpressions::bf_expr(
#'   bf.df = bf.df,
#'   output = "null",
#'   k = 3,
#'   caption = "Note: Iris dataset"
#' )
#'
#' # creating caption (for alternative)
#' statsExpressions::bf_expr(
#'   bf.df = bf.df,
#'   output = "alternative"
#' )
#' }
#' @export

# function body
bf_expr <- function(bf.df,
                    k = 2,
                    output = "null",
                    caption = NULL,
                    ...) {

  # changing aspects of the caption based on what output is needed
  if (output %in% c("null", "caption", "H0", "h0")) {
    hypothesis.text <- "In favor of null: "
    bf.value <- bf.df$log_e_bf01[[1]]
    bf.subscript <- "01"
  } else {
    hypothesis.text <- "In favor of alternative: "
    bf.value <- -bf.df$log_e_bf01[[1]]
    bf.subscript <- "10"
  }

  # prepare the Bayes Factor message
  bf_text <-
    substitute(
      atop(displaystyle(top.text),
        expr = paste(
          hypothesis.text,
          "log"["e"],
          "(BF"[bf.subscript],
          ") = ",
          bf,
          ", ",
          italic("r")["Cauchy"]^"JZS",
          " = ",
          bf_prior
        )
      ),
      env = list(
        hypothesis.text = hypothesis.text,
        top.text = caption,
        bf.subscript = bf.subscript,
        bf = specify_decimal_p(x = bf.value, k = k),
        bf_prior = specify_decimal_p(x = bf.df$bf.prior[[1]], k = k)
      )
    )

  # return the caption
  return(bf_text)
}

#' @title Bayesian correlation test.
#' @name bf_corr_test
#' @author \href{https://github.com/IndrajeetPatil}{Indrajeet Patil}
#'
#' @inheritParams expr_corr_test
#' @inheritParams bf_expr
#' @param bf.prior A numeric value between `0.5` and `2` (default `0.707`), the
#'   prior width to use in calculating Bayes Factors.
#'
#' @importFrom BayesFactor correlationBF
#' @importFrom dplyr pull
#'
#' @seealso \code{\link{bf_contingency_tab}}, \code{\link{bf_oneway_anova}},
#' \code{\link{bf_ttest}}
#'
#' @examples
#' \donttest{
#' # for reproducibility
#' set.seed(123)
#'
#' # to get caption (default)
#' bf_corr_test(
#'   data = anscombe,
#'   x = x1,
#'   y = y4,
#'   bf.prior = 1
#' )
#'
#' # to see results
#' bf_corr_test(
#'   data = anscombe,
#'   x = x1,
#'   y = y4,
#'   bf.prior = 0.8,
#'   output = "results"
#' )
#' }
#' @export

# function body
bf_corr_test <- function(data,
                         x,
                         y,
                         bf.prior = 0.707,
                         caption = NULL,
                         output = "null",
                         k = 2,
                         ...) {

  # make sure both quoted and unquoted arguments are allowed
  c(x, y) %<-% c(rlang::ensym(x), rlang::ensym(y))

  # ============================ data preparation ==========================

  # creating a dataframe
  data %<>%
    dplyr::select(.data = ., {{ x }}, {{ y }}) %>%
    tidyr::drop_na(data = .) %>%
    tibble::as_tibble(.)

  # ========================= subtitle preparation ==========================

  # extracting results from Bayesian test and creating a dataframe
  bf.df <-
    bf_extractor(
      BayesFactor::correlationBF(
        x = data %>% dplyr::pull({{ x }}),
        y = data %>% dplyr::pull({{ y }}),
        nullInterval = NULL,
        rscale = bf.prior,
        ...
      )
    ) %>% # adding prior width column
    dplyr::mutate(.data = ., bf.prior = bf.prior)

  # prepare the Bayes Factor message
  if (output != "results") {
    bf_message <-
      bf_expr(
        bf.df = bf.df,
        output = output,
        k = k,
        caption = caption
      )
  }

  # ============================ return ==================================

  # return the text results or the dataframe with results
  return(switch(
    EXPR = output,
    "results" = bf.df,
    bf_message
  ))
}


#' @title Bayesian contingency table analysis
#' @name bf_contingency_tab
#' @author \href{https://github.com/IndrajeetPatil}{Indrajeet Patil}
#'
#' @inheritParams BayesFactor::contingencyTableBF
#' @inheritParams expr_contingency_tab
#' @inheritParams bf_corr_test
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
#'
#' @importFrom BayesFactor contingencyTableBF logMeanExpLogs
#' @importFrom stats dmultinom rgamma
#' @importFrom dplyr pull select rename mutate tibble
#' @importFrom tidyr uncount drop_na
#'
#' @seealso \code{\link{bf_corr_test}}, \code{\link{bf_oneway_anova}},
#' \code{\link{bf_ttest}}
#'
#' @note Bayes Factor for goodness of fit test is based on gist provided by
#'   Richard Morey:
#'   \url{https://gist.github.com/richarddmorey/a4cd3a2051f373db917550d67131dba4}.
#'
#' @examples
#'
#' \donttest{
#' # ------------------ association tests --------------------------------
#'
#' # for reproducibility
#' set.seed(123)
#' library(statsExpressions)
#'
#' # to get caption (in favor of null)
#' bf_contingency_tab(
#'   data = mtcars,
#'   x = am,
#'   y = cyl,
#'   fixed.margin = "cols"
#' )
#'
#' # to get caption (in favor of alternative)
#' bf_contingency_tab(
#'   data = mtcars,
#'   x = am,
#'   y = cyl,
#'   fixed.margin = "rows",
#'   output = "alternative"
#' )
#'
#' # to see results
#' bf_contingency_tab(
#'   data = mtcars,
#'   x = am,
#'   y = cyl,
#'   sampling.plan = "jointMulti",
#'   fixed.margin = "rows",
#'   prior.concentration = 1
#' )
#'
#' # ------------------ goodness of fit tests --------------------------------
#'
#' bf_contingency_tab(
#'   data = mtcars,
#'   x = am,
#'   prior.concentration = 10
#' )
#' }
#' @export

# function body
bf_contingency_tab <- function(data,
                               x,
                               y = NULL,
                               counts = NULL,
                               ratio = NULL,
                               sampling.plan = "indepMulti",
                               fixed.margin = "rows",
                               prior.concentration = 1,
                               caption = NULL,
                               output = "null",
                               k = 2,
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

  # x
  data %<>% dplyr::mutate(.data = ., {{ x }} := droplevels(as.factor({{ x }})))

  # ========================= contingency tabs =============================

  if (!rlang::quo_is_null(rlang::enquo(y))) {
    # dropping unused levels
    data %<>% dplyr::mutate(.data = ., {{ y }} := droplevels(as.factor({{ y }})))

    # detailed text of sample plan
    sampling_plan_text <-
      switch(
        EXPR = sampling.plan,
        "jointMulti" = "joint multinomial",
        "poisson" = "poisson",
        "indepMulti" = "independent multinomial",
        "hypergeom" = "hypergeometric"
      )

    # extracting results from Bayesian test and creating a dataframe
    bf.df <-
      bf_extractor(
        BayesFactor::contingencyTableBF(
          x = table(
            data %>% dplyr::pull({{ x }}),
            data %>% dplyr::pull({{ y }})
          ),
          sampleType = sampling.plan,
          fixedMargin = fixed.margin,
          priorConcentration = prior.concentration,
          ...
        )
      ) %>%
      dplyr::mutate(
        .data = .,
        sampling.plan = sampling_plan_text,
        fixed.margin = fixed.margin,
        prior.concentration = prior.concentration
      )
  }

  # ========================= goodness of fit =============================

  if (rlang::quo_is_null(rlang::enquo(y))) {
    # ratio
    if (is.null(ratio)) {
      x_length <- length(table(data %>% dplyr::pull({{ x }})))
      ratio <- rep(1 / x_length, x_length)
    }

    # no. of levels in `x` variable
    n_levels <- length(as.vector(table(data %>% dplyr::pull({{ x }}))))

    # probability can't be exactly 0 or 1
    if (1 / n_levels == 0 || 1 / n_levels == 1) {
      return(NULL)
    }

    # one sample goodness of fit test for equal proportions
    x_vec <- as.matrix(table(data %>% dplyr::pull({{ x }})))

    # (log) prob of data under null
    pr_y_h0 <- stats::dmultinom(x = x_vec, prob = ratio, log = TRUE)

    # estimate log prob of data under null with Monte Carlo
    M <- 100000

    # `rdirichlet` function from `MCMCpack`
    rdirichlet_int <- function(n, alpha) {
      l <- length(alpha)
      x <- matrix(stats::rgamma(l * n, alpha), ncol = l, byrow = TRUE)
      sm <- x %*% rep(1, l)
      return(x / as.vector(sm))
    }

    # use it
    p1s <- rdirichlet_int(n = M, alpha = prior.concentration * ratio)

    # prob
    tmp_pr_h1 <-
      sapply(
        X = 1:M,
        FUN = function(i) stats::dmultinom(x = x_vec, prob = p1s[i, ], log = TRUE)
      )

    # estimate log prob of data under alternative
    pr_y_h1 <- BayesFactor::logMeanExpLogs(tmp_pr_h1)

    # computing Bayes Factor and formatting the results
    bf.df <-
      dplyr::tibble(bf10 = exp(pr_y_h1 - pr_y_h0)) %>%
      bf_formatter(.) %>%
      dplyr::mutate(.data = ., prior.concentration = prior.concentration)
  }

  # ========================= caption preparation =============================

  # changing aspects of the caption based on what output is needed
  if (output %in% c("null", "caption", "H0", "h0")) {
    hypothesis.text <- "In favor of null: "
    bf.value <- bf.df$log_e_bf01[[1]]
    bf.subscript <- "01"
  } else {
    hypothesis.text <- "In favor of alternative: "
    bf.value <- -bf.df$log_e_bf01[[1]]
    bf.subscript <- "10"
  }

  # prepare the Bayes Factor message for contingency table
  if (!rlang::quo_is_null(rlang::enquo(y))) {
    bf_message <-
      substitute(
        atop(
          displaystyle(top.text),
          expr = paste(
            hypothesis.text,
            "log"["e"],
            "(BF"[bf.subscript],
            ") = ",
            bf,
            ", sampling = ",
            sampling.plan,
            ", ",
            italic("a"),
            " = ",
            a
          )
        ),
        env = list(
          hypothesis.text = hypothesis.text,
          top.text = caption,
          bf.subscript = bf.subscript,
          bf = specify_decimal_p(x = bf.value, k = k),
          sampling.plan = sampling_plan_text,
          a = specify_decimal_p(x = bf.df$prior.concentration[[1]], k = k)
        )
      )
  }

  # prepare the Bayes Factor message for goodness of fit
  if (rlang::quo_is_null(rlang::enquo(y))) {
    bf_message <-
      substitute(
        atop(
          displaystyle(top.text),
          expr = paste(
            hypothesis.text,
            "log"["e"],
            "(BF"[bf.subscript],
            ") = ",
            bf,
            ", ",
            italic("a"),
            " = ",
            a
          )
        ),
        env = list(
          hypothesis.text = hypothesis.text,
          top.text = caption,
          bf.subscript = bf.subscript,
          bf = specify_decimal_p(x = bf.value, k = k),
          a = specify_decimal_p(x = bf.df$prior.concentration[[1]], k = k)
        )
      )
  }

  # ============================ return ==================================

  # return the text results or the dataframe with results
  return(switch(
    EXPR = output,
    "results" = bf.df,
    bf_message
  ))
}


#' @rdname bf_contingency_tab
#' @aliases bf_contingency_tab
#' @export

bf_onesample_proptest <- bf_contingency_tab


#' @title Bayes Factor for *t*-test
#' @author \href{https://github.com/IndrajeetPatil}{Indrajeet Patil}
#' @details If `y` is `NULL`, a one-sample *t*-test will be carried out,
#'   otherwise a two-sample *t*-test will be carried out.
#'
#' @param x Either the grouping variable from the dataframe `data` if it's a
#'   two-sample *t*-test or a numeric variable if it's a one-sample *t*-test.
#' @inheritParams expr_t_parametric
#' @inheritParams BayesFactor::ttestBF
#' @inheritParams bf_corr_test
#' @inheritParams expr_t_onesample
#'
#' @importFrom BayesFactor ttestBF
#' @importFrom rlang !! quo_is_null new_formula ensym enquo
#'
#' @seealso \code{\link{bf_contingency_tab}}, \code{\link{bf_corr_test}},
#' \code{\link{bf_oneway_anova}}
#'
#' @examples
#'
#' # ------------------- two-samples tests -----------------------------------
#'
#' # for reproducibility
#' set.seed(123)
#' library(statsExpressions)
#'
#' # to get caption (default)
#' bf_ttest(
#'   data = mtcars,
#'   x = am,
#'   y = wt,
#'   paired = FALSE,
#'   bf.prior = 0.880
#' )
#'
#' # to see results
#' bf_ttest(
#'   data = mtcars,
#'   x = am,
#'   y = wt,
#'   paired = FALSE,
#'   output = "results"
#' )
#'
#' # for paired sample test
#' bf_ttest(
#'   data = dplyr::filter(
#'     statsExpressions::intent_morality,
#'     condition %in% c("accidental", "attempted"),
#'     harm == "Poisoning"
#'   ),
#'   x = condition,
#'   y = rating,
#'   paired = TRUE,
#'   output = "results"
#' )
#'
#' # ------------------- one-samples test -----------------------------------
#'
#' # to get caption (default)
#' bf_ttest(
#'   data = iris,
#'   x = Sepal.Length,
#'   test.value = 5.85,
#'   bf.prior = 0.8,
#'   output = "caption", k = 2
#' )
#'
#' # to get results dataframe
#' bf_ttest(
#'   data = iris,
#'   x = Sepal.Length,
#'   test.value = 5.85,
#'   bf.prior = 0.8,
#'   output = "results"
#' )
#' @export

# function body
bf_ttest <- function(data,
                     x,
                     y = NULL,
                     test.value = 0,
                     paired = FALSE,
                     bf.prior = 0.707,
                     caption = NULL,
                     output = "null",
                     k = 2,
                     ...) {

  # make sure both quoted and unquoted arguments are allowed
  x <- rlang::ensym(x)
  y <- if (!rlang::quo_is_null(rlang::enquo(y))) rlang::ensym(y)

  # -------------------------- two-sample tests ------------------------------

  if (!rlang::quo_is_null(rlang::enquo(y))) {
    # dropping unused factor levels from `x` variable
    data %<>% dplyr::mutate(.data = ., {{ x }} := droplevels(as.factor({{ x }})))

    # within-subjects design
    if (isTRUE(paired)) {
      # the data needs to be in wide format
      data_wide <- long_to_wide_converter(data = data, x = {{ x }}, y = {{ y }})

      # change names for convenience
      colnames(data_wide) <- c("rowid", "col1", "col2")

      # extracting results from Bayesian test and creating a dataframe
      bf_object <-
        BayesFactor::ttestBF(
          x = data_wide$col1,
          y = data_wide$col2,
          rscale = bf.prior,
          paired = TRUE,
          progress = FALSE,
          ...
        )
    }

    # between-subjects design
    if (isFALSE(paired)) {
      # removing NAs
      data %<>% dplyr::filter(.data = ., !is.na({{ x }}), !is.na({{ y }}))

      # extracting results from Bayesian test and creating a dataframe
      bf_object <-
        BayesFactor::ttestBF(
          formula = rlang::new_formula({{ y }}, {{ x }}),
          data = as.data.frame(data),
          rscale = bf.prior,
          paired = FALSE,
          progress = FALSE,
          ...
        )
    }
  }

  # -------------------------- one-sample tests ------------------------------

  if (rlang::quo_is_null(rlang::enquo(y))) {
    bf_object <-
      BayesFactor::ttestBF(
        x = data %>% dplyr::pull({{ x }}),
        rscale = bf.prior,
        mu = test.value,
        nullInterval = NULL,
        ...
      )
  }

  # extracting the Bayes factors
  bf.df <-
    bf_extractor(bf.object = bf_object) %>%
    dplyr::mutate(.data = ., bf.prior = bf.prior)

  # ============================ return ==================================

  # prepare the Bayes factor message
  if (output != "results") {
    bf_message <-
      bf_expr(
        bf.df = bf.df,
        output = output,
        k = k,
        caption = caption
      )
  }

  # return the text results or the dataframe with results
  return(switch(
    EXPR = output,
    "results" = bf.df,
    bf_message
  ))
}

#' @rdname bf_ttest
#' @aliases bf_ttest
#' @export

bf_one_sample_ttest <- bf_ttest

#' @rdname bf_ttest
#' @aliases bf_ttest
#' @export

bf_two_sample_ttest <- bf_ttest

#' @title Bayesian one-way analysis of variance.
#' @name bf_oneway_anova
#' @author \href{https://github.com/IndrajeetPatil}{Indrajeet Patil}
#'
#' @importFrom BayesFactor anovaBF
#'
#' @inheritParams expr_anova_parametric
#' @inheritParams bf_ttest
#' @param ... Additional arguments.
#'
#' @seealso \code{\link{bf_contingency_tab}}, \code{\link{bf_corr_test}},
#' \code{\link{bf_ttest}}
#'
#' @examples
#' \donttest{
#'
#' # between-subjects -------------------------------------------------------
#'
#' # to get caption (default)
#' bf_oneway_anova(
#'   data = iris,
#'   x = Species,
#'   y = Sepal.Length,
#'   bf.prior = 0.8,
#'   paired = FALSE
#' )
#'
#' # to get results dataframe
#' bf_oneway_anova(
#'   data = iris,
#'   x = Species,
#'   y = Sepal.Length,
#'   bf.prior = 0.8,
#'   output = "results"
#' )
#'
#' # within-subjects -------------------------------------------------------
#' bf_oneway_anova(
#'   data = bugs_long,
#'   x = condition,
#'   y = desire,
#'   paired = TRUE
#' )
#' }
#' @export

# function body
bf_oneway_anova <- function(data,
                            x,
                            y,
                            bf.prior = 0.707,
                            caption = NULL,
                            output = "null",
                            paired = FALSE,
                            k = 2,
                            ...) {

  # make sure both quoted and unquoted arguments are allowed
  c(x, y) %<-% c(rlang::ensym(x), rlang::ensym(y))

  # ============================ data preparation ==========================

  # creating a dataframe
  data %<>%
    dplyr::select(.data = ., {{ x }}, {{ y }}) %>%
    dplyr::mutate(.data = ., {{ x }} := droplevels(as.factor({{ x }}))) %>%
    tibble::as_tibble(.)

  # ========================= within-subjects design ==========================

  if (isTRUE(paired)) {
    # converting to long format and then getting it back in wide so that the
    # rowid variable can be used as the block variable
    data %<>%
      df_cleanup_paired(data = ., x = {{ x }}, y = {{ y }}) %>%
      dplyr::mutate(.data = ., rowid = as.factor(rowid))

    # extracting results from Bayesian test (`y ~ x + id`) and creating a dataframe
    bf.df <-
      bf_extractor(BayesFactor::anovaBF(
        formula = rlang::new_formula(
          {{ rlang::enexpr(y) }}, rlang::expr(!!rlang::enexpr(x) + rowid)
        ),
        data = as.data.frame(data),
        whichRandom = "rowid",
        rscaleFixed = bf.prior,
        progress = FALSE,
        rscaleRandom = 1,
        ...
      )) %>%
      dplyr::mutate(.data = ., bf.prior = bf.prior)
  }

  # ========================= between-subjects design =========================

  if (isFALSE(paired)) {
    # remove NAs listwise for between-subjects design
    data %<>% tidyr::drop_na(.)

    # extracting results from Bayesian test and creating a dataframe
    bf.df <-
      bf_extractor(
        BayesFactor::anovaBF(
          formula = rlang::new_formula({{ y }}, {{ x }}),
          data = as.data.frame(data),
          rscaleFixed = bf.prior,
          progress = FALSE,
          ...
        )
      ) %>%
      dplyr::mutate(.data = ., bf.prior = bf.prior)
  }

  # ============================ return ==================================

  # prepare the Bayes Factor message
  if (output != "results") {
    bf_message <-
      bf_expr(
        bf.df = bf.df,
        output = output,
        k = k,
        caption = caption
      )
  }

  # return the text results or the dataframe with results
  return(switch(
    EXPR = output,
    "results" = bf.df,
    bf_message
  ))
}


#' @title Bayes factor message for random-effects meta-analysis
#' @name bf_meta
#'
#' @importFrom metaBMA meta_random prior
#'
#' @inherit metaBMA::meta_random return Description
#'
#' @inheritParams expr_meta_parametric
#' @inheritParams bf_expr
#' @inheritParams metaBMA::meta_random
#' @inheritDotParams metaBMA::meta_random -y -SE
#'
#' @examples
#'
#' \donttest{
#' # setup
#' set.seed(123)
#' library(metaBMA)
#'
#' # creating a dataframe
#' (df <-
#'   structure(
#'     .Data = list(
#'       study = c("1", "2", "3", "4", "5"),
#'       estimate = c(
#'         0.382047603321706,
#'         0.780783111514665,
#'         0.425607573765058,
#'         0.558365541235078,
#'         0.956473848429961
#'       ),
#'       std.error = c(
#'         0.0465576338644502,
#'         0.0330218199731529,
#'         0.0362834986178494,
#'         0.0480571500648261,
#'         0.062215818388157
#'       )
#'     ),
#'     row.names = c(NA, -5L),
#'     class = c("tbl_df", "tbl", "data.frame")
#'   ))
#'
#' # getting Bayes factor in favor of null hypothesis
#' bf_meta(
#'   data = df,
#'   k = 3,
#'   iter = 1500,
#'   messages = TRUE,
#'   # customizing analysis with additional arguments
#'   control = list(max_treedepth = 15)
#' )
#' }
#'
#' @export

# function body
bf_meta <- function(data,
                    d = prior("norm", c(mean = 0, sd = 0.3)),
                    tau = prior("invgamma", c(shape = 1, scale = 0.15)),
                    k = 2,
                    output = "null",
                    caption = NULL,
                    messages = TRUE,
                    ...) {

  # check the data contains needed column
  meta_data_check(data)

  #----------------------- meta-analysis -------------------------------

  # extracting results from random-effects meta-analysis
  meta_res <-
    metaBMA::meta_random(
      y = data$estimate,
      SE = data$std.error,
      d = d,
      tau = tau,
      ...
    )

  # print results from meta-analysis
  if (isTRUE(messages)) print(meta_res)

  #----------------------- preparing caption -------------------------------

  # creating a dataframe with posterior estimates
  df_estimates <-
    tibble::as_tibble(meta_res$estimates, rownames = "term") %>%
    dplyr::filter(.data = ., term == "d")

  # dataframe with bayes factors
  bf.df <-
    dplyr::tibble(bf10 = meta_res$BF["random_H1", "random_H0"]) %>%
    bf_formatter(.)

  # changing aspects of the caption based on what output is needed
  if (output %in% c("null", "caption", "H0", "h0")) {
    hypothesis.text <- "In favor of null: "
    bf.value <- bf.df$log_e_bf01[[1]]
    bf.subscript <- "01"
  } else {
    hypothesis.text <- "In favor of alternative: "
    bf.value <- bf.df$log_e_bf10[[1]]
    bf.subscript <- "10"
  }

  # prepare the Bayes factor message
  bf_text <-
    substitute(
      atop(displaystyle(top.text),
        expr = paste(
          hypothesis.text,
          "log"["e"],
          "(BF"[bf.subscript],
          ") = ",
          bf,
          ", ",
          italic("d")["mean"]^"posterior",
          " = ",
          d.pmean,
          ", CI"["95%"],
          " [",
          d.pmean.LB,
          ", ",
          d.pmean.UB,
          "]"
        )
      ),
      env = list(
        hypothesis.text = hypothesis.text,
        top.text = caption,
        bf.subscript = bf.subscript,
        bf = specify_decimal_p(x = bf.value, k = k),
        d.pmean = specify_decimal_p(x = df_estimates$mean[[1]], k = k),
        d.pmean.LB = specify_decimal_p(x = df_estimates$hpd95_lower[[1]], k = k),
        d.pmean.UB = specify_decimal_p(x = df_estimates$hpd95_upper[[1]], k = k)
      )
    )

  # return the caption
  return(bf_text)
}

#' @noRd
#' @keywords internal

bf_formatter <- function(data) {
  dplyr::mutate(
    .data = data,
    bf01 = 1 / bf10,
    log_e_bf10 = log(bf10),
    log_e_bf01 = -1 * log_e_bf10,
    log_10_bf10 = log10(bf10),
    log_10_bf01 = -1 * log_10_bf10
  )
}
