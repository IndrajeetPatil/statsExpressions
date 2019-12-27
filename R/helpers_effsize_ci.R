#' @title Calculating Cohen's *d* or Hedge's *g* (for between-/within- or one
#'   sample designs).
#' @name effsize_t_parametric
#' @author Chuck Powell
#'
#' @param formula This function only accepts the variables in `formula` format
#'   e.g. `sleep_rem ~ vore` (two sample) or `~ vore` (one sample).
#' @param mu If conducting a single sample test against a mean (Default: `0`).
#' @param hedges.correction Logical indicating whether to apply Hedges
#'   correction, Hedge's *g* (Default: `TRUE`).
#' @param noncentral Logical indicating whether to use non-central
#'   *t*-distributions for computing the confidence intervals (Default: `TRUE`).
#' @param tobject Object with the *t*-test specification.
#' @inheritParams expr_t_parametric
#'
#' @importFrom rlang is_formula
#' @importFrom stats t.test cor qt pt uniroot
#' @importFrom tibble tibble
#'
#' @details
#' References-
#' \itemize{
#' \item Cooper, Harris, Hedges, Larry V., Valentine, Jeffrey C., The Handbook
#' of Research Synthesis and Meta-Analysis, 2009. \item Cumming, G., Finch, S.,
#' A Primer On The Understanding, Use, And Calculation Of Confidence Intervals
#' That Are Based On Central And Noncentral Distributions, Educational and
#' Psychological Measurement, Vol. 61 No. 4, August 2001 532-574. \item Cohen,
#' J. (1988). Statistical power analysis for the behavioral sciences (2nd ed.)
#' Hillsdale, NJ: Lawrence Erlbaum Associates. \item David C. Howell (2010).
#' Confidence Intervals on Effect Size
#' }
#'
#' @examples
#'
#' \donttest{
#' # for reproducibility
#' set.seed(123)
#'
#' #---------------- two-sample test ------------------------------------
#'
#' # creating a smaller dataset
#' msleep_short <- dplyr::filter(
#'   .data = ggplot2::msleep,
#'   vore %in% c("carni", "herbi")
#' )
#'
#' # with defaults
#' tobj1 <- t.test(
#'   formula = sleep_rem ~ vore,
#'   data = msleep_short
#' )
#' statsExpressions:::effsize_t_parametric(
#'   formula = sleep_rem ~ vore,
#'   data = msleep_short,
#'   tobject = tobj1
#' )
#'
#' # changing defaults
#' tobj2 <- t.test(
#'   formula = sleep_rem ~ vore,
#'   data = msleep_short,
#'   mu = 1,
#'   paired = FALSE,
#'   conf.level = .99
#' )
#' statsExpressions:::effsize_t_parametric(
#'   formula = sleep_rem ~ vore,
#'   data = msleep_short,
#'   mu = 1, # ignored in this case
#'   paired = FALSE,
#'   hedges.correction = TRUE,
#'   conf.level = .99,
#'   noncentral = FALSE,
#'   tobject = tobj2
#' )
#'
#' #---------------- one-sample test ------------------------------------
#'
#' tobj3 <- t.test(
#'   x = msleep_short$sleep_rem,
#'   mu = 2,
#'   conf.level = .90
#' )
#' statsExpressions:::effsize_t_parametric(
#'   formula = ~sleep_rem,
#'   data = msleep_short,
#'   mu = 2,
#'   hedges.correction = TRUE,
#'   conf.level = .90,
#'   noncentral = TRUE,
#'   tobject = tobj3
#' )
#' }
#'
#' @keywords internal

# function body
effsize_t_parametric <- function(formula = NULL,
                                 data = NULL,
                                 mu = 0,
                                 paired = FALSE,
                                 hedges.correction = TRUE,
                                 conf.level = .95,
                                 var.equal = FALSE,
                                 noncentral = TRUE,
                                 tobject = NULL,
                                 ...) {

  # -------------- input checking -------------------

  if (!rlang::is_formula(formula) | !is.data.frame(data)) {
    stop("arguments must include a formula and a data frame")
  }
  if (length(formula) == 2 & length(all.vars(formula)) > 1) {
    stop("Your formula has too many items on the rhs")
  }
  if (length(formula) == 3 & length(all.vars(formula)) > 2) {
    stop("Your formula has too many variables")
  }
  if (is.null(tobject)) {
    stop("This is an internal function and requires a tobject as
         part of its call")
  }

  # -------------- single sample compare to mu -------------------

  if (length(formula) == 2 & length(all.vars(formula)) == 1) {
    method <- "Cohen's d"
    x <- eval(formula[[2]], data)
    x <- x[!is.na(x)]
    n <- length(x)
    sd.est <- sd(x)
    df <- length(x) - 1
    mean.diff <- mean(x) - mu
    d <- mean.diff / sd.est
    Sigmad <- sqrt((n / (n / 2)^2) + (d^2 / (2 * n)))
    Z <- -stats::qt((1 - conf.level) / 2, df)
    tvalue <- tobject$statistic
    dfvalue <- tobject$parameter
    civalue <- conf.level
    twosamples <- FALSE
    paired <- NA_character_
  }

  # ---------------two independent samples by factor -------------------

  # two samples by factor
  if (length(formula) == 3 & isFALSE(paired)) {
    # getting `x` and `y` in required format
    outcome <- eval(formula[[2]], data)
    group <- eval(formula[[3]], data)
    group <- factor(group)

    # test relevant variables
    x <- split(outcome, group)
    y <- x[[2]]
    x <- x[[1]]
    x <- x[!is.na(x)]
    y <- y[!is.na(y)]
    sq.devs <- (c(x - mean(x), y - mean(y)))^2
    n <- length(sq.devs)
    n1 <- length(x)
    n2 <- length(y)
    if (isTRUE(var.equal)) {
      sd.est <- sqrt(sum(sq.devs) / (n - 2))
    } else {
      sd.est <- sqrt((var(x) + var(y)) / 2)
    }
    mean.diff <- mean(x) - mean(y)
    df <- tobject$parameter
    d <- mean.diff / sd.est
    Sigmad <- sqrt((n1 + n2) / (n1 * n2) + 0.5 * d^2 / (n1 + n2))
    Z <- -stats::qt((1 - conf.level) / 2, df)
    method <- "Cohen's d"
    tvalue <- tobject$statistic
    dfvalue <- tobject$parameter
    civalue <- conf.level
    twosamples <- TRUE
  }

  # -------------- two paired samples in matching columns -------------------

  # if the data is in tidy format
  if (length(formula) == 3 & isTRUE(paired)) {
    if (is.factor(eval(formula[[3]], data)) || is.character(eval(formula[[3]], data))) {
      # getting `x` and `y` in required format
      outcome <- eval(formula[[2]], data)
      group <- eval(formula[[3]], data)
      group <- droplevels(as.factor(group))
      x <- split(outcome, group)
      y <- x[[2]]
      x <- x[[1]]
    } else {
      x <- eval(formula[[2]], data)
      y <- eval(formula[[3]], data)
      ind <- !is.na(x) & !is.na(y)
      x <- x[ind]
      y <- y[ind]
    }

    # test relevant variables
    n <- length(x)
    df <- n - 1
    r <- cor(x, y)
    sd.est <- sd(x - y)
    mean.diff <- mean(x) - mean(y)
    d <- mean.diff / sd.est
    Sigmad <- sqrt(((1 / n) + (d^2 / n)) * 2 * (1 - r)) # paired
    Z <- -qt((1 - conf.level) / 2, df)
    method <- "Cohen's d"
    tvalue <- tobject$statistic
    dfvalue <- tobject$parameter
    civalue <- conf.level
    twosamples <- FALSE
  }

  # -------------- apply hedges correction -------------------

  if (hedges.correction == TRUE) {
    method <- "Hedges's g"
    d <- d * (n - 3) / (n - 2.25)
  }

  lower.ci <- c(d - Z * Sigmad)
  upper.ci <- c(d + Z * Sigmad)

  # -------------- calculate NCP intervals -------------------

  if (isTRUE(noncentral)) {
    st <- max(0.1, tvalue)
    end1 <- tvalue
    while (stats::pt(q = tvalue, df = dfvalue, ncp = end1) > (1 - civalue) / 2) {
      end1 <- end1 + st
    }
    ncp1 <- uniroot(
      function(x) {
        (1 - civalue) / 2 - stats::pt(
          q = tvalue,
          df = dfvalue,
          ncp = x
        )
      },
      c(2 * tvalue - end1, end1)
    )$root
    end2 <- tvalue
    while (stats::pt(q = tvalue, df = dfvalue, ncp = end2) < (1 + civalue) / 2) {
      end2 <- end2 - st
    }
    ncp2 <- uniroot(
      function(x) {
        (1 + civalue) / 2 - stats::pt(
          q = tvalue,
          df = dfvalue,
          ncp = x
        )
      },
      c(end2, 2 * tvalue - end2)
    )$root

    if (isTRUE(twosamples)) {
      ncp.upper.ci <- ncp1 * sqrt(1 / n1 + 1 / n2)
      ncp.lower.ci <- ncp2 * sqrt(1 / n1 + 1 / n2)
    } else {
      ncp.upper.ci <- ncp1 / sqrt(dfvalue)
      ncp.lower.ci <- ncp2 / sqrt(dfvalue)
    }
  }

  # -------------- return results desired ------------------

  if (isTRUE(noncentral)) {
    lower.ci <- min(ncp.lower.ci, ncp.upper.ci)
    upper.ci <- max(ncp.lower.ci, ncp.upper.ci)
  }

  # return the final dataframe with results
  return(tibble::tibble(
    method = method,
    estimate = d,
    conf.low = lower.ci,
    conf.high = upper.ci,
    conf.level = conf.level,
    alternative = "two.sided",
    paired = paired,
    noncentral = noncentral,
    var.equal = var.equal
  ))
}


#' @title A heteroscedastic one-way ANOVA for trimmed means with confidence
#'   interval for effect size.
#' @name t1way_ci
#' @description Custom function to get confidence intervals for effect size
#'   measure for robust ANOVA.
#'
#' @param data A dataframe (or a tibble) from which variables specified are to
#'   be taken. A matrix or tables will **not** be accepted.
#' @param x The grouping variable from the dataframe `data`.
#' @param y The response (a.k.a. outcome or dependent) variable from the
#'   dataframe `data`.
#' @param nboot Number of bootstrap samples for computing confidence interval
#'   for the effect size (Default: `100`).
#' @param tr Trim level for the mean when carrying out `robust` tests. If you
#'   get error stating "Standard error cannot be computed because of Winsorized
#'   variance of 0 (e.g., due to ties). Try to decrease the trimming level.",
#'   try to play around with the value of `tr`, which is by default set to
#'   `0.1`. Lowering the value might help.
#' @param conf.type A vector of character strings representing the type of
#'   intervals required. The value should be any subset of the values `"norm"`,
#'   `"basic"`, `"perc"`, `"bca"`. For more, see `?boot::boot.ci`.
#' @param conf.level Scalar between 0 and 1. If unspecified, the defaults return
#'   `95%` lower and upper confidence intervals (`0.95`).
#' @inheritDotParams boot::boot
#'
#' @importFrom tibble as_tibble
#' @importFrom dplyr select contains
#' @importFrom boot boot
#' @importFrom rlang !! enquo
#' @importFrom WRS2 t1way
#'
#' @examples
#' # for reproducibility
#' set.seed(123)
#' \donttest{
#' statsExpressions:::t1way_ci(
#'   data = morley,
#'   x = Expt,
#'   y = Speed,
#'   nboot = 10
#' )
#' }
#' @keywords internal

t1way_ci <- function(data,
                     x,
                     y,
                     tr = 0.1,
                     nboot = 100,
                     conf.level = 0.95,
                     conf.type = "norm",
                     ...) {
  # creating a dataframe from entered data
  data %<>%
    dplyr::select(.data = ., x = {{ x }}, y = {{ y }}) %>%
    dplyr::filter(.data = ., !is.na(x), !is.na(y)) %>%
    dplyr::mutate(.data = ., x = droplevels(as.factor(x))) %>%
    tibble::as_tibble(x = .)

  # running robust one-way anova
  fit <-
    WRS2::t1way(
      formula = stats::as.formula(y ~ x),
      data = data,
      tr = tr
    )

  # function to obtain 95% CI for xi
  xici <- function(formula, data, tr, indices) {
    # allows boot to select sample
    d <- data[indices, ]

    # running the function
    fit <-
      WRS2::t1way(
        formula = stats::as.formula(formula),
        data = d,
        tr = tr
      )

    # return the value of interest: effect size
    return(fit$effsize)
  }

  # save the bootstrapped results to an object
  bootobj <-
    boot::boot(
      data = data,
      statistic = xici,
      R = nboot,
      formula = y ~ x,
      tr = tr,
      parallel = "multicore",
      ...
    )

  # extracting all details with the custom function
  return(extract_boot_output(bootobj, fit, conf.type, conf.level))
}

#' @title Paired samples robust *t*-tests with confidence
#'   interval for effect size.
#' @name yuend_ci
#' @description Custom function to get confidence intervals for effect size
#'   measure for paired samples robust t-tests.
#'
#' @inheritParams t1way_ci
#' @inheritDotParams boot::boot
#'
#' @importFrom WRS2 yuend
#'
#' @examples
#'
#' # for reproducibility
#' set.seed(123)
#' \donttest{
#' statsExpressions:::yuend_ci(
#'   data = dplyr::filter(
#'     .data = statsExpressions::iris_long,
#'     condition %in% c("Sepal.Length", "Petal.Length")
#'   ),
#'   x = condition,
#'   y = value,
#'   nboot = 50,
#'   tr = 0.2
#' )
#' }
#' @keywords internal

# function body
yuend_ci <- function(data,
                     x,
                     y,
                     tr = 0.1,
                     nboot = 100,
                     conf.level = 0.95,
                     conf.type = "norm",
                     ...) {
  # creating a dataframe from entered data
  data %<>%
    dplyr::select(.data = ., x = {{ x }}, y = {{ y }}) %>%
    dplyr::filter(.data = ., !is.na(x), !is.na(y)) %>%
    dplyr::mutate(.data = ., x = droplevels(as.factor(x))) %>%
    tibble::as_tibble(x = .)

  # jamovi needs data to be wide format and not long format
  data_wide <- long_to_wide_converter(data = data, x = x, y = y)

  # sample size
  sample_size <- nrow(data_wide)

  # running robust one-way anova
  fit <- WRS2::yuend(x = data_wide[2], y = data_wide[3], tr = tr)

  # function to obtain 95% CI for xi
  xici <- function(data, tr, indices) {
    # allows boot to select sample
    d <- data[indices, ]

    # running the function
    fit <- WRS2::yuend(x = d[2], y = d[3], tr = tr)

    # return the value of interest: effect size
    return(fit$effsize)
  }

  # save the bootstrapped results to an object
  bootobj <-
    boot::boot(
      statistic = xici,
      R = nboot,
      data = data_wide,
      tr = tr,
      parallel = "multicore",
      ...
    )

  # extracting all details with the custom function
  return(extract_boot_output(bootobj, fit, conf.type, conf.level))
}

#' @title Robust correlation coefficient and its confidence interval
#' @name robcor_ci
#' @description Custom function to get confidence intervals for percentage bend
#'   correlation coefficient.
#' @return A tibble with percentage bend correlation coefficient, along with its
#'   confidence intervals, and the number of bootstrap samples used to generate
#'   confidence intervals. Additionally, it also includes information about
#'   sample size, bending constant, no. of bootstrap samples, etc.
#'
#' @param x A vector containing the explanatory variable.
#' @param y The response - a vector of length the number of rows of `x`.
#' @param beta bending constant (Default: `0.1`). For more, see `?WRS2::pbcor`.
#' @inheritParams t1way_ci
#' @inheritDotParams boot::boot
#'
#' @importFrom WRS2 pbcor
#'
#' @examples
#'
#' # for reproducibility
#' set.seed(123)
#' \donttest{
#' statsExpressions:::robcor_ci(
#'   data = mtcars,
#'   x = "hp",
#'   y = "mpg",
#'   beta = .01,
#'   nboot = 25,
#'   conf.level = .99,
#'   conf.type = c("basic")
#' )
#' }
#' @keywords internal

# function body
robcor_ci <- function(data,
                      x,
                      y,
                      beta = 0.1,
                      nboot = 100,
                      conf.level = 0.95,
                      conf.type = "norm",
                      ...) {
  # creating a dataframe from entered data
  data %<>%
    dplyr::select(.data = ., x = {{ x }}, y = {{ y }}) %>%
    dplyr::filter(.data = ., !is.na(x), !is.na(y)) %>%
    tibble::as_tibble(x = .)

  # getting the p.value for the correlation coefficient
  fit <- WRS2::pbcor(x = data$x, y = data$y, beta = beta)

  # function to obtain 95% CI for xi
  robcor_ci <- function(data, x, y, beta = beta, indices) {
    # allows boot to select sample
    d <- data[indices, ]

    # correlation object
    fit <- WRS2::pbcor(x = d$x, y = d$y, beta = beta)

    # return the value of interest: correlation coefficient
    return(fit$cor)
  }

  # save the bootstrapped results to an object
  bootobj <-
    boot::boot(
      data = data,
      statistic = robcor_ci,
      R = nboot,
      x = x,
      y = y,
      beta = beta,
      parallel = "multicore",
      ...
    )

  # extracting all details with the custom function
  return(extract_boot_output(bootobj, fit, conf.type, conf.level))
}


#' @name extract_boot_output
#'
#' @importFrom dplyr select contains rename rename_all recode bind_cols
#' @importFrom purrr pluck
#' @importFrom boot boot boot.ci
#'
#' @noRd

# extract CI
extract_boot_output <- function(bootobj, fit, conf.type, conf.level) {
  # get 95% CI from the bootstrapped object
  bootci <- boot::boot.ci(boot.out = bootobj, conf = conf.level, type = conf.type)

  # extracting ci part
  ci <- purrr::pluck(bootci, dplyr::contains(conf.type, TRUE, names(bootci)))

  # function to clean WRS object
  wrs_cleaning <- function(wrs_obj) {
    x <- unlist(wrs_obj)
    x[length(x)] <- NULL
    tibble::as_tibble(x)
  }

  # combining
  results_df <-
    dplyr::bind_cols(wrs_cleaning(fit), tibble::as_tibble(as.data.frame(ci))) %>%
    dplyr::rename_all(
      .tbl = .,
      .funs = dplyr::recode,
      effsize = "estimate",
      cor = "estimate",
      test = "statistic",
      se = "std.error"
    )

  # selecting the columns corresponding to the confidence intervals
  if (conf.type == "norm") {
    return(results_df %>% dplyr::rename(.data = ., conf.low = V2, conf.high = V3))
  } else {
    return(results_df %>% dplyr::rename(.data = ., conf.low = V4, conf.high = V5))
  }
}
