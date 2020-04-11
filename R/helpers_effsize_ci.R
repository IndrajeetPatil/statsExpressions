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
#' @param ... Currently ignored.
#'
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
    as_tibble(x = .)

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
#' @param ... Currently ignored.
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
    as_tibble(.)

  # jamovi needs data to be wide format and not long format
  data_wide <- long_to_wide_converter(data = data, x = x, y = y)

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
    as_tibble(x)
  }

  # combining
  results_df <-
    dplyr::bind_cols(wrs_cleaning(fit), as_tibble(as.data.frame(ci))) %>%
    dplyr::rename_all(
      .tbl = .,
      .funs = dplyr::recode,
      effsize = "estimate",
      test = "statistic",
      se = "std.error",
      df = "parameter",
      df1 = "parameter1",
      df2 = "parameter2"
    )

  # selecting the columns corresponding to the confidence intervals
  if (conf.type == "norm") {
    return(results_df %>% dplyr::rename(.data = ., conf.low = V2, conf.high = V3))
  } else {
    return(results_df %>% dplyr::rename(.data = ., conf.low = V4, conf.high = V5))
  }
}
