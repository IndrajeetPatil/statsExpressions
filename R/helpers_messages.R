#' @title Message about bootstrapped confidence intervals for effect sizes.
#' @name effsize_ci_message
#' @author \href{https://github.com/IndrajeetPatil}{Indrajeet Patil}
#'
#' @inheritParams t1way_ci
#'
#' @family helper_messages
#'
#' @keywords internal

# displaying message about bootstrap
effsize_ci_message <- function(nboot = 100, conf.level = 0.95) {
  message(cat(
    crayon::green("Note:"),
    crayon::yellow(paste(conf.level * 100, "%", sep = "")),
    crayon::blue("CI for effect size estimate was computed with"),
    crayon::yellow(nboot),
    crayon::blue("bootstrap samples.\n")
  ),
  sep = ""
  )
}


#' @title Switch function to determine which effect size is to computed.
#' @name effsize_type_switch
#' @description Takes in all allowed characters describing the needed effect
#'   size (e.g., `"d"`, `"partial_eta"`, etc.) and converts it into standard
#'   terms (`"biased"` or `"unbiased"`) to reduce the complexity of conditional
#'   statements.
#' @author \href{https://github.com/IndrajeetPatil}{Indrajeet Patil}
#'
#' @param effsize.type Character describing the needed effect size.
#'
#' @keywords internal

effsize_type_switch <- function(effsize.type = NULL) {
  # figuring out which effect size to use
  if (!is.null(effsize.type)) {
      switch(
        EXPR = effsize.type,
        d = "biased",
        g = "unbiased",
        eta = "biased",
        omega = "unbiased",
        partial_eta = "biased",
        partial_omega = "unbiased",
        partial.eta = "biased",
        partial.omega = "unbiased",
        p_eta = "biased",
        p_omega = "unbiased",
        biased = "biased",
        unbiased = "unbiased",
        "unbiased"
      )
  } else {
    "unbiased"
  }
}


#' @title Switch function to determine which type of statistics is to be run.
#' @name stats_type_switch
#' @description Takes in all allowed characters describing the needed type of
#'   test and converts it into standard terms to reduce the complexity of
#'   conditional statements.
#' @author \href{https://github.com/IndrajeetPatil}{Indrajeet Patil}
#'
#' @param stats.type Character describing the needed type of statistics (e.g.,
#'   `"parametric"`, `"nonparametric"`, `"robust"`, `"bayes"``, etc.).
#'
#' @keywords internal

stats_type_switch <- function(stats.type) {
  # figuring out which effect size to use
  if (!is.null(stats.type)) {
      switch(
        EXPR = stats.type,
        parametric = "parametric",
        p = "parametric",
        pearson = "parametric",
        nonparametric = "nonparametric",
        np = "nonparametric",
        "non-parametric" = "nonparametric",
        spearman = "nonparametric",
        robust = "robust",
        r = "robust",
        pb = "robust",
        bayes = "bayes",
        bf = "bayes",
        "parametric"
      )
  } else {
    "parametric"
  }
}
