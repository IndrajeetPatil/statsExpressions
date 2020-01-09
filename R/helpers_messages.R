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


#' @noRd

effsize_type_switch <- function(effsize.type) {
  dplyr::case_when(
    grepl("^u|^g|omega", effsize.type, TRUE) ~ "unbiased",
    grepl("^b|^d|eta", effsize.type, TRUE) ~ "biased",
    TRUE ~ "unbiased"
  )
}


#' @noRd

stats_type_switch <- function(type) {
  dplyr::case_when(
    grepl("^p", type, TRUE) ~ "parametric",
    grepl("^n|^s", type, TRUE) ~ "nonparametric",
    grepl("^r", type, TRUE) ~ "robust",
    grepl("^b", type, TRUE) ~ "bayes",
    TRUE ~ "parametric"
  )
}
