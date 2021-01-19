#' @title Expression template for Bayes Factor results
#' @name bf_expr_template
#'
#' @param prior.type A character that specifies the prior type.
#' @param estimate.type A character that specifies the relevant effect size.
#' @param estimate.df Dataframe containing estimates and their credible
#'   intervals along with Bayes Factor value. The columns should be named as
#'   `estimate`, `estimate.LB`, `estimate.UB`, and `bf10`.
#' @param ... Currently ignored.
#' @inheritParams bf_extractor
#' @inheritParams expr_t_twosample
#'
#' @importFrom ipmisc format_num
#'
#' @export

bf_expr_template <- function(top.text,
                             estimate.df,
                             prior.type = NULL,
                             estimate.type = NULL,
                             centrality = "median",
                             conf.level = 0.95,
                             conf.method = "HDI",
                             k = 2L,
                             ...) {
  # extracting estimate values
  if ("r2" %in% names(estimate.df)) {
    # for ANOVA designs
    c(estimate, estimate.LB, estimate.UB) %<-%
      c(estimate.df$r2[[1]], estimate.df$r2.conf.low[[1]], estimate.df$r2.conf.high[[1]])
  } else {
    # for non-ANOVA designs
    c(estimate, estimate.LB, estimate.UB) %<-%
      c(estimate.df$estimate[[1]], estimate.df$conf.low[[1]], estimate.df$conf.high[[1]])
  }

  # if expression elements are `NULL`
  if (is.null(prior.type)) prior.type <- prior_type_switch(estimate.df$method[[1]])
  if (is.null(estimate.type)) estimate.type <- estimate_type_switch(estimate.df$method[[1]])

  # prepare the Bayes Factor message
  bf01_expr <-
    substitute(
      atop(
        displaystyle(top.text),
        expr = paste(
          "log"["e"] * "(BF"["01"] * ") = " * bf * ", ",
          widehat(estimate.type)[centrality]^"posterior" * " = " * estimate * ", ",
          "CI"[conf.level]^conf.method * " [" * estimate.LB * ", " * estimate.UB * "], ",
          prior.type * " = " * bf.prior
        )
      ),
      env = list(
        top.text = top.text,
        estimate.type = estimate.type,
        centrality = centrality,
        conf.level = paste0(conf.level * 100, "%"),
        conf.method = toupper(conf.method),
        bf = format_num(-log(estimate.df$bf10[[1]]), k = k),
        estimate = format_num(estimate, k = k),
        estimate.LB = format_num(estimate.LB, k = k),
        estimate.UB = format_num(estimate.UB, k = k),
        prior.type = prior.type,
        bf.prior = format_num(estimate.df$prior.scale[[1]], k = k)
      )
    )

  # return the final expression
  if (is.null(top.text)) bf01_expr$expr else bf01_expr
}


#' @noRd

prior_type_switch <- function(method) {
  switch(
    method,
    "Bayesian contingency tabs analysis" = quote(italic("a")["Gunel-Dickey"]),
    quote(italic("r")["Cauchy"]^"JZS")
  )
}


#' @noRd

estimate_type_switch <- function(method) {
  switch(
    method,
    "Bayesian contingency tabs analysis" = quote(italic("V")),
    "Bayesian correlation analysis" = quote(italic(rho)),
    "Bayesian meta-analysis using 'metaBMA'" = ,
    "Bayesian t-test" = quote(italic(delta)),
    "Bayes factors for linear models" = quote(italic(R^"2"))
  )
}
