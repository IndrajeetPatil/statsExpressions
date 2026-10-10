#' @title Multiple pairwise comparison for one-way design
#' @name pairwise_comparisons
#'
#' @description
#'
#' Calculate parametric, non-parametric, robust, and Bayes Factor pairwise
#' comparisons between group levels with corrections for multiple testing.
#'
#' @inheritParams long_to_wide_converter
#' @inheritParams extract_stats_type
#' @inheritParams oneway_anova
#' @inheritParams two_sample_test
#' @param p.adjust.method Adjustment method for *p*-values for multiple
#'   comparisons. Possible methods are: `"holm"` (default), `"hochberg"`,
#'   `"hommel"`, `"bonferroni"`, `"BH"`, `"BY"`, `"fdr"`, `"none"`.
#' @param ... Additional arguments passed to the underlying pairwise test
#'   function for the parametric and non-parametric tests (see the table
#'   below). Ignored for robust tests. For Bayesian tests, they are passed to
#'   the frequentist test (`stats::pairwise.t.test()` or
#'   `PMCMRplus::gamesHowellTest()`) that is run first to enumerate the pairs,
#'   so they do not change the Bayes factors, but unsupported arguments can
#'   still cause an error.
#' @inheritParams stats::t.test
#' @inheritParams WRS2::rmmcp
#'
#' @section Pairwise comparison tests:
#'
#' ```{r child="man/rmd-fragments/table_intro.Rmd"}
#' ```
#'
#' ```{r child="man/rmd-fragments/pairwise_comparisons.Rmd"}
#' ```
#'
#' @returns
#'
#' ```{r child="man/rmd-fragments/return.Rmd"}
#' ```
#'
#' @references For more, see:
#' <https://www.indrapatil.com/ggstatsplot/articles/web_only/pairwise.html>
#'
#' @autoglobal
#'
#' @examplesIf identical(Sys.getenv("NOT_CRAN"), "true")
#' # for reproducibility
#' set.seed(123)
#' library(statsExpressions)
#'
#' #------------------- between-subjects design ----------------------------
#'
#' # parametric
#' # if `var.equal = TRUE`, then Student's t-test will be run
#' pairwise_comparisons(
#'   data            = mtcars,
#'   x               = cyl,
#'   y               = wt,
#'   type            = "parametric",
#'   var.equal       = TRUE,
#'   paired          = FALSE,
#'   p.adjust.method = "none"
#' )
#'
#' # if `var.equal = FALSE`, then Games-Howell test will be run
#' pairwise_comparisons(
#'   data            = mtcars,
#'   x               = cyl,
#'   y               = wt,
#'   type            = "parametric",
#'   var.equal       = FALSE,
#'   paired          = FALSE,
#'   p.adjust.method = "bonferroni"
#' )
#'
#' # non-parametric (Dunn test)
#' pairwise_comparisons(
#'   data            = mtcars,
#'   x               = cyl,
#'   y               = wt,
#'   type            = "nonparametric",
#'   paired          = FALSE,
#'   p.adjust.method = "none"
#' )
#'
#' # robust (Yuen's trimmed means *t*-test)
#' pairwise_comparisons(
#'   data            = mtcars,
#'   x               = cyl,
#'   y               = wt,
#'   type            = "robust",
#'   paired          = FALSE,
#'   p.adjust.method = "fdr"
#' )
#'
#' # Bayes Factor (Student's *t*-test)
#' pairwise_comparisons(
#'   data   = mtcars,
#'   x      = cyl,
#'   y      = wt,
#'   type   = "bayes",
#'   paired = FALSE
#' )
#'
#' #------------------- within-subjects design ----------------------------
#'
#' # parametric (Student's *t*-test)
#' pairwise_comparisons(
#'   data            = bugs_long,
#'   x               = condition,
#'   y               = desire,
#'   subject.id      = subject,
#'   type            = "parametric",
#'   paired          = TRUE,
#'   p.adjust.method = "BH"
#' )
#'
#' # non-parametric (Durbin-Conover test)
#' pairwise_comparisons(
#'   data            = bugs_long,
#'   x               = condition,
#'   y               = desire,
#'   subject.id      = subject,
#'   type            = "nonparametric",
#'   paired          = TRUE,
#'   p.adjust.method = "BY"
#' )
#'
#' # robust (Yuen's trimmed means *t*-test)
#' pairwise_comparisons(
#'   data            = bugs_long,
#'   x               = condition,
#'   y               = desire,
#'   subject.id      = subject,
#'   type            = "robust",
#'   paired          = TRUE,
#'   p.adjust.method = "hommel"
#' )
#'
#' # Bayes Factor (Student's *t*-test)
#' pairwise_comparisons(
#'   data       = bugs_long,
#'   x          = condition,
#'   y          = desire,
#'   subject.id = subject,
#'   type       = "bayes",
#'   paired     = TRUE
#' )
#'
#' @template citation
#'
#' @export
pairwise_comparisons <- function(
  data,
  x,
  y,
  subject.id = NULL,
  type = "parametric",
  paired = FALSE,
  var.equal = FALSE,
  tr = 0.2,
  bf.prior = 0.707,
  p.adjust.method = "holm",
  digits = 2L,
  exact = FALSE,
  ...
) {
  # data -------------------------------------------

  type <- extract_stats_type(type)
  x <- ensym(x)
  y <- ensym(y)

  data <- long_to_wide_converter(
    data,
    x = {{ x }},
    y = {{ y }},
    subject.id = {{ subject.id }},
    paired = paired,
    spread = FALSE
  )

  # a few functions expect these as vectors
  x_vec <- pull(data, {{ x }})
  y_vec <- pull(data, {{ y }})
  g_vec <- pull(data, .rowid)
  .f.args <- list(paired = paired, p.adjust.method = "none", exact = exact, ...)

  # parametric & Bayesian ---------------------------------

  if (type %in% c("parametric", "bayes")) {
    if (var.equal || paired) {
      .f <- stats::pairwise.t.test
      test <- "Student's t"
    } else {
      .f <- PMCMRplus::gamesHowellTest
      test <- "Games-Howell"
    }
  }

  # nonparametric ----------------------------

  if (type == "nonparametric") {
    if (paired) {
      .f <- PMCMRplus::durbinAllPairsTest
      test <- "Durbin-Conover"
    } else {
      .f <- PMCMRplus::kwAllPairsDunnTest
      test <- "Dunn"
    }

    # Durbin-Conover needs `y`, but it can't be a common argument because
    # `pairwise.t.test()` would pass it on to `t.test()`
    .f.args$y <- y_vec
  }

  if (type != "robust") {
    df_pair <- suppressWarnings(exec(
      .f,
      # Dunn, Games-Howell, Student's t-test
      x = y_vec,
      g = x_vec,
      # Durbin-Conover test
      groups = x_vec,
      blocks = g_vec,
      # common
      !!!.f.args
    )) |>
      tidy_model_parameters() |>
      select(-matches("^parameter1$|^parameter2$")) |>
      rename(group2 = group1, group1 = group2)
  }

  # robust ----------------------------------

  if (type == "robust") {
    df_pair <- if (paired) {
      WRS2::rmmcp(y = y_vec, groups = x_vec, blocks = g_vec, tr = tr)
    } else {
      WRS2::lincon(new_formula(y, x), data, tr = tr, method = "none")
    }

    df_pair <- tidy_model_parameters(df_pair)
    test <- "Yuen's trimmed means"
  }

  # Bayesian --------------------------------

  if (type == "bayes") {
    df_tidy <- map2_vec(
      .x = as.character(df_pair$group1),
      .y = as.character(df_pair$group2),
      .f = function(a, b) {
        two_sample_test(
          data = droplevels(filter(data, {{ x }} %in% c(a, b))),
          x = {{ x }},
          y = {{ y }},
          paired = paired,
          bf.prior = bf.prior,
          type = "bayes"
        )
      }
    ) |>
      filter(term == "Difference") |>
      mutate(
        expression = glue(
          "list(log[e]*(BF['01'])=='{format_value(-log(bf10), digits)}')"
        ),
        test = "Student's t"
      )

    df_pair <- bind_cols(select(df_pair, group1, group2), df_tidy)
  }

  # expression formatting ----------------------------------

  df_pair <- df_pair |>
    mutate(across(where(is.factor), as.character)) |>
    arrange(group1, group2) |>
    select(group1, group2, everything())

  if (type != "bayes") {
    df_pair <- df_pair |>
      .pairwise_p_adjust_expr(p.adjust.method, digits, test) |>
      mutate(p.value = p.value.adj) |>
      select(-p.value.adj)
  }

  select(df_pair, -matches("p.adjustment|^method$")) |>
    .glue_to_expression()
}

#' @noRd
.pairwise_p_adjust_expr <- function(data, p.adjust.method, digits, test) {
  method_label <- insight::format_capitalize(p.adjust.method) |>
    replace_values(c("BH", "Fdr") ~ "FDR")

  p_subscript <- if (method_label == "None") {
    "unadj."
  } else {
    glue("'{method_label}'-adj.")
  }

  data |>
    mutate(
      p.value.adj = stats::p.adjust(p = p.value, method = p.adjust.method),
      p.adjust.method = method_label,
      test = test,
      expression = glue(
        "list(italic(p)[{p_subscript}]=='{format_value(p.value.adj, digits)}')"
      )
    )
}
