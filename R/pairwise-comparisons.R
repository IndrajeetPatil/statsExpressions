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
#' @param p.adjust.method Adjustment method for *p*-values for multiple
#'   comparisons. Possible methods are: `"holm"` (default), `"hochberg"`,
#'   `"hommel"`, `"bonferroni"`, `"BH"`, `"BY"`, `"fdr"`, `"none"`.
#' @param ... Additional arguments passed to other methods.
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
#' <https://indrajeetpatil.github.io/ggstatsplot/articles/web_only/pairwise.html>
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
#' # robust (Yuen's trimmed means t-test)
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
    ...) {
  # data -------------------------------------------

  type <- extract_stats_type(type)
  c(x, y) %<-% c(ensym(x), ensym(y))

  data %<>% long_to_wide_converter(
    x          = {{ x }},
    y          = {{ y }},
    subject.id = {{ subject.id }},
    paired     = paired,
    spread     = FALSE
  )

  # a few functions expect these as vectors
  x_vec <- pull(data, {{ x }})
  y_vec <- pull(data, {{ y }})
  g_vec <- pull(data, .rowid)
  .f.args <- list(paired = paired, p.adjust.method = "none", exact = FALSE, ...)

  # parametric ---------------------------------

  if (type %in% c("parametric", "bayes")) {
    # styler: off
    if (var.equal || paired)    c(.f, test) %<-% c(stats::pairwise.t.test, "Student's t")
    if (!(var.equal || paired)) c(.f, test) %<-% c(PMCMRplus::gamesHowellTest, "Games-Howell")
    # styler: on
  }

  # nonparametric ----------------------------

  if (type == "nonparametric") {
    # styler: off
    if (!paired) c(.f, test) %<-% c(PMCMRplus::kwAllPairsDunnTest, "Dunn")
    if (paired)  c(.f, test) %<-% c(PMCMRplus::durbinAllPairsTest, "Durbin-Conover")
    # styler: on

    # `exec` fails otherwise for `pairwise.t.test` because `y` is passed to `t.test`
    .f.args <- utils::modifyList(.f.args, list(y = y_vec))
  }

  if (type != "robust") {
    df_pair <- suppressWarnings(exec(
      .f,
      # Dunn, Games-Howell, Student's t-test
      x      = y_vec,
      g      = x_vec,
      # Durbin-Conover test
      groups = x_vec,
      blocks = g_vec,
      # common
      !!!.f.args
    )) %>%
      tidy_model_parameters() %>%
      select(-matches("^parameter1$|^parameter2$")) %>%
      rename(group2 = group1, group1 = group2)
  }

  # robust ----------------------------------

  if (type == "robust") {
    if (!paired) {
      c(.ns, .fn) %<-% c("WRS2", "lincon")
      .f.args <- list(formula = new_formula(y, x), data = data, method = "none")
    }

    if (paired) {
      c(.ns, .fn) %<-% c("WRS2", "rmmcp")
      .f.args <- list(y = quote(y_vec), groups = quote(x_vec), blocks = quote(g_vec))
    }

    df_pair <- eval(call2(.ns = .ns, .fn = .fn, tr = tr, !!!.f.args)) %>% tidy_model_parameters()
    test <- "Yuen's trimmed means"
  }

  # Bayesian --------------------------------

  if (type == "bayes") {
    df_tidy <- map_dfr(
      # creating a list of data frames with subsections of data
      .x = map2(
        .x = as.character(df_pair$group1),
        .y = as.character(df_pair$group2),
        .f = function(a, b) droplevels(filter(data, {{ x }} %in% c(a, b)))
      ),
      .f = ~ two_sample_test(
        data     = .x,
        x        = {{ x }},
        y        = {{ y }},
        paired   = paired,
        bf.prior = bf.prior,
        type     = "bayes"
      )
    ) %>%
      filter(term == "Difference") %>%
      mutate(
        expression = glue("list(log[e]*(BF['01'])=='{format_value(-log(bf10), digits)}')"),
        test = "Student's t"
      )

    df_pair <- bind_cols(select(df_pair, group1, group2), df_tidy)
  }

  # expression formatting ----------------------------------

  df_pair %<>%
    mutate(across(where(is.factor), ~ as.character())) %>%
    arrange(group1, group2) %>%
    select(group1, group2, everything())

  if (type != "bayes") {
    df_pair %<>%
      mutate(
        p.value = stats::p.adjust(p = p.value, method = p.adjust.method),
        p.adjust.method = p_adjust_text(p.adjust.method),
        test = test,
        expression = case_when(
          p.adjust.method == "None" ~ glue("list(italic(p)[unadj.]=='{format_value(p.value, digits)}')"),
          .default = glue("list(italic(p)['{p.adjust.method}'-adj.]=='{format_value(p.value, digits)}')")
        )
      )
  }

  select(df_pair, everything(), -matches("p.adjustment|^method$")) %>%
    .glue_to_expression()
}

#' @title *p*-value adjustment method text
#' @name p_adjust_text
#'
#' @description
#' Preparing text to describe which *p*-value adjustment method was used
#'
#' @returns Standardized text description for what method was used.
#'
#' @inheritParams pairwise_comparisons
#'
#' @examples
#' p_adjust_text("none")
#' p_adjust_text("BY")
#'
#' @export
p_adjust_text <- function(p.adjust.method) {
  case_when(
    grepl("^n|^bo|^h", p.adjust.method) ~ paste0(
      toupper(substr(p.adjust.method, 1L, 1L)),
      substr(p.adjust.method, 2L, nchar(p.adjust.method))
    ),
    grepl("^BH|^f", p.adjust.method) ~ "FDR",
    grepl("^BY", p.adjust.method) ~ "BY",
    .default = "Holm"
  )
}
