#' @title Format expression to R Markdown
#' @name format_markdown
#'
#' @description
#'
#'  Pass the expression generated using any of the functions to
#'  get statistical details ready for the writing reports in R Markdown
#'
#' @param expr The expression returned inside the data.frame containing
#' statistical details
#' @param ... Currently ignored
#'
#' @examples
#' \donttest{
#' # for reproducibility
#' set.seed(123)
#' library(statsExpressions)
#' options(tibble.width = Inf, pillar.bold = TRUE, pillar.neg = TRUE)
#'
#' # without changing defaults
#' result <- corr_test(
#'   data = ggplot2::midwest,
#'   x    = area,
#'   y    = percblack
#' )
#'
#' format_markdown(result$expression)
#'
#' result <- oneway_anova(
#'   data = iris_long,
#'   x = condition,
#'   y = value,
#'   paired = TRUE
#' )
#'
#' format_markdown(result$expression)
#'
#' }
#' @export

format_markdown <- function(expr, ...) {

  # trasnform expression to be able to modify it
  if (is.list(expr)) expr <- expr[[1]]
  expr <- as.list(x = as.list(expr)[[1]])

  # replace invalid patterns to be evaluated
  expr <- gsub(")[", ") * \"\"[", expr, fixed = TRUE)
  expr <- gsub("](", "] * list2(", expr, fixed = TRUE)
  expr <- gsub("~", "%@%", expr, fixed = TRUE)

  # transform to again to expression to evaluate it
  expr <- lapply(expr, str2lang)
  expr <- as.call(expr)

  # global variables; could be changed for anything else
  p <- "p"; CI <- "CI"; chi <- "*X*";
  mu <- "*mu*"; log <- "log"; BF <- "BF";
  e <- "e"; epsilon <- "Epsilon"; R <- "R";
  HDI <- "HDI"; xi <- "xi"; omega <- "Omega";

  # list works pasting expressions
  list <- function(...) paste(..., sep = ", ")

  # `list2` works the same but only for expressions with >= 1 parameter(s)
  list2 <- function(...) paste0("(", paste(..., sep = ", "), ")")

  # italic text just has stars around it
  italic <- function(s) paste0("*", s, "*")

  # wide hat has no effect on final output
  widehat <- function(s) as.character(s)

  # single subscripts are entered using subsetting
  `[` <- function(main, subscript) paste0(main, "~", subscript, "~")

  # single superscript are entered using symbol in both sides
  `^` <- function(main, superscript) paste0(main, "^", superscript, "^")

  # this symbol will concatenate
  `*` <- function(lhs, rhs) paste0(lhs, rhs, collapse = ", ")
  `%@%` <- function(lhs, rhs) paste0(lhs, rhs)

  # replace to equal sign
  `==` <- function(lhs, rhs) paste0(lhs, " = ", rhs)

  # evaluate the expression to produce a string
  eval(expr)
}
