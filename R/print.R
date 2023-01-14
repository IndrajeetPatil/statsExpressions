#' @export
print.statsExpressions <- function(x, ...) {
  withr::with_options(
    list(pillar.width = Inf, pillar.bold = TRUE, pillar.subtle_num = TRUE),
    NextMethod()
  )

  invisible(x)
}
