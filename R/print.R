#' @export
print.statsExpressions <- function(x, ...) {
  local_options(
    pillar.width = Inf,
    pillar.bold = TRUE,
    pillar.subtle_num = TRUE
  )
  NextMethod()
  invisible(x)
}
