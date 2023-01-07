#' @export
print.statsExpressions <- function(x, ...) {
  withr::with_options(
    list(
      tibble.width      = Inf,
      pillar.bold       = TRUE,
      pillar.neg        = TRUE,
      pillar.subtle_num = TRUE,
      pillar.min_chars  = Inf
    ),
    code = {
      NextMethod()
    }
  )

  invisible(x)
}
