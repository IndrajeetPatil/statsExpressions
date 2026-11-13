# Each command measures R startup, package loading, and a repeated API workload.
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 1L)

workload <- switch(
  args[[1L]],
  "two-sample-parametric" = function() {
    statsExpressions::two_sample_test(
      data = datasets::ToothGrowth,
      x = supp,
      y = len,
      type = "parametric"
    )
  },
  "correlation-parametric" = function() {
    statsExpressions::corr_test(
      data = datasets::mtcars,
      x = wt,
      y = mpg,
      type = "parametric"
    )
  },
  stop("Unknown benchmark: ", args[[1L]], call. = FALSE)
)

for (iteration in seq_len(25L)) {
  result <- workload()
  stopifnot(
    nrow(result) == 1L,
    is.finite(result$statistic),
    is.finite(result$p.value),
    length(result$expression) == 1L
  )
}
