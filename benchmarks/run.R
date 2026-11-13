# Each command includes R startup, dependency loading, and five API calls.
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 1L)

workload <- switch(
  args[[1L]],
  "centrality-parametric" = function() {
    statsExpressions::centrality_description(
      data = datasets::mtcars,
      x = am,
      y = wt,
      type = "parametric"
    )
  },
  "one-sample-parametric" = function() {
    statsExpressions::one_sample_test(
      data = datasets::mtcars,
      x = wt,
      type = "parametric"
    )
  },
  "two-sample-parametric" = function() {
    statsExpressions::two_sample_test(
      data = datasets::ToothGrowth,
      x = supp,
      y = len,
      type = "parametric"
    )
  },
  "anova-parametric" = function() {
    statsExpressions::oneway_anova(
      data = datasets::iris,
      x = Species,
      y = Sepal.Length,
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
  "contingency-parametric" = function() {
    statsExpressions::contingency_table(
      data = as.data.frame(datasets::HairEyeColor),
      x = Hair,
      y = Eye,
      counts = Freq
    )
  },
  "pairwise-parametric" = function() {
    statsExpressions::pairwise_comparisons(
      data = datasets::iris,
      x = Species,
      y = Sepal.Length,
      type = "parametric"
    )
  },
  "pairwise-contingency" = function() {
    statsExpressions::pairwise_contingency_table(
      data = as.data.frame(datasets::HairEyeColor),
      x = Hair,
      y = Eye,
      counts = Freq
    )
  },
  stop("Unknown benchmark: ", args[[1L]], call. = FALSE)
)

for (iteration in seq_len(5L)) {
  result <- workload()
  stopifnot(
    is.data.frame(result),
    nrow(result) > 0L,
    length(result$expression) == nrow(result)
  )
}
