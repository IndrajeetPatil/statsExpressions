# Each command includes R startup, dependency loading, and five API calls.
args <- commandArgs(TRUE)
stopifnot(length(args) == 1L)
hair_eye <- as.data.frame(datasets::HairEyeColor)
hair_eye <- hair_eye[rep(seq_len(nrow(hair_eye)), hair_eye$Freq), ]

workload <- switch(
  args[[1L]],
  "centrality-parametric" = function() {
    statsExpressions::centrality_description(datasets::mtcars, am, wt)
  },
  "one-sample-parametric" = function() {
    statsExpressions::one_sample_test(datasets::mtcars, wt)
  },
  "two-sample-parametric" = function() {
    statsExpressions::two_sample_test(datasets::ToothGrowth, supp, len)
  },
  "anova-parametric" = function() {
    statsExpressions::oneway_anova(datasets::iris, Species, Sepal.Length)
  },
  "correlation-parametric" = function() {
    statsExpressions::corr_test(datasets::mtcars, wt, mpg)
  },
  "contingency-parametric" = function() {
    statsExpressions::contingency_table(hair_eye, Hair, Eye)
  },
  "pairwise-parametric" = function() {
    statsExpressions::pairwise_comparisons(
      datasets::iris,
      Species,
      Sepal.Length
    )
  },
  "pairwise-contingency" = function() {
    statsExpressions::pairwise_contingency_table(hair_eye, Hair, Eye)
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
