# for reproducibility
set.seed(123)

#------------------- between-subjects design ----------------------------

# parametric
# if `var.equal = TRUE`, then Student's t-test will be run
pairwise_comparisons(
  mtcars,
  cyl,
  wt,
  var.equal = TRUE,
  p.adjust.method = "none"
)

# if `var.equal = FALSE`, then Games-Howell test will be run
pairwise_comparisons(mtcars, cyl, wt, p.adjust.method = "bonferroni")

# non-parametric (Dunn test)
pairwise_comparisons(
  mtcars,
  cyl,
  wt,
  type = "nonparametric",
  p.adjust.method = "none"
)

# robust (Yuen's trimmed means t-test)
pairwise_comparisons(mtcars, cyl, wt, type = "robust", p.adjust.method = "fdr")

# Bayes Factor (Student's t-test)
pairwise_comparisons(mtcars, cyl, wt, type = "bayes")

#------------------- within-subjects design ----------------------------

# parametric (Student's t-test)
pairwise_comparisons(
  bugs_long,
  condition,
  desire,
  subject.id = subject,
  paired = TRUE,
  p.adjust.method = "BH"
)

# non-parametric (Durbin-Conover test)
pairwise_comparisons(
  bugs_long,
  condition,
  desire,
  subject.id = subject,
  type = "nonparametric",
  paired = TRUE,
  p.adjust.method = "BY"
)

# robust (Yuen's trimmed means t-test)
pairwise_comparisons(
  bugs_long,
  condition,
  desire,
  subject.id = subject,
  type = "robust",
  paired = TRUE,
  p.adjust.method = "hommel"
)

# Bayes Factor (Student's t-test)
pairwise_comparisons(
  bugs_long,
  condition,
  desire,
  subject.id = subject,
  type = "bayes",
  paired = TRUE
)
