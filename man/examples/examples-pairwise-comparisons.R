# for reproducibility
set.seed(123)

#------------------- between-subjects design ----------------------------

# parametric
# if `var.equal = TRUE`, then Student's t-test will be run
pairwise_comparisons(
  data = mtcars,
  x = cyl,
  y = wt,
  type = "parametric",
  var.equal = TRUE,
  paired = FALSE,
  p.adjust.method = "none"
)

# if `var.equal = FALSE`, then Games-Howell test will be run
pairwise_comparisons(
  data = mtcars,
  x = cyl,
  y = wt,
  type = "parametric",
  var.equal = FALSE,
  paired = FALSE,
  p.adjust.method = "bonferroni"
)

# non-parametric (Dunn test)
pairwise_comparisons(
  data = mtcars,
  x = cyl,
  y = wt,
  type = "nonparametric",
  paired = FALSE,
  p.adjust.method = "none"
)

# robust (Yuen's trimmed means *t*-test)
pairwise_comparisons(
  data = mtcars,
  x = cyl,
  y = wt,
  type = "robust",
  paired = FALSE,
  p.adjust.method = "fdr"
)

# Bayes Factor (Student's *t*-test)
pairwise_comparisons(
  data = mtcars,
  x = cyl,
  y = wt,
  type = "bayes",
  paired = FALSE
)

#------------------- within-subjects design ----------------------------

# parametric (Student's *t*-test)
pairwise_comparisons(
  data = bugs_long,
  x = condition,
  y = desire,
  subject.id = subject,
  type = "parametric",
  paired = TRUE,
  p.adjust.method = "BH"
)

# non-parametric (Durbin-Conover test)
pairwise_comparisons(
  data = bugs_long,
  x = condition,
  y = desire,
  subject.id = subject,
  type = "nonparametric",
  paired = TRUE,
  p.adjust.method = "BY"
)

# robust (Yuen's trimmed means *t*-test)
pairwise_comparisons(
  data = bugs_long,
  x = condition,
  y = desire,
  subject.id = subject,
  type = "robust",
  paired = TRUE,
  p.adjust.method = "hommel"
)

# Bayes Factor (Student's *t*-test)
pairwise_comparisons(
  data = bugs_long,
  x = condition,
  y = desire,
  subject.id = subject,
  type = "bayes",
  paired = TRUE
)
