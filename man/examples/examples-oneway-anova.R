# for reproducibility
set.seed(123)

# ----------------------- parametric -------------------------------------

# between-subjects
oneway_anova(
  data = mtcars,
  x = cyl,
  y = wt
)

# biased (partial eta-squared) effect size
oneway_anova(
  data = mtcars,
  x = cyl,
  y = wt,
  effsize.type = "eta"
)

# within-subjects design
oneway_anova(
  data = bugs_long,
  x = condition,
  y = desire,
  subject.id = subject,
  paired = TRUE
)

# ----------------------- non-parametric ----------------------------------

# between-subjects
oneway_anova(
  data = mtcars,
  x = cyl,
  y = wt,
  type = "np"
)

# within-subjects design
oneway_anova(
  data = bugs_long,
  x = condition,
  y = desire,
  subject.id = subject,
  paired = TRUE,
  type = "np"
)

# ----------------------- robust -------------------------------------

# between-subjects
oneway_anova(
  data = mtcars,
  x = cyl,
  y = wt,
  type = "r"
)

# within-subjects design
oneway_anova(
  data = bugs_long,
  x = condition,
  y = desire,
  subject.id = subject,
  paired = TRUE,
  type = "r"
)

# ----------------------- Bayesian -------------------------------------

# between-subjects
oneway_anova(
  data = mtcars,
  x = cyl,
  y = wt,
  type = "bayes"
)

# within-subjects design
oneway_anova(
  data = bugs_long,
  x = condition,
  y = desire,
  subject.id = subject,
  paired = TRUE,
  type = "bayes"
)
