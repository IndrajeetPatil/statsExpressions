# for reproducibility
set.seed(123)

# ----------------------- parametric -------------------------------------

# between-subjects
oneway_anova(mtcars, cyl, wt)

# biased (partial eta-squared) effect size
oneway_anova(mtcars, cyl, wt, effsize.type = "eta")

# within-subjects design
oneway_anova(bugs_long, condition, desire, subject.id = subject, paired = TRUE)

# ----------------------- non-parametric ----------------------------------

# between-subjects
oneway_anova(mtcars, cyl, wt, type = "np")

# within-subjects design
oneway_anova(
  bugs_long,
  condition,
  desire,
  subject.id = subject,
  paired = TRUE,
  type = "np"
)

# ----------------------- robust -------------------------------------

# between-subjects
oneway_anova(mtcars, cyl, wt, type = "r")

# within-subjects design
oneway_anova(
  bugs_long,
  condition,
  desire,
  subject.id = subject,
  paired = TRUE,
  type = "r"
)

# ----------------------- Bayesian -------------------------------------

# between-subjects
oneway_anova(mtcars, cyl, wt, type = "bayes")

# within-subjects design
oneway_anova(
  bugs_long,
  condition,
  desire,
  subject.id = subject,
  paired = TRUE,
  type = "bayes"
)
