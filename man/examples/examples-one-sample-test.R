# for reproducibility
set.seed(123)

# ----------------------- parametric -----------------------

one_sample_test(mtcars, wt, test.value = 3)

# biased (Cohen's d) effect size
one_sample_test(mtcars, wt, test.value = 3, effsize.type = "d")

# ----------------------- non-parametric -------------------

one_sample_test(mtcars, wt, test.value = 3, type = "nonparametric")

# ----------------------- robust ---------------------------

one_sample_test(mtcars, wt, test.value = 3, type = "robust")

# ----------------------- Bayesian -------------------------

one_sample_test(mtcars, wt, test.value = 3, type = "bayes")
