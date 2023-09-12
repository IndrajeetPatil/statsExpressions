# for reproducibility
set.seed(123)

# ----------------------- parametric -----------------------

corr_test(mtcars, wt, mpg, type = "parametric")

# ----------------------- non-parametric -------------------

corr_test(mtcars, wt, mpg, type = "nonparametric")

# ----------------------- robust ---------------------------

corr_test(mtcars, wt, mpg, type = "robust")

# ----------------------- Bayesian -------------------------

corr_test(mtcars, wt, mpg, type = "bayes")
