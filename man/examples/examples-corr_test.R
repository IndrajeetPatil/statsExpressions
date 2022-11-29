# for reproducibility
set.seed(123)

# ----------------------- parametric -----------------------

corr_test(mtcars, wt, mpg)

# ----------------------- non-parametric -------------------

corr_test(mtcars, wt, mpg, type = "n")

# ----------------------- robust ---------------------------

corr_test(mtcars, wt, mpg, type = "r")

# ----------------------- Bayesian -------------------------

corr_test(mtcars, wt, mpg, type = "b")
