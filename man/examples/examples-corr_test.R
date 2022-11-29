# for reproducibility
set.seed(123)

# for pretty-printing tibble
options(tibble.width = Inf, pillar.bold = TRUE, pillar.neg = TRUE)

# ----------------------- parametric -----------------------

corr_test(mtcars, wt, mpg)

# ----------------------- non-parametric -------------------

corr_test(mtcars, wt, mpg, type = "n")

# ----------------------- robust ---------------------------

corr_test(mtcars, wt, mpg, type = "r")

# ----------------------- Bayesian -------------------------

corr_test(mtcars, wt, mpg, type = "b")
