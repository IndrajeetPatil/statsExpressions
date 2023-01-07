# for reproducibility
set.seed(123)




# ----------------------- parametric -----------------------

one_sample_test(mtcars, wt, test.value = 3)

# ----------------------- non-parametric -------------------

one_sample_test(mtcars, wt, test.value = 3, type = "n")

# ----------------------- robust ---------------------------

one_sample_test(mtcars, wt, test.value = 3, type = "r")

# ----------------------- Bayesian -------------------------

one_sample_test(mtcars, wt, test.value = 3, type = "b")
