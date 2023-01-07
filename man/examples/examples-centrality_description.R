# for reproducibility
set.seed(123)

# ----------------------- parametric -----------------------

centrality_description(iris, Species, Sepal.Length)

# ----------------------- non-parametric -------------------

centrality_description(mtcars, am, wt, type = "n")

# ----------------------- robust ---------------------------

centrality_description(ToothGrowth, supp, len, type = "r")

# ----------------------- Bayesian -------------------------

centrality_description(sleep, group, extra, type = "b")
