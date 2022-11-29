# ----------------------- parametric -----------------------
set.seed(123)
centrality_description(iris, Species, Sepal.Length)

# ----------------------- non-parametric -------------------
set.seed(123)
centrality_description(mtcars, am, wt, type = "n")

# ----------------------- robust ---------------------------
set.seed(123)
centrality_description(ToothGrowth, supp, len, type = "r")

# ----------------------- Bayesian -------------------------
set.seed(123)
centrality_description(sleep, group, extra, type = "b")
