# for reproducibility
set.seed(123)

# ----------------------- parametric -----------------------

centrality_description(iris, Species, Sepal.Length, type = "parametric")

# ----------------------- non-parametric -------------------

centrality_description(mtcars, am, wt, type = "nonparametric")

# ----------------------- robust ---------------------------

centrality_description(ToothGrowth, supp, len, type = "robust")

# ----------------------- Bayesian -------------------------

centrality_description(sleep, group, extra, type = "bayes")
