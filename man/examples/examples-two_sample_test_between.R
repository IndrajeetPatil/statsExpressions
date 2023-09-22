# ----------------------- between-subjects -------------------------------------

# for reproducibility
set.seed(123)

# ----------------------- parametric ---------------------------------------

# unequal variance
two_sample_test(ToothGrowth, supp, len, type = "parametric")

# equal variance
two_sample_test(ToothGrowth, supp, len, type = "parametric", var.equal = TRUE)

# ----------------------- non-parametric -----------------------------------

two_sample_test(ToothGrowth, supp, len, type = "nonparametric")

# ----------------------- robust --------------------------------------------

two_sample_test(ToothGrowth, supp, len, type = "robust")

# ----------------------- Bayesian ---------------------------------------

two_sample_test(ToothGrowth, supp, len, type = "bayes")
