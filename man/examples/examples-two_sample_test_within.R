# ----------------------- within-subjects -------------------------------------

# data
df <- dplyr::filter(bugs_long, condition %in% c("LDLF", "LDHF"))

# for reproducibility
set.seed(123)

# ----------------------- parametric ---------------------------------------

two_sample_test(df, condition, desire, subject.id = subject, paired = TRUE, type = "parametric")

# ----------------------- non-parametric -----------------------------------

two_sample_test(df, condition, desire, subject.id = subject, paired = TRUE, type = "nonparametric")

# ----------------------- robust --------------------------------------------

two_sample_test(df, condition, desire, subject.id = subject, paired = TRUE, type = "robust")

# ----------------------- Bayesian ---------------------------------------

two_sample_test(df, condition, desire, subject.id = subject, paired = TRUE, type = "bayes")
