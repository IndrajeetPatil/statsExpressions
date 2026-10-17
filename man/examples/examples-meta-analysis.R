# let's use `mag` dataset from `{metaplus}`
data(mag, package = "metaplus")
dat <- dplyr::rename(mag, estimate = yi, std.error = sei)

# ----------------------- parametric ---------------------------------------

set.seed(123)
meta_analysis(dat)

# ----------------------- robust --------------------------------------------

set.seed(123)
meta_analysis(dat, type = "robust", random = "normal")

# ----------------------- Bayesian ------------------------------------------

# suppress warnings about divergent transitions after warmup
set.seed(123)
suppressWarnings(meta_analysis(dat, type = "bayes"))
