#### -------------------- association test ------------------------ ####

# ------------------------ frequentist ---------------------------------

# unpaired

set.seed(123)
contingency_table(
  data = mtcars,
  x = am,
  y = vs,
  paired = FALSE
)

# paired

paired_data <- dplyr::tibble(
  response_before = factor(c("no", "yes", "no", "yes")),
  response_after = factor(c("no", "no", "yes", "yes")),
  Freq = c(65L, 25L, 5L, 5L)
)

set.seed(123)
contingency_table(
  data = paired_data,
  x = response_before,
  y = response_after,
  paired = TRUE,
  counts = Freq
)

# ------------------------ Bayesian -------------------------------------

# unpaired

set.seed(123)
contingency_table(
  data = mtcars,
  x = am,
  y = vs,
  paired = FALSE,
  type = "bayes"
)

# paired

set.seed(123)
contingency_table(
  data = paired_data,
  x = response_before,
  y = response_after,
  paired = TRUE,
  counts = Freq,
  type = "bayes"
)

#### -------------------- goodness-of-fit test -------------------- ####

# ------------------------ frequentist ---------------------------------

set.seed(123)
contingency_table(
  data = as.data.frame(HairEyeColor),
  x = Eye,
  counts = Freq
)

# ------------------------ Bayesian -------------------------------------

set.seed(123)
contingency_table(
  data = as.data.frame(HairEyeColor),
  x = Eye,
  counts = Freq,
  ratio = c(0.2, 0.2, 0.3, 0.3),
  type = "bayes"
)
