# for reproducibility
set.seed(123)

#### -------------------- association test ------------------------ ####

# ------------------------ frequentist ---------------------------------

# unpaired
contingency_table(mtcars, am, vs)

# paired
paired_data <- dplyr::tibble(
  response_before = factor(c("no", "yes", "no", "yes")),
  response_after = factor(c("no", "no", "yes", "yes")),
  Freq = c(65L, 25L, 5L, 5L)
)

contingency_table(
  paired_data,
  response_before,
  response_after,
  paired = TRUE,
  counts = Freq
)

# ------------------------ Bayesian -------------------------------------

# only unpaired designs are supported
contingency_table(mtcars, am, vs, type = "bayes")

#### -------------------- goodness-of-fit test -------------------- ####

hair_eye_data <- as.data.frame(HairEyeColor)

# ------------------------ frequentist ---------------------------------

contingency_table(hair_eye_data, Eye, counts = Freq)

# ------------------------ Bayesian -------------------------------------

contingency_table(
  hair_eye_data,
  Eye,
  counts = Freq,
  ratio = c(0.2, 0.2, 0.3, 0.3),
  type = "bayes"
)
