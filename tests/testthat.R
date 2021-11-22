library(testthat)
library(dplyr)
library(statsExpressions)

## pretty tibble printing
options(
  tibble.width = Inf,
  pillar.bold = TRUE,
  pillar.neg = TRUE,
  pillar.subtle_num = TRUE,
  pillar.min_chars = Inf
)

test_check("statsExpressions")
