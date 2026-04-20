# for reproducibility
set.seed(123)
library(statsExpressions)

# pairwise Fisher's exact tests with Holm adjustment
pairwise_contingency_table(
  data = mtcars,
  x = cyl,
  y = am,
  p.adjust.method = "holm"
)

# with counts data and Bonferroni adjustment
pairwise_contingency_table(
  data = as.data.frame(Titanic),
  x = Class,
  y = Survived,
  counts = Freq,
  p.adjust.method = "bonferroni"
)

# no p-value adjustment
pairwise_contingency_table(
  data = mtcars,
  x = cyl,
  y = am,
  p.adjust.method = "none"
)
