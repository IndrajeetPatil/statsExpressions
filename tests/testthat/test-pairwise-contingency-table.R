# basic usage --------------------------------------------------

test_that("pairwise_contingency_table works - basic", {
  set.seed(123)
  df1 <- pairwise_contingency_table(
    data = mtcars,
    x = cyl,
    y = am,
    p.adjust.method = "bonferroni"
  )

  expect_snapshot(select(df1, -expression))
  expect_snapshot(df1[["expression"]])

  # correct number of pairs (3 levels -> 3 pairs)
  expect_equal(nrow(df1), 3L)

  # both raw and adjusted p-values present
  expect_true(all(c("p.value", "p.value.adj") %in% names(df1)))

  # adjusted should be >= raw
  expect_true(all(df1$p.value.adj >= df1$p.value))
})

# counts data --------------------------------------------------

test_that("pairwise_contingency_table works - with counts", {
  set.seed(123)
  df1 <- pairwise_contingency_table(
    data = as.data.frame(Titanic),
    x = Class,
    y = Survived,
    counts = Freq,
    p.adjust.method = "fdr"
  )

  expect_snapshot(select(df1, -expression))
  expect_snapshot(df1[["expression"]])

  # 4 levels -> C(4,2) = 6 pairs
  expect_equal(nrow(df1), 6L)
})

# no adjustment --------------------------------------------------

test_that("pairwise_contingency_table works - no adjustment", {
  set.seed(123)
  df1 <- pairwise_contingency_table(
    data = mtcars,
    x = cyl,
    y = am,
    p.adjust.method = "none"
  )

  expect_snapshot(select(df1, -expression))
  expect_snapshot(df1[["expression"]])

  # with no adjustment, raw and adjusted should be equal
  expect_equal(df1$p.value, df1$p.value.adj)
})

# data with NAs --------------------------------------------------

test_that("pairwise_contingency_table works - data with NAs", {
  set.seed(123)
  df1 <- pairwise_contingency_table(
    data = msleep,
    x = vore,
    y = conservation,
    p.adjust.method = "holm"
  )

  expect_snapshot(select(df1, -expression))
  expect_snapshot(df1[["expression"]])
})

# custom conf.level and alternative --------------------------------------------------

test_that("pairwise_contingency_table works - custom conf.level and alternative", {
  set.seed(123)
  df1 <- pairwise_contingency_table(
    data = mtcars,
    x = cyl,
    y = am,
    conf.level = 0.90,
    alternative = "greater",
    p.adjust.method = "holm"
  )

  expect_snapshot(select(df1, -expression))
  expect_snapshot(df1[["expression"]])
})

# custom digits --------------------------------------------------

test_that("pairwise_contingency_table works - custom digits", {
  set.seed(123)
  df1 <- pairwise_contingency_table(
    data = mtcars,
    x = cyl,
    y = am,
    digits = 4L,
    p.adjust.method = "BH"
  )

  expect_snapshot(select(df1, -expression))
  expect_snapshot(df1[["expression"]])
})
