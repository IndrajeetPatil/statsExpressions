test_that("within-subjects effect size excludes the row id column", {
  data <- WRS2::WineTasting

  set.seed(123)
  df <- oneway_anova(
    data = data,
    x = Wine,
    y = Taste,
    subject.id = Taster,
    type = "robust",
    paired = TRUE,
    nboot = 100
  )

  set.seed(123)
  ez <- WRS2::wmcpAKP(
    as.data.frame(split(data$Taste, data$Wine)),
    tr = 0.2,
    nboot = 100
  )

  expect_equal(
    c(df$estimate, df$conf.low, df$conf.high),
    unclass(ez),
    ignore_attr = TRUE
  )
  expect_snapshot(select(df, -expression))
  expect_snapshot(df[["expression"]])
})
