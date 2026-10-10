test_that("print doesn't change global options and returns object invisibly", {
  pillar_options <- c("pillar.width", "pillar.bold", "pillar.subtle_num")
  old_options <- options()[pillar_options]
  df <- contingency_table(mtcars, am)

  capture.output({
    printed <- expect_invisible(print(df))
  })

  expect_identical(options()[pillar_options], old_options)
  expect_identical(printed, df)
})
