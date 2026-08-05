test_that(desc = "centrality description works as expected - no missing data", code = {
  set.seed(123)
  df <- purrr::pmap_dfr(
    .l = list(
      data = list(mtcars),
      x = alist(am),
      y = alist(wt),
      type = list("p", "np", "r", "b"),
      digits = 3L,
      conf.level = list(0.89, 0.99, 0.90, 0.50)
    ),
    .f = centrality_description
  )

  set.seed(123)
  expect_snapshot(select(df, -expression))
  expect_snapshot(df[["expression"]])
})

test_that(desc = "centrality description works as expected - missing data", code = {
  # some MAP estimates are different on macOS compared to windows and linux
  skip_on_os(c("windows", "linux"))

  set.seed(123)
  df_na <- purrr::pmap_dfr(
    .l = list(
      data = list(bugs_long),
      x = alist(gender),
      y = alist(desire),
      type = list("p", "np", "r", "b"),
      digits = 3L,
      conf.level = list(0.89, 0.99, 0.90, 0.50)
    ),
    .f = centrality_description
  )

  set.seed(123)
  expect_snapshot(select(df_na, -expression))
  expect_snapshot(df_na[["expression"]])
})


test_that(desc = "centrality description works when variable is named `variable`", code = {
  df_var <- dplyr::rename(mtcars, variable = cyl)

  set.seed(123)
  res <- suppressWarnings(centrality_description(df_var, variable, wt))

  set.seed(123)
  expect_snapshot(select(res, -expression))
  expect_snapshot(res[["expression"]])
})

test_that("centrality description works with reserved response names", {
  for (response in c("n", "Mean", "SD", "IQR")) {
    data <- data.frame(group = rep(c("a", "b"), each = 3L), value = 1:6)
    names(data)[[2L]] <- response

    result <- rlang::inject(centrality_description(data, group, !!rlang::sym(response)))

    expect_identical(names(result)[1:2], c("group", response))
    expect_identical(result[[response]], c(2, 5))
  }
})
