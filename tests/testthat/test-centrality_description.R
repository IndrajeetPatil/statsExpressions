test_that(
  desc = "centrality description works as expected",
  code = {
    options(tibble.width = Inf)

    # data without NAs
    set.seed(123)
    df <- purrr::pmap_dfr(
      .l = list(
        data = list(iris),
        x = alist(Species),
        y = alist(Sepal.Length),
        type = list("p", "np", "r", "b"),
        k = list(2L, 3L, 3L, 2L),
        conf.level = list(0.89, 0.99, 0.90, 0.50)
      ),
      .f = statsExpressions::centrality_description
    )

    set.seed(123)
    expect_snapshot(select(df, -expression))
    expect_snapshot(df$expression)

    # data with NAs
    set.seed(123)
    df_na <- purrr::pmap_dfr(
      .l = list(
        data = list(bugs_long),
        x = alist(condition),
        y = alist(desire),
        type = list("p", "np", "r", "b"),
        k = list(2L, 3L, 3L, 2L),
        conf.level = list(0.89, 0.99, 0.90, 0.50)
      ),
      .f = statsExpressions::centrality_description
    )

    set.seed(123)
    expect_snapshot(select(df_na, -expression))
    expect_snapshot(df_na$expression)
  }
)
