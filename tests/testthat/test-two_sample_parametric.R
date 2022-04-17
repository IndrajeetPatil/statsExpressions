test_that(
  desc = "parametric t-test works (between-subjects without NAs)",
  code = {
    # parametric t-test (between-subjects without NAs) ----------------------

    options(tibble.width = Inf)

    # `statsExpressions` output
    set.seed(123)
    df1 <- suppressWarnings(
      two_sample_test(
        ToothGrowth,
        x = supp,
        y = len,
        effsize.type = "d",
        var.equal = TRUE,
        conf.level = 0.99,
        k = 5
      )
    )

    # testing all details
    set.seed(123)
    expect_snapshot(select(df1, -expression))
    expect_snapshot(unlist(df1$expression[[1]]))
  }
)

test_that(
  desc = "parametric t-test works (between-subjects with NAs)",
  code = {
    # parametric t-test (between-subjects with NAs) --------------------------

    # `statsExpressions` output
    set.seed(123)
    df1 <- suppressWarnings(
      two_sample_test(
        ToothGrowth,
        x = supp,
        y = len,
        effsize.type = "g",
        var.equal = FALSE,
        conf.level = 0.90,
        k = 3
      )
    )

    # testing all details
    set.seed(123)
    expect_snapshot(select(df1, -expression))
    expect_snapshot(unlist(df1$expression[[1]]))
  }
)

test_that(
  desc = "parametric t-test works (within-subjects without NAs)",
  code = {
    # parametric t-test (within-subjects without NAs) -----------------------

    # output from `statsExpressions` helper subtitle
    set.seed(123)
    df1 <- suppressWarnings(two_sample_test(
      data = filter(
        iris_long,
        condition %in% c("Sepal.Length", "Sepal.Width")
      ),
      x = condition,
      y = value,
      paired = TRUE,
      effsize.type = "g",
      k = 4,
      conf.level = 0.50
    ))

    # testing all details
    set.seed(123)
    expect_snapshot(select(df1, -expression))
    expect_snapshot(unlist(df1$expression[[1]]))
  }
)


test_that(
  desc = "parametric t-test works (within-subjects with NAs)",
  code = {
    # parametric t-test (within-subjects with NAs) ---------------------------

    # output from `statsExpressions` helper subtitle
    set.seed(123)
    df1 <- two_sample_test(
      data = filter(bugs_long, condition %in% c("HDHF", "HDLF")),
      x = condition,
      y = desire,
      paired = TRUE,
      effsize.type = "d",
      k = 3
    )

    # testing all details
    set.seed(123)
    expect_snapshot(select(df1, -expression))
    expect_snapshot(unlist(df1$expression[[1]]))
  }
)

test_that(
  desc = "works with subject id",
  code = {
    # works with subject id ------------------------------------------------

    # data
    df <-
      structure(list(
        score = c(
          70, 82.5, 97.5, 100, 52.5, 62.5,
          92.5, 70, 90, 92.5, 90, 75, 60, 90, 85, 67.5, 90, 72.5, 45, 60,
          72.5, 80, 100, 100, 97.5, 95, 65, 87.5, 90, 62.5, 100, 100, 97.5,
          100, 97.5, 95, 82.5, 82.5, 40, 92.5, 85, 72.5, 35, 27.5, 82.5
        ), condition = structure(c(
          5L, 1L, 2L, 3L, 4L, 4L, 5L, 1L,
          2L, 3L, 2L, 3L, 3L, 4L, 2L, 1L, 5L, 5L, 4L, 1L, 1L, 4L, 3L, 5L,
          2L, 5L, 1L, 2L, 3L, 4L, 4L, 5L, 1L, 2L, 3L, 2L, 3L, 4L, 1L, 5L,
          3L, 2L, 5L, 4L, 1L
        ), .Label = c("1", "2", "3", "4", "5"), class = "factor"),
        id = structure(c(
          1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L,
          2L, 3L, 3L, 4L, 3L, 4L, 3L, 4L, 3L, 4L, 4L, 5L, 5L, 5L, 5L,
          5L, 6L, 6L, 6L, 6L, 6L, 7L, 7L, 7L, 7L, 7L, 8L, 8L, 8L, 8L,
          8L, 9L, 9L, 9L, 9L, 9L
        ), .Label = c(
          "1", "2", "3", "4", "5",
          "6", "7", "8", "9"
        ), class = "factor")
      ), row.names = c(
        NA,
        45L
      ), class = "data.frame")

    df <- filter(df, condition %in% c(1, 5))

    # incorrect
    set.seed(123)
    expr1 <-
      two_sample_test(
        data = df,
        x = condition,
        y = score,
        subject.id = id,
        paired = TRUE
      )

    # correct
    set.seed(123)
    expr2 <-
      two_sample_test(
        data = arrange(df, id),
        x = condition,
        y = score,
        paired = TRUE
      )

    expect_equal(expr1, expr2, ignore_attr = TRUE)
  }
)
