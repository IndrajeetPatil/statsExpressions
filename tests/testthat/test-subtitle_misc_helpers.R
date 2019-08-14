# context ------------------------------------------------------------------
context(desc = "long_to_wide_converter")

# long_to_wide_converter works ---------------------------------------------

testthat::test_that(
  desc = "long_to_wide_converter works",
  code = {


    # setup
    set.seed(123)

    # converting to long format
    data_bugs <- bugs_long

    # ----------------------- data without NAs ------------------------------

    # within-subjects
    set.seed(123)
    df1 <- statsExpressions:::long_to_wide_converter(
      data = iris_long,
      x = condition,
      y = value
    )

    testthat::expect_equal(dim(df1), c(150L, 5L))

    # between-subjects
    set.seed(123)
    df2 <- statsExpressions:::long_to_wide_converter(
      data = mtcars,
      x = am,
      y = wt,
      paired = FALSE
    )

    testthat::expect_equal(dim(df2), c(19L, 3L))

    # -------------------------- data with NAs ------------------------------

    # within-subjects
    set.seed(123)
    df3 <- statsExpressions:::long_to_wide_converter(
      data = data_bugs,
      x = condition,
      y = desire
    )

    testthat::expect_equal(dim(df3), c(88L, 5L))

    # between-subjects
    set.seed(123)
    df4 <- statsExpressions:::long_to_wide_converter(
      data = ggplot2::msleep,
      x = vore,
      y = brainwt,
      paired = FALSE
    )

    testthat::expect_equal(dim(df4), c(26L, 5L))
  }
)
