# t1way_ci works ------------------------------------------------------

testthat::test_that(
  desc = "t1way_ci works",
  code = {
    testthat::skip_if(getRversion() < "3.6")

    # normal
    set.seed(123)
    df1 <-
      statsExpressions:::t1way_ci(
        data = dplyr::filter(.data = ggplot2::msleep, vore != "insecti"),
        x = vore,
        y = brainwt,
        nboot = 10,
        conf.level = 0.99,
        tr = 0.05,
        conf.type = c("norm")
      )

    # percentile
    set.seed(123)
    df2 <-
      suppressWarnings(statsExpressions:::t1way_ci(
        data = dplyr::filter(.data = ggplot2::msleep, vore != "insecti"),
        x = vore,
        y = brainwt,
        tr = 0.1,
        nboot = 10,
        conf.level = 0.99,
        conf.type = "perc"
      ))

    # test normal CI
    testthat::expect_equal(df1$estimate, 0.5423793, tolerance = 0.00002)
    testthat::expect_equal(df1$conf.low, -0.5290579, tolerance = 0.00002)
    testthat::expect_equal(df1$conf.high, 1.877737, tolerance = 0.00002)
    testthat::expect_equal(df1$statistic, 0.6146867, tolerance = 0.00002)
    testthat::expect_equal(df1$p.value, 0.5487093, tolerance = 0.00002)

    # test percentile CI
    testthat::expect_equal(df2$estimate, 1.295478, tolerance = 0.00002)
    testthat::expect_equal(df2$conf.low, 0.3947649, tolerance = 0.00002)
    testthat::expect_equal(df2$conf.high, 1.887348, tolerance = 0.00002)
    testthat::expect_equal(df2$statistic, 0.260884, tolerance = 0.00002)
    testthat::expect_equal(df2$p.value, 0.772501, tolerance = 0.00002)
  }
)

# test_yuend_ci works ---------------------------------------------------------

context("test_yuend_ci")

testthat::test_that(
  desc = "Yuen's test on trimmed means for dependent samples works",
  code = {
    testthat::skip_if(getRversion() < "3.6")

    # ggstatsplot output
    set.seed(123)
    df1 <-
      statsExpressions:::yuend_ci(
        data = mtcars,
        x = am,
        y = wt,
        nboot = 25
      )

    # creating a dataframe with NAs
    mydata1 <- dplyr::filter(ggplot2::msleep, vore %in% c("omni", "herbi"))

    # creating a dataframe
    set.seed(123)
    df2 <-
      statsExpressions:::yuend_ci(
        data = mydata1,
        x = vore,
        y = brainwt,
        conf.level = 0.90,
        conf.type = "basic"
      )

    # testing (dataframe without NAs)
    testthat::expect_equal(df1$statistic, 6.082871, tolerance = 0.001)
    testthat::expect_equal(df1$estimate, 0.7881904, tolerance = 0.0001)
    testthat::expect_equal(df1$conf.low, 0.5830343, tolerance = 0.0001)
    testthat::expect_equal(df1$conf.high, 0.8415761, tolerance = 0.001)
    testthat::expect_equal(df1$parameter, 10L)
    testthat::expect_equal(df1$p.value, 0.0001183419, tolerance = 0.00001)

    # testing (dataframe with NAs)
    testthat::expect_equal(df2$statistic, 0.8617648, tolerance = 0.001)
    testthat::expect_equal(df2$estimate, 0.9279336, tolerance = 0.0001)
    testthat::expect_equal(df2$conf.low, 0.9642619, tolerance = 0.0001)
    testthat::expect_equal(df2$conf.high, 1.798206, tolerance = 0.001)
    testthat::expect_equal(df2$parameter, 14L)
    testthat::expect_equal(df2$p.value, 0.4033365, tolerance = 0.000001)
  }
)
