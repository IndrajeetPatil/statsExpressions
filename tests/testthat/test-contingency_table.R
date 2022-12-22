withr::local_options(list(tibble.width = Inf))

test_that(
  desc = "contingency_table works",
  code = {
    # contingency tab - without NAs ---------------------------------

    set.seed(123)
    df1 <- suppressWarnings(contingency_table(
      data = mtcars,
      x = am,
      y = cyl,
      k = 5L,
      conf.level = 0.99
    ))

    set.seed(123)
    expect_snapshot(select(df1, -expression))
    expect_snapshot(df1[["expression"]])

    # with counts
    set.seed(123)
    df2 <- contingency_table(
      data = as.data.frame(Titanic),
      x = Sex,
      y = Survived,
      counts = Freq
    )

    set.seed(123)
    expect_snapshot(select(df2, -expression))
    expect_snapshot(df2[["expression"]])

    # contingency tab - with NAs --------------------------------------

    # introduce NAs
    set.seed(123)
    df3 <- suppressWarnings(contingency_table(
      data = msleep,
      x = vore,
      y = conservation,
      conf.level = 0.990
    ))

    set.seed(123)
    expect_snapshot(select(df3, -expression))
    expect_snapshot(df3[["expression"]])
  }
)

test_that(
  desc = "paired contingency_table works ",
  code = {
    # paired data - without NAs and counts data ----------------------------

    paired_data <-
      structure(
        list(
          response_before = structure(c(1L, 2L, 1L, 2L), levels = c("no", "yes"), class = "factor"),
          response_after = structure(c(1L, 1L, 2L, 2L), levels = c("no", "yes"), class = "factor"),
          Freq = c(65L, 25L, 5L, 5L)
        ),
        class = c("tbl_df", "tbl", "data.frame"),
        row.names = c(NA, -4L)
      )

    set.seed(123)
    df1 <- suppressWarnings(
      contingency_table(
        data = paired_data,
        x = response_before,
        y = response_after,
        paired = TRUE,
        counts = Freq,
        k = 5
      )
    )

    set.seed(123)
    expect_snapshot(select(df1, -expression))
    expect_snapshot(df1[["expression"]])

    # paired data with NAs  ---------------------------------------------

    # untabling the data frame
    paired_data %<>% tidyr::uncount(weights = Freq)

    # deliberately introduce NAs
    set.seed(123)
    paired_data[1, 1] <- NA
    paired_data[12, 1] <- NA
    paired_data[22, 1] <- NA
    paired_data[24, 1] <- NA
    paired_data[65, 1] <- NA

    set.seed(123)
    df2 <- suppressWarnings(
      contingency_table(
        data = paired_data,
        x = response_before,
        y = response_after,
        paired = TRUE,
        k = 3L,
        conf.level = 0.90
      )
    )


    set.seed(123)
    expect_snapshot(select(df2, -expression))
    expect_snapshot(df2[["expression"]])
  }
)

test_that(
  desc = "Goodness of Fit contingency_table works without counts",
  code = {
    # one-sample test (without NAs) -------------------------------------

    set.seed(123)
    df1 <- suppressWarnings(contingency_table(
      data = mtcars,
      x = am,
      conf.level = 0.99,
      k = 5
    ))

    set.seed(123)
    expect_snapshot(select(df1, -expression))
    expect_snapshot(df1[["expression"]])

    # with counts
    set.seed(123)
    df2 <- contingency_table(
      data = as.data.frame(Titanic),
      x = Sex,
      counts = Freq
    )

    set.seed(123)
    expect_snapshot(select(df2, -expression))
    expect_snapshot(df2[["expression"]])

    # one-sample test (with NAs) -------------------------------------

    set.seed(123)
    df3 <- contingency_table(
      data = msleep,
      x = vore,
      ratio = c(0.2, 0.2, 0.3, 0.3)
    )

    set.seed(123)
    expect_snapshot(select(df3, -expression))
    expect_snapshot(df3[["expression"]])

    # edge case
    expect_null(contingency_table(data.frame(x = "x"), x, type = "bayes"))
  }
)

test_that(
  desc = "bayesian (proportion test)",
  code = {
    # bayesian (proportion test) --------------------------------------

    set.seed(123)
    df1 <- contingency_table(
      data = mtcars,
      x = am,
      type = "bayes"
    )

    expect_snapshot(select(df1, -expression))
    expect_snapshot(df1[["expression"]])

    set.seed(123)
    df2 <- contingency_table(
      type = "bayes",
      data = mtcars,
      x = cyl,
      prior.concentration = 10
    )

    expect_snapshot(select(df2, -expression))
    expect_snapshot(df2[["expression"]])
  }
)

test_that(
  desc = "bayesian (contingency tab)",
  code = {
    # without NAs
    set.seed(123)
    df1 <- contingency_table(
      type = "bayes",
      data = mtcars,
      x = am,
      y = cyl
    )

    expect_snapshot(df1[["expression"]])

    # with NAs
    set.seed(123)
    df2 <- contingency_table(
      type = "bayes",
      data = msleep,
      x = vore,
      y = conservation
    )

    expect_snapshot(df2[["expression"]])
  }
)
