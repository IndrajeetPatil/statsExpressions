test_that(
  desc = "contingency_table works",
  code = {
    options(tibble.width = Inf)

    # contingency tab - without NAs ---------------------------------

    # `{statsExpressions}` output
    set.seed(123)
    df1 <- suppressWarnings(contingency_table(
      data = mtcars,
      x = am,
      y = cyl,
      k = 5,
      conf.level = 0.99
    ))

    # testing all details
    set.seed(123)
    expect_snapshot(select(df1, -expression))
    expect_snapshot(as.character(df1$expression[[1]]))

    # with counts
    set.seed(123)
    df2 <- contingency_table(
      data = as.data.frame(Titanic),
      x = names(as.data.frame(Titanic))[2],
      y = Survived,
      counts = Freq
    )

    # testing all details
    set.seed(123)
    expect_snapshot(select(df2, -expression))
    expect_snapshot(as.character(df2$expression[[1]]))

    # contingency tab - with NAs --------------------------------------

    # introduce NAs
    set.seed(123)
    df3 <- suppressWarnings(contingency_table(
      data = ggplot2::msleep,
      x = vore,
      y = conservation,
      conf.level = 0.990
    ))

    # testing all details
    set.seed(123)
    expect_snapshot(select(df3, -expression))
    expect_snapshot(as.character(df3$expression[[1]]))
  }
)

test_that(
  desc = "paired contingency_table works ",
  code = {
    # paired data - without NAs and counts data ----------------------------

    # create data structure
    paired_data <-
      structure(
        list(
          response_before =
            structure(
              c(1L, 2L, 1L, 2L),
              .Label = c("no", "yes"),
              class = "factor"
            ),
          response_after = structure(
            c(1L, 1L, 2L, 2L),
            .Label = c("no", "yes"),
            class = "factor"
          ),
          Freq = c(65L, 25L, 5L, 5L)
        ),
        class = "data.frame",
        row.names = c(NA, -4L)
      )

    # `{statsExpressions}` output
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

    # testing all details
    set.seed(123)
    expect_snapshot(select(df1, -expression))
    expect_snapshot(as.character(df1$expression[[1]]))

    # paired data with NAs  ---------------------------------------------

    # expanding the dataframe
    paired_data %<>% tidyr::uncount(weights = Freq)

    # introduce NAs
    # check that 2-by-2 doesn't produce continuity correction
    set.seed(123)
    paired_data[1, 1] <- NA
    paired_data[12, 1] <- NA
    paired_data[22, 1] <- NA
    paired_data[24, 1] <- NA
    paired_data[65, 1] <- NA

    # `{statsExpressions}` output
    set.seed(123)
    df2 <- suppressWarnings(
      contingency_table(
        data = paired_data,
        x = response_before,
        y = response_after,
        paired = TRUE,
        k = 3,
        conf.level = 0.90
      )
    )

    # testing all details
    set.seed(123)
    expect_snapshot(select(df2, -expression))
    expect_snapshot(as.character(df2$expression[[1]]))
  }
)

test_that(
  desc = "Goodness of Fit contingency_table works without counts",
  code = {
    # one-sample test (without NAs) -------------------------------------

    # `{statsExpressions}` output
    set.seed(123)
    df1 <- suppressWarnings(contingency_table(
      data = mtcars,
      x = am,
      conf.level = 0.99,
      k = 5
    ))

    # testing all details
    set.seed(123)
    expect_snapshot(select(df1, -expression))
    expect_snapshot(as.character(df1$expression[[1]]))

    # with counts
    set.seed(123)
    df2 <- contingency_table(
      data = as.data.frame(Titanic),
      x = Sex,
      counts = Freq
    )

    # testing all details
    set.seed(123)
    expect_snapshot(select(df2, -expression))
    expect_snapshot(as.character(df2$expression[[1]]))

    # one-sample test (with NAs) -------------------------------------

    # from function
    set.seed(123)
    df3 <- contingency_table(
      data = ggplot2::msleep,
      x = vore,
      ratio = c(0.2, 0.2, 0.3, 0.3)
    )

    # testing all details
    set.seed(123)
    expect_snapshot(select(df3, -expression))
    expect_snapshot(as.character(df3$expression[[1]]))
  }
)

test_that(
  desc = "bayes factor (proportion test)",
  code = {
    # bayes factor (proportion test) --------------------------------------

    # extracting results from where this function is implemented
    set.seed(123)
    df1 <- contingency_table(
      data = mtcars,
      x = am,
      type = "bayes"
    )

    expect_snapshot(select(df1, -expression))
    expect_snapshot(df1$expression[[1]])

    # expr
    set.seed(123)
    df2 <- contingency_table(
      type = "bayes",
      data = mtcars,
      x = cyl,
      prior.concentration = 10,
      top.text = "duh"
    )

    expect_snapshot(select(df2, -expression))
    expect_snapshot(df2$expression[[1]])
  }
)

test_that(
  desc = "bayes factor (contingency tab)",
  code = {
    # bayes factor (contingency tab) --------------------------------------

    # extracting results from where this function is implemented
    set.seed(123)
    df_results <- contingency_table(
      type = "bayes",
      data = mtcars,
      x = am,
      y = cyl,
      sampling.plan = "jointMulti",
      fixed.margin = "rows"
    )

    # objects
    expect_identical(class(df_results), c("tbl_df", "tbl", "data.frame"))

    # check bayes factor values
    expect_equal(df_results$bf10[[1]], 28.07349, tolerance = 0.001)

    # expr
    set.seed(123)
    expr_text1 <- contingency_table(
      type = "bayes",
      data = mtcars,
      x = colnames(mtcars)[9],
      y = cyl,
      sampling.plan = "jointMulti",
      fixed.margin = "rows",
      conf.level = 0.89,
      k = 3L
    )

    # with counts
    set.seed(123)
    expr_text2 <- contingency_table(
      data = as.data.frame(Titanic),
      x = Survived,
      y = colnames(as.data.frame(Titanic))[2],
      counts = Freq,
      sampling.plan = "jointMulti",
      fixed.margin = "rows",
      conf.level = 0.99,
      type = "bayes",
      k = 3L
    )

    # with counts
    set.seed(123)
    expr_text3 <- contingency_table(
      data = as.data.frame(Titanic),
      x = Survived,
      y = Sex,
      counts = Freq,
      k = 3L,
      type = "bayes",
      prior.concentration = 1.5
    )

    # expr text
    expect_snapshot(list(
      as.character(expr_text1$expression[[1]]),
      as.character(expr_text2$expression[[1]]),
      as.character(expr_text3$expression[[1]])
    ))
  }
)
