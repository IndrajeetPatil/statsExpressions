withr::local_options(list(tibble.width = Inf))

test_that(
  desc = "bayesian (independent samples t-test)",
  code = {
    # bayesian (independent samples t-test) ----------------------

    # expression
    set.seed(123)
    df <-
      two_sample_test(
        type = "bayes",
        data = ToothGrowth,
        x = supp,
        y = len,
        paired = FALSE,
        conf.level = 0.99
      )

    # call
    expect_snapshot(df[["expression"]])
  }
)

test_that(
  desc = "bayesian (paired t-test)",
  code = {
    # Bayes factor (paired t-test) ---------------------------------------------

    dat_tidy <- dplyr::filter(bugs_long, condition %in% c("HDLF", "HDHF"))

    # extracting results from where this function is implemented
    set.seed(123)
    df_results <-
      two_sample_test(
        type = "bayes",
        data = dat_tidy,
        x = "condition",
        y = desire,
        paired = TRUE,
        bf.prior = 0.8
      )

    expect_equal(df_results$bf10[[1]], 40.36079, tolerance = 0.001)

    set.seed(123)
    df <-
      two_sample_test(
        type = "bayes",
        data = dat_tidy,
        x = "condition",
        y = desire,
        paired = TRUE,
        bf.prior = 0.8
      )

    expect_snapshot(df[["expression"]])
  }
)

test_that(
  desc = "works with subject id",
  code = {
    # works with subject id -----------------------------------------------

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
        type = "bayes",
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
        type = "bayes",
        data = arrange(df, id),
        x = condition,
        y = score,
        paired = TRUE
      )

    expect_equal(expr1, expr2, ignore_attr = TRUE)
  }
)
