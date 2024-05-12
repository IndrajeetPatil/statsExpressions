test_that(
  desc = " parametric t-tests",
  code = {
    skip_if_not(getRversion() >= "4.4.0")

    # one-sample t-test (with NAs) ---------

    set.seed(123)
    df_1 <- purrr::pmap_dfr(
      .l = list(
        data = list(msleep),
        x = list("brainwt"),
        test.value = list(0.25),
        effsize.type = list("d", "g", "d", "g"),
        alternative = c("two.sided", "less", "greater", "two.sided"),
        var.equal = list(TRUE, FALSE, TRUE, FALSE),
        conf.level = list(0.89, 0.99, 0.90, 0.50)
      ),
      .f = one_sample_test
    )

    set.seed(123)
    expect_snapshot(df_1)

    # parametric t-test (between-subjects without NAs) ---------

    set.seed(123)
    df_2_between <- purrr::pmap_dfr(
      .l = list(
        data = list(mtcars),
        x = list("am"),
        y = list("wt"),
        effsize.type = list("d", "g", "d", "g"),
        var.equal = list(TRUE, FALSE, TRUE, FALSE),
        alternative = c("two.sided", "less", "greater", "two.sided"),
        conf.level = list(0.89, 0.99, 0.90, 0.50)
      ),
      .f = two_sample_test
    )

    set.seed(123)
    expect_snapshot(df_2_between)

    # parametric t-test (within-subjects with NAs) ---------

    set.seed(123)
    df_2_within <- purrr::pmap_dfr(
      .l = list(
        data = list(filter(bugs_long, condition %in% c("HDHF", "HDLF"))),
        x = list("condition"),
        y = list("desire"),
        paired = list(TRUE),
        effsize.type = list("d", "g", "d", "g"),
        var.equal = list(TRUE, FALSE, TRUE, FALSE),
        conf.level = list(0.89, 0.99, 0.90, 0.50)
      ),
      .f = two_sample_test
    )

    set.seed(123)
    expect_snapshot(df_2_within)

    # parametric ANOVA (within-subjects with NAs) ---------

    set.seed(123)
    df_3_between <- purrr::pmap_dfr(
      .l = list(
        data = list(msleep),
        x = list("vore"),
        y = list("sleep_rem"),
        effsize.type = list("eta", "omega", "eta", "omega"),
        var.equal = list(TRUE, FALSE, TRUE, FALSE),
        conf.level = list(0.89, 0.80, 0.90, 0.50)
      ),
      .f = oneway_anova
    )

    set.seed(123)
    expect_snapshot(df_3_between)

    # parametric ANOVA (within-subjects with NAs) ---------

    set.seed(123)
    df_3_within <- purrr::pmap_dfr(
      .l = list(
        data = list(bugs_long),
        x = list("condition"),
        y = list("desire"),
        paired = list(TRUE),
        effsize.type = list("eta", "omega"),
        conf.level = list(0.89, 0.90)
      ),
      .f = oneway_anova
    )

    set.seed(123)
    expect_snapshot(df_3_within)
  }
)
