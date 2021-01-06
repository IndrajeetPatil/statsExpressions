
test_that(
  desc = " dataframes for parametric t-tests",
  code = {
    skip_if(getRversion() < "3.6")

    # dataframes for one-sample t-test (with NAs) ---------
    set.seed(123)
    df_1 <-
      purrr::pmap_dfr(
        .l = list(
          data = list(ggplot2::msleep),
          x = list("brainwt"),
          test.value = list(0.25),
          effsize.type = list("d", "g", "d", "g"),
          var.equal = list(TRUE, FALSE, TRUE, FALSE),
          conf.level = list(0.89, 0.99, 0.90, 0.50),
          output = list("dataframe")
        ),
        .f = statsExpressions::expr_t_onesample
      )

    expect_equal(
      dplyr::select(df_1, -term),
      structure(
        list(
          mu = c(0.25, 0.25, 0.25, 0.25),
          statistic = c(
            0.242042658015805,
            0.242042658015805,
            0.242042658015805,
            0.242042658015805
          ),
          df.error = c(55, 55, 55, 55),
          p.value = c(
            0.809647353132935,
            0.809647353132935,
            0.809647353132935,
            0.809647353132935
          ),
          method = c(
            "One Sample t-test",
            "One Sample t-test",
            "One Sample t-test",
            "One Sample t-test"
          ),
          estimate = c(
            0.0323443106885441,
            0.031901237939386,
            0.0323443106885441,
            0.031901237939386
          ),
          ci.width = c(
            0.89,
            0.99, 0.9, 0.5
          ),
          conf.low = c(
            -0.183068375979454,
            -0.310613406479536,
            -0.189361765654486,
            -0.0576821386753824
          ),
          conf.high = c(
            0.248046511784345,
            0.374701843243403,
            0.254339866355058,
            0.121770038540287
          )
        ),
        row.names = c(NA, -4L),
        class = c("tbl_df", "tbl", "data.frame")
      ),
      tolerance = 0.001
    )

    # dataframes for parametric t-test (between-subjects without NAs) ---------
    set.seed(123)
    df_2_between <-
      purrr::pmap_dfr(
        .l = list(
          data = list(mtcars),
          x = list("am"),
          y = list("wt"),
          effsize.type = list("d", "g", "d", "g"),
          var.equal = list(TRUE, FALSE, TRUE, FALSE),
          conf.level = list(0.89, 0.99, 0.90, 0.50),
          output = list("dataframe")
        ),
        .f = statsExpressions::expr_t_parametric
      )

    expect_equal(
      df_2_between,
      structure(
        list(
          term = c("wt", "wt", "wt", "wt"),
          group = c(
            "am",
            "am", "am", "am"
          ),
          mean.group1 = c(
            3.76889473684211,
            3.76889473684211,
            3.76889473684211,
            3.76889473684211
          ),
          mean.group2 = c(
            2.411, 2.411,
            2.411, 2.411
          ),
          statistic = c(
            5.25760304439422,
            5.49390493921009,
            5.25760304439422,
            5.49390493921009
          ),
          df.error = c(
            30, 29.2335155632247,
            30, 29.2335155632247
          ),
          p.value = c(
            1.12543955234246e-05,
            1.12543955234246e-05,
            1.12543955234246e-05,
            1.12543955234246e-05
          ),
          method = c(
            "Two Sample t-test",
            "Welch Two Sample t-test",
            "Two Sample t-test",
            "Welch Two Sample t-test"
          ),
          estimate = c(
            1.89240595391238,
            1.84469824078854,
            1.89240595391238,
            1.84469824078854
          ),
          ci.width = c(0.89, 0.99, 0.9, 0.5),
          conf.low = c(
            1.18500335249366,
            0.747883380823306,
            1.16498891945519,
            1.54285021768194
          ),
          conf.high = c(
            2.57362806393695,
            2.92929408313073,
            2.59414985250707,
            2.11415288387047
          )
        ),
        row.names = c(
          NA,
          -4L
        ),
        class = c("tbl_df", "tbl", "data.frame")
      ),
      tolerance = 0.001
    )

    # dataframes for parametric t-test (within-subjects with NAs) ---------
    set.seed(123)
    df_2_within <-
      purrr::pmap_dfr(
        .l = list(
          data = list(dplyr::filter(bugs_long, condition %in% c("HDHF", "HDLF"))),
          x = list("condition"),
          y = list("desire"),
          paired = list(TRUE),
          effsize.type = list("d", "g", "d", "g"),
          var.equal = list(TRUE, FALSE, TRUE, FALSE),
          conf.level = list(0.89, 0.99, 0.90, 0.50),
          output = list("dataframe")
        ),
        .f = statsExpressions::expr_t_parametric
      )

    expect_equal(
      df_2_within,
      structure(
        list(
          term = c("desire", "desire", "desire", "desire"),
          group = c("condition", "condition", "condition", "condition"),
          statistic = c(
            3.61338634142544,
            3.61338634142544,
            3.61338634142544,
            3.61338634142544
          ),
          df.error = c(89, 89, 89, 89),
          p.value = c(
            0.00049996105275453,
            0.00049996105275453,
            0.00049996105275453,
            0.00049996105275453
          ),
          method = c(
            "Paired t-test",
            "Paired t-test",
            "Paired t-test",
            "Paired t-test"
          ),
          estimate = c(
            0.380884363501574,
            0.377665622401561,
            0.380884363501574,
            0.377665622401561
          ),
          ci.width = c(
            0.89, 0.99,
            0.9, 0.5
          ),
          conf.low = c(
            0.206487637741569,
            0.0983971798295905,
            0.201367635364937,
            0.305264348714117
          ),
          conf.high = c(
            0.557476445259069,
            0.659309890984744,
            0.562603847478034,
            0.452141340639216
          )
        ),
        row.names = c(
          NA,
          -4L
        ),
        class = c("tbl_df", "tbl", "data.frame")
      ),
      tolerance = 0.001
    )
  }
)
