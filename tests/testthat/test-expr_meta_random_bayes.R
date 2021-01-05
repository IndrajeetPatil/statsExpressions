# subtitle from meta-analysis -------------------------------------------

test_that(
  desc = "expr_meta_random works",
  code = {
    skip_if(getRversion() < "3.6")

    # setup
    set.seed(123)

    # creating a dataframe
    df1 <-
      structure(
        .Data = list(
          estimate = c(
            1.5, -0.221, 0.14, 0.161, -0.181, 1.54, 0.847, 1.008, 1.008,
            -0.221, 0.473, 0.283, 0.201, 0.855, 0.772, 0.06, -0.408, 0.494,
            0.43, 1.805, 0.926, 0.516, 0.262, 0.631, 0.008, -0.046, 0.126,
            0.772, -0.368, 0.464, 0.425, 0.57, 0.453, -0.427, -0.079, 0.142,
            0.915, 0.542
          ),
          std.error = c(
            0.44, 0.44, 0.25, 0.21, 0.33, 1.13, 0.66, 0.42, 0.62, 0.34,
            0.36, 0.67, 0.14, 0.23, 0.43, 0.4, 0.38, 0.26, 0.21, 0.29, 0.24,
            0.17, 0.33, 0.33, 0.17, 0.2, 0.2, 0.29, 0.2, 0.25, 0.33, 0.16,
            0.16, 0.26, 0.25, 0.26, 0.33, 0.22
          )
        ),
        row.names = c(NA, -10L),
        class = c("tbl_df", "tbl", "data.frame")
      )

    # subtitle
    set.seed(123)
    results1 <-
      suppressWarnings(expr_meta_random(
        data = df1,
        type = "bayes",
        k = 3,
        iter = 1000
      ))

    # subtitle
    set.seed(123)
    df <-
      suppressWarnings(expr_meta_random(
        data = df1,
        type = "bayes",
        iter = 1000,
        output = "dataframe"
      ))

    # test
    expect_identical(as.character(results1)[5], "-7.513")
    expect_s3_class(df, "tbl_df")
  }
)
