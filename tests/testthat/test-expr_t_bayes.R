# between-subjects design -----------------------------------------------

testthat::test_that(
  desc = "expr_t_bayes works - between-subjects design",
  code = {
    testthat::skip_if(getRversion() < "3.6")

    # statsExpressions output
    set.seed(123)
    using_function <-
      statsExpressions::expr_t_bayes(
        data = dplyr::filter(movies_long, genre == "Action" | genre == "Drama"),
        x = "genre",
        y = rating,
        bf.prior = .9,
        paired = FALSE,
        k = 5
      )

    set.seed(123)
    results <-
      tidyBF::bf_ttest(
        data = dplyr::filter(movies_long, genre == "Action" | genre == "Drama"),
        x = "genre",
        y = rating,
        bf.prior = .9,
        paired = FALSE,
        k = 5,
        output = "h1"
      )$expr

    # testing overall call
    testthat::expect_identical(using_function, results)
  }
)

testthat::test_that(
  desc = "expr_t_bayes works - between-subjects design - with NA",
  code = {
    testthat::skip_if(getRversion() < "3.6")

    # statsExpressions output
    set.seed(123)
    using_function <-
      statsExpressions::expr_t_bayes(
        data = dplyr::filter(ggplot2::msleep, vore %in% c("omni", "carni")),
        x = vore,
        y = bodywt,
        paired = FALSE,
        bf.prior = 0.8,
        k = 4,
        messages = FALSE
      )

    # expected output
    set.seed(123)
    results <-
      tidyBF::bf_ttest(
        data = dplyr::filter(ggplot2::msleep, vore %in% c("omni", "carni")),
        x = vore,
        y = bodywt,
        paired = FALSE,
        bf.prior = 0.8,
        k = 4,
        messages = FALSE,
        output = "h1"
      )$expr

    # testing overall call
    testthat::expect_identical(using_function, results)
  }
)

# within-subjects design -----------------------------------------------

testthat::test_that(
  desc = "expr_t_bayes_paired works - within-subjects design",
  code = {
    testthat::skip_if(getRversion() < "3.6")

    # made up data
    Input <- ("
              Bird   Typical  Odd
              A     -0.255   -0.324
              B     -0.213   -0.185
              C     -0.190   -0.299
              D     -0.185   -0.144
              E     -0.045   -0.027
              F     -0.025   -0.039
              G     -0.015   -0.264
              H      0.003   -0.077
              I      0.015   -0.017
              J      0.020   -0.169
              K      0.023   -0.096
              L      0.040   -0.330
              M      0.040   -0.346
              N      0.050   -0.191
              O      0.055   -0.128
              P      0.058   -0.182
              ")

    # creating a dataframe
    df_bird <- read.table(textConnection(Input), header = TRUE)

    # converting to long format
    df_bird %<>%
      as_tibble(x = .) %>%
      tidyr::gather(
        data = .,
        key = "type",
        value = "length",
        Typical:Odd
      )

    # statsExpressions output
    set.seed(123)
    using_function <-
      statsExpressions::expr_t_bayes(
        data = df_bird,
        x = type,
        y = "length",
        bf.prior = 0.6,
        k = 5,
        paired = TRUE,
        messages = FALSE
      )

    # expected output
    set.seed(123)
    results <-
      tidyBF::bf_ttest(
        data = df_bird,
        x = type,
        y = "length",
        bf.prior = 0.6,
        k = 5,
        paired = TRUE,
        messages = FALSE,
        output = "h1"
      )$expr

    # testing overall call
    testthat::expect_identical(using_function, results)
  }
)

testthat::test_that(
  desc = "expr_t_bayes_paired works - within-subjects design - with NA",
  code = {
    testthat::skip_if(getRversion() < "3.6")

    # statsExpressions output
    set.seed(123)
    using_function <-
      statsExpressions::expr_t_bayes(
        data = dplyr::filter(statsExpressions::bugs_long, condition %in% c("LDLF", "HDLF")),
        x = condition,
        y = desire,
        bf.prior = 0.77,
        k = 4,
        paired = TRUE,
        messages = FALSE
      )

    # expected output
    set.seed(123)
    results <-
      tidyBF::bf_ttest(
        data = dplyr::filter(statsExpressions::bugs_long, condition %in% c("LDLF", "HDLF")),
        x = condition,
        y = desire,
        bf.prior = 0.77,
        k = 4,
        paired = TRUE,
        messages = FALSE,
        output = "h1"
      )$expr

    # testing overall call
    testthat::expect_identical(using_function, results)
  }
)


# dataframe -----------------------------------------------------------

testthat::test_that(
  desc = "dataframe",
  code = {
    testthat::expect_is(
      statsExpressions::expr_t_bayes(
        data = dplyr::filter(movies_long, genre == "Action" | genre == "Drama"),
        x = "genre",
        y = rating,
        output = "dataframe"
      ),
      "tbl_df"
    )
  }
)
