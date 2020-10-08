context("expr_templates")

# zero parameter -----------------------------------------------------------

testthat::test_that(
  desc = "checking if subtitle template works without any parameter",
  code = {
    testthat::skip_if(getRversion() < "3.6")

    # subtitle
    set.seed(123)
    subtitle <-
      ggplot2::expr(
        paste(
          italic("W"),
          " = ",
          "8.74",
          ", ",
          italic("p"),
          " = ",
          "0.013",
          ", ",
          widehat(italic("r")),
          " = ",
          "0.52",
          ", CI"["99%"],
          " [",
          "0.13",
          ", ",
          "0.93",
          "]",
          ", ",
          italic("n"),
          " = ",
          32
        )
      )

    # creating a dataframe with stats results
    stats_df <- cbind.data.frame(
      statistic = 8.74,
      p.value = 0.013
    )

    # creating a dataframe with effect size results
    effsize_df <- cbind.data.frame(
      estimate = 0.52,
      conf.low = 0.13,
      conf.high = 0.93
    )

    # created using a template maker
    template_0 <-
      statsExpressions::expr_template(
        no.parameters = 0L,
        stats.df = stats_df,
        effsize.df = effsize_df,
        statistic.text = quote(italic("W")),
        conf.level = 0.99,
        k = 2L,
        n = 32,
        effsize.text = quote(widehat(italic("r")))
      )

    # check if they are equivalent
    testthat::expect_identical(subtitle, template_0)
  }
)

# single parameter -----------------------------------------------------------

testthat::test_that(
  desc = "checking if subtitle template works with a single parameter",
  code = {
    testthat::skip_if(getRversion() < "3.6")

    # subtitle
    set.seed(123)
    subtitle <-
      ggplot2::expr(
        paste(
          italic(chi)^2,
          "(",
          "2",
          ") = ",
          "8.74",
          ", ",
          italic("p"),
          " = ",
          "0.013",
          ", ",
          widehat(italic("V")),
          " = ",
          "0.52",
          ", CI"["95%"],
          " [",
          "0.13",
          ", ",
          "0.93",
          "]",
          ", ",
          italic("n"),
          " = ",
          32
        )
      )

    # creating a dataframe with stats results
    stats_df <- cbind.data.frame(
      statistic = 8.74,
      parameter = 2,
      p.value = 0.013
    )

    # creating a dataframe with effect size results
    effsize_df <- cbind.data.frame(
      estimate = 0.52,
      conf.low = 0.13,
      conf.high = 0.93
    )

    # created using a template maker
    template_1 <-
      statsExpressions::expr_template(
        no.parameters = 1L,
        stats.df = stats_df,
        effsize.df = effsize_df,
        statistic.text = quote(italic(chi)^2),
        n = 32,
        effsize.text = quote(widehat(italic("V")))
      )

    # check if they are equivalent
    testthat::expect_identical(subtitle, template_1)
  }
)

# two parameters -----------------------------------------------------------

testthat::test_that(
  desc = "checking if subtitle template works with two parameters",
  code = {
    testthat::skip_if(getRversion() < "3.6")

    # subtitle
    set.seed(123)
    subtitle <-
      ggplot2::expr(
        paste(
          italic("F"),
          "(",
          "3",
          ",",
          "24.05",
          ") = ",
          "2.27",
          ", ",
          italic("p"),
          " = ",
          "0.107",
          ", ",
          widehat(omega["p"]^2),
          " = ",
          "0.00",
          ", CI"["95%"],
          " [",
          "-0.08",
          ", ",
          "0.10",
          "]",
          ", ",
          italic("n"),
          " = ",
          51L
        )
      )

    # creating a dataframe with stats results
    stats_df <- cbind.data.frame(
      statistic = 2.27,
      parameter1 = 3,
      parameter2 = 24.05,
      p.value = 0.107
    )

    # creating a dataframe with effect size results
    effsize_df <- cbind.data.frame(
      estimate = 0.00,
      conf.low = -0.08,
      conf.high = 0.10
    )

    # created using a template maker
    template_1 <-
      statsExpressions::expr_template(
        no.parameters = 2L,
        stats.df = stats_df,
        effsize.df = effsize_df,
        statistic.text = quote(italic("F")),
        conf.level = 0.95,
        k = 2L,
        k.parameter = 0L,
        k.parameter2 = 2L,
        n = 51L,
        effsize.text = quote(widehat(omega["p"]^2))
      )

    # check if they are equivalent
    testthat::expect_identical(subtitle, template_1)
  }
)
