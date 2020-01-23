context("helpers_bf_tests")

# bayes factor (correlation) --------------------------

testthat::test_that(
  desc = "bayes factor (correlation)",
  code = {
    testthat::skip_if(getRversion() < "3.6")

    # creating a dataframe
    set.seed(123)
    df <- suppressMessages(statsExpressions::bf_extractor(
      BayesFactor::correlationBF(
        x = ggplot2::msleep$brainwt,
        y = ggplot2::msleep$sleep_total
      )
    ))

    # check bayes factor values
    testthat::expect_equal(df$bf10, 8.990505, tolerance = 0.001)
    testthat::expect_equal(df$log_e_bf10, 2.196169, tolerance = 0.001)
    testthat::expect_equal(df$log_e_bf10, -df$log_e_bf01, tolerance = 0.001)
    testthat::expect_equal(df$log_10_bf10, 0.9537841, tolerance = 0.001)
    testthat::expect_equal(df$log_10_bf10, -df$log_10_bf01, tolerance = 0.001)
  }
)

# bayes factor (independent samples t-test) ----------------------

testthat::test_that(
  desc = "bayes factor (independent samples t-test)",
  code = {
    testthat::skip_if(getRversion() < "3.6")

    # from Bayes Factor
    df <- suppressMessages(statsExpressions::bf_extractor(
      BayesFactor::ttestBF(
        formula = len ~ supp,
        data = as.data.frame(ToothGrowth),
        rscale = 0.99,
        paired = FALSE
      )
    ))

    # extracting results from where this function is implemented
    set.seed(123)
    df_results <- statsExpressions::bf_ttest(
      data = ToothGrowth,
      x = supp,
      y = "len",
      paired = FALSE,
      bf.prior = 0.99,
      output = "results"
    )

    # check bayes factor values
    testthat::expect_equal(df$log_e_bf10, -0.001119132, tolerance = 0.001)
    testthat::expect_equal(df$log_e_bf10, -df$log_e_bf01, tolerance = 0.001)
    testthat::expect_equal(df$log_10_bf10, -0.0004860328, tolerance = 0.001)
    testthat::expect_equal(df$log_10_bf10, -df$log_10_bf01, tolerance = 0.001)

    # checking if two usages of the function are producing the same results
    testthat::expect_equal(df$bf10, df_results$bf10, tolerance = 0.001)
    testthat::expect_equal(df$log_e_bf01, df_results$log_e_bf01, tolerance = 0.001)
  }
)

# Bayes factor (paired t-test) ---------------------------------------------

testthat::test_that(
  desc = "bayes factor (paired t-test)",
  code = {
    testthat::skip_if(getRversion() < "3.6")

    # data
    dat <- tidyr::spread(bugs_long, condition, desire) %>%
      dplyr::filter(.data = ., !is.na(HDLF), !is.na(HDHF))

    # BF output
    set.seed(123)
    df <- suppressMessages(statsExpressions::bf_extractor(
      BayesFactor::ttestBF(
        x = dat$HDLF,
        y = dat$HDHF,
        rscale = 0.8,
        paired = TRUE
      )
    ))

    # creating a tidy dataframe
    dat_tidy <- dplyr::filter(bugs_long, condition %in% c("HDLF", "HDHF"))

    # extracting results from where this function is implemented
    set.seed(123)
    df_results <- statsExpressions::bf_ttest(
      data = dat_tidy,
      x = "condition",
      y = desire,
      paired = TRUE,
      bf.prior = 0.8,
      output = "results"
    )

    # check bayes factor values
    testthat::expect_equal(df$bf10, 40.36079, tolerance = 0.001)
    testthat::expect_equal(df$log_e_bf10, 3.697859, tolerance = 0.001)
    testthat::expect_equal(df$log_e_bf10, -df$log_e_bf01, tolerance = 0.001)
    testthat::expect_equal(df$log_10_bf10, 1.60596, tolerance = 0.001)
    testthat::expect_equal(df$log_10_bf10, -df$log_10_bf01, tolerance = 0.001)

    # checking if two usages of the function are producing the same results
    testthat::expect_equal(df$bf10, df_results$bf10, tolerance = 0.001)
    testthat::expect_equal(df$log_e_bf01, df_results$log_e_bf01, tolerance = 0.001)
  }
)

# bayes factor (one sample t-test) ----------------------

testthat::test_that(
  desc = "bayes factor (one sample t-test)",
  code = {
    testthat::skip_if(getRversion() < "3.6")

    # creating a dataframe
    set.seed(123)
    df <- suppressMessages(statsExpressions::bf_extractor(
      BayesFactor::ttestBF(
        x = iris$Petal.Length,
        mu = 5.5,
        rscale = 0.99
      )
    ))

    # extracting results from where this function is implemented
    set.seed(123)
    df_results <- statsExpressions::bf_ttest(
      data = iris,
      x = Petal.Length,
      y = NULL,
      test.value = 5.5,
      bf.prior = 0.99,
      output = "results"
    )

    # check Bayes factor values
    testthat::expect_equal(df$bf10, 5.958171e+20, tolerance = 0.001)
    testthat::expect_equal(df$log_e_bf10, 47.83647, tolerance = 0.001)
    testthat::expect_equal(df$log_e_bf10, -df$log_e_bf01, tolerance = 0.001)
    testthat::expect_equal(df$log_10_bf10, 20.77511, tolerance = 0.001)
    testthat::expect_equal(df$log_10_bf10, -df$log_10_bf01, tolerance = 0.001)

    # checking if two usages of the function are producing the same results
    testthat::expect_equal(df$bf10, df_results$bf10, tolerance = 0.001)
    testthat::expect_equal(df$log_e_bf01, df_results$log_e_bf01, tolerance = 0.001)
  }
)

# bayes factor (between-subjects - anova) ---------------------------------

testthat::test_that(
  desc = "bayes factor (between-subjects - anova)",
  code = {
    testthat::skip_if(getRversion() < "3.6")

    # dataframe
    set.seed(123)
    dat <- dplyr::filter(ggplot2::msleep, !is.na(brainwt), !is.na(vore)) %>%
      dplyr::mutate(.data = ., vore = as.factor(vore))

    # creating a dataframe
    set.seed(123)
    df <- suppressMessages(statsExpressions::bf_extractor(
      BayesFactor::anovaBF(
        formula = brainwt ~ vore,
        data = as.data.frame(dat),
        progress = FALSE,
        rscaleFixed = 0.99
      )
    ))

    # extracting results from where this function is implemented
    set.seed(123)
    df_results <- statsExpressions::bf_oneway_anova(
      data = dat,
      x = vore,
      y = brainwt,
      bf.prior = 0.99,
      output = "results"
    )

    # extracting caption - null
    set.seed(123)
    results1 <- statsExpressions::bf_oneway_anova(
      data = dat,
      x = vore,
      y = brainwt,
      bf.prior = 0.88,
      output = "null"
    )

    # extracting caption - alternative
    set.seed(123)
    results2 <- statsExpressions::bf_oneway_anova(
      data = dat,
      x = vore,
      y = brainwt,
      bf.prior = 0.88,
      output = "alternative"
    )

    # check bayes factor values
    testthat::expect_equal(df$bf10, 0.1177186, tolerance = 0.001)
    testthat::expect_equal(df$log_e_bf10, -2.139458, tolerance = 0.001)
    testthat::expect_equal(df$log_e_bf10, -df$log_e_bf01, tolerance = 0.001)
    testthat::expect_equal(df$log_10_bf10, -0.9291548, tolerance = 0.001)
    testthat::expect_equal(df$log_10_bf10, -df$log_10_bf01, tolerance = 0.001)

    # checking if two usages of the function are producing the same results
    testthat::expect_equal(df$bf10, df_results$bf10, tolerance = 0.001)
    testthat::expect_equal(df$log_e_bf01, df_results$log_e_bf01, tolerance = 0.001)

    # call for null and alternative
    testthat::expect_identical(
      results1,
      ggplot2::expr(atop(
        displaystyle(NULL),
        expr = paste(
          "In favor of null: ",
          "log"["e"],
          "(BF"["01"],
          ") = ",
          "1.92",
          ", ",
          italic("r")["Cauchy"]^"JZS",
          " = ",
          "0.88"
        )
      ))
    )

    testthat::expect_identical(
      results2,
      ggplot2::expr(atop(
        displaystyle(NULL),
        expr = paste(
          "In favor of alternative: ",
          "log"["e"],
          "(BF"["10"],
          ") = ",
          "-1.92",
          ", ",
          italic("r")["Cauchy"]^"JZS",
          " = ",
          "0.88"
        )
      ))
    )
  }
)

# bayes factor (within-subjects - anova) ---------------------------------

testthat::test_that(
  desc = "bayes factor (within-subjects - anova)",
  code = {
    testthat::skip_if(getRversion() < "3.6")

    # dataframe
    dat <- WRS2::WineTasting

    # creating a dataframe
    set.seed(123)
    df <- suppressMessages(statsExpressions::bf_extractor(
      BayesFactor::anovaBF(
        formula = Taste ~ Wine + Taster,
        data = as.data.frame(dat),
        progress = FALSE,
        whichRandom = "Taster",
        rscaleFixed = 0.99,
        rscaleRandom = 1
      )
    ))

    # extracting results from where this function is implemented
    set.seed(123)
    df_results <- statsExpressions::bf_oneway_anova(
      data = dat,
      x = Wine,
      y = "Taste",
      paired = TRUE,
      bf.prior = 0.99,
      output = "results"
    )

    # extracting caption - null
    set.seed(123)
    results1 <- statsExpressions::bf_oneway_anova(
      data = dat,
      x = "Wine",
      y = Taste,
      k = 4,
      paired = TRUE,
      bf.prior = 0.88,
      output = "null"
    )

    # extracting caption - alternative
    set.seed(123)
    results2 <- statsExpressions::bf_oneway_anova(
      data = dat,
      x = Wine,
      y = Taste,
      k = 4,
      paired = TRUE,
      bf.prior = 0.88,
      output = "alternative"
    )

    # check bayes factor values
    testthat::expect_equal(df$bf10, 6.364917, tolerance = 0.001)
    testthat::expect_equal(df$log_e_bf10, 1.850801, tolerance = 0.001)
    testthat::expect_equal(df$log_e_bf10, -df$log_e_bf01, tolerance = 0.001)
    testthat::expect_equal(df$log_10_bf10, 0.8037927, tolerance = 0.001)
    testthat::expect_equal(df$log_10_bf10, -df$log_10_bf01, tolerance = 0.001)

    # checking if two usages of the function are producing the same results
    testthat::expect_equal(df$bf10, df_results$bf10, tolerance = 0.001)
    testthat::expect_equal(df$log_e_bf01, df_results$log_e_bf01, tolerance = 0.001)

    # call for null and alternative
    testthat::expect_identical(
      results1,
      ggplot2::expr(atop(
        displaystyle(NULL),
        expr = paste(
          "In favor of null: ",
          "log"["e"],
          "(BF"["01"],
          ") = ",
          "-1.9580",
          ", ",
          italic("r")["Cauchy"]^"JZS",
          " = ",
          "0.8800"
        )
      ))
    )

    testthat::expect_identical(
      results2,
      ggplot2::expr(atop(
        displaystyle(NULL),
        expr = paste(
          "In favor of alternative: ",
          "log"["e"],
          "(BF"["10"],
          ") = ",
          "1.9580",
          ", ",
          italic("r")["Cauchy"]^"JZS",
          " = ",
          "0.8800"
        )
      ))
    )
  }
)

# bayes factor (proportion test) --------------------------------------

testthat::test_that(
  desc = "bayes factor (proportion test)",
  code = {
    testthat::skip_if(getRversion() < "3.6")

    # extracting results from where this function is implemented
    set.seed(123)
    df <-
      statsExpressions::bf_contingency_tab(
        data = mtcars,
        x = am,
        output = "results"
      )

    # check bayes factor values
    testthat::expect_equal(df$bf10, 0.2465787, tolerance = 0.001)
    testthat::expect_equal(df$log_e_bf10, -1.400074, tolerance = 0.001)
    testthat::expect_equal(df$log_e_bf10, -df$log_e_bf01, tolerance = 0.001)
    testthat::expect_equal(df$log_10_bf10, -0.6080444, tolerance = 0.001)
    testthat::expect_equal(df$log_10_bf10, -df$log_10_bf01, tolerance = 0.001)

    # caption
    set.seed(123)
    caption_text <-
      statsExpressions::bf_contingency_tab(
        data = mtcars,
        x = "cyl",
        output = "alternative",
        prior.concentration = 10
      )

    testthat::expect_identical(
      caption_text,
      ggplot2::expr(atop(
        displaystyle(NULL),
        expr = paste(
          "In favor of alternative: ",
          "log"["e"],
          "(BF"["10"],
          ") = ",
          "-0.55",
          ", ",
          italic("a"),
          " = ",
          "10.00"
        )
      ))
    )
  }
)

# bayes factor (contingency tab) --------------------------------------

testthat::test_that(
  desc = "bayes factor (contingency tab)",
  code = {
    testthat::skip_if(getRversion() < "3.6")

    # extracting results from where this function is implemented
    set.seed(123)
    df <- suppressMessages(statsExpressions::bf_extractor(
      BayesFactor::contingencyTableBF(
        x = table(mtcars$am, mtcars$cyl),
        sampleType = "jointMulti",
        fixedMargin = "rows"
      )
    ))

    # extracting results from where this function is implemented
    set.seed(123)
    df_results <- statsExpressions::bf_contingency_tab(
      data = mtcars,
      x = am,
      y = cyl,
      sampling.plan = "jointMulti",
      fixed.margin = "rows",
      output = "results"
    )

    # caption
    caption_text <- statsExpressions::bf_contingency_tab(
      data = mtcars,
      x = am,
      y = "cyl",
      sampling.plan = "jointMulti",
      fixed.margin = "rows",
      output = "alternative"
    )

    # with counts
    caption_text2 <- statsExpressions::bf_contingency_tab(
      data = as.data.frame(Titanic),
      x = "Survived",
      y = Sex,
      counts = "Freq",
      sampling.plan = "jointMulti",
      fixed.margin = "rows",
      output = "alternative"
    )

    # with counts
    caption_text3 <- statsExpressions::bf_contingency_tab(
      data = as.data.frame(Titanic),
      x = Survived,
      y = Sex,
      counts = "Freq",
      output = "H0"
    )

    # check bayes factor values
    testthat::expect_equal(df$bf10, 28.07349, tolerance = 0.001)
    testthat::expect_equal(df$log_e_bf10, 3.334826, tolerance = 0.001)
    testthat::expect_equal(df$log_e_bf10, -df$log_e_bf01, tolerance = 0.001)
    testthat::expect_equal(df$log_10_bf10, 1.448296, tolerance = 0.001)
    testthat::expect_equal(df$log_10_bf10, -df$log_10_bf01, tolerance = 0.001)

    # checking if two usages of the function are producing the same results
    testthat::expect_equal(df$bf10, df_results$bf10, tolerance = 0.001)
    testthat::expect_equal(df$log_e_bf01, df_results$log_e_bf01, tolerance = 0.001)

    # caption text
    testthat::expect_identical(caption_text, ggplot2::expr(atop(
      displaystyle(NULL),
      expr = paste(
        "In favor of alternative: ",
        "log"["e"],
        "(BF"["10"],
        ") = ",
        "3.33",
        ", sampling = ",
        "joint multinomial",
        ", ",
        italic("a"),
        " = ",
        "1.00"
      )
    )))

    testthat::expect_identical(
      caption_text2,
      ggplot2::expr(atop(
        displaystyle(NULL),
        expr = paste(
          "In favor of alternative: ",
          "log"["e"],
          "(BF"["10"],
          ") = ",
          "214.25",
          ", sampling = ",
          "joint multinomial",
          ", ",
          italic("a"),
          " = ",
          "1.00"
        )
      ))
    )
    testthat::expect_identical(
      caption_text3,
      ggplot2::expr(atop(
        displaystyle(NULL),
        expr = paste(
          "In favor of null: ",
          "log"["e"],
          "(BF"["01"],
          ") = ",
          "-213.98",
          ", sampling = ",
          "independent multinomial",
          ", ",
          italic("a"),
          " = ",
          "1.00"
        )
      ))
    )
  }
)

# check edge cases --------------------------------------------

testthat::test_that(
  desc = "bayes factor caption maker check",
  code = {
    df <- data.frame(x = c("a"))

    testthat::expect_null(bf_onesample_proptest(df, x))
  }
)

# bayes factor caption maker check --------------------------

testthat::test_that(
  desc = "bayes factor caption maker check",
  code = {
    testthat::skip_if(getRversion() < "3.6")

    # bayes factor results
    set.seed(123)
    bf_results <- tibble::tribble(
      ~log_e_bf01, ~bf.prior,
      1.1, 0.88
    )

    # expected
    using1 <- statsExpressions::bf_expr(
      bf.df = bf_results,
      k = 3,
      caption = substitute(paste(italic("Note", ": made up data")))
    )
    using2 <- statsExpressions::bf_expr(
      bf.df = bf_results,
      output = "H1",
      caption = substitute(paste(italic("Note", ": made up data")))
    )

    testthat::expect_identical(
      using1,
      ggplot2::expr(atop(
        displaystyle(paste(italic(
          "Note", ": made up data"
        ))),
        expr = paste(
          "In favor of null: ",
          "log"["e"],
          "(BF"["01"],
          ") = ",
          "1.100",
          ", ",
          italic("r")["Cauchy"]^"JZS",
          " = ",
          "0.880"
        )
      ))
    )

    testthat::expect_identical(
      using2,
      ggplot2::expr(atop(
        displaystyle(paste(italic(
          "Note", ": made up data"
        ))),
        expr = paste(
          "In favor of alternative: ",
          "log"["e"],
          "(BF"["10"],
          ") = ",
          "-1.10",
          ", ",
          italic("r")["Cauchy"]^"JZS",
          " = ",
          "0.88"
        )
      ))
    )
  }
)


# bf_meta works ----------------------------------------------------

context("bf_meta")

testthat::test_that(
  desc = "bf_meta works",
  code = {
    testthat::skip_if(getRversion() < "3.6")

    # setup
    set.seed(123)
    library(metaBMA)

    # creating a dataframe
    df1 <-
      structure(
        .Data = list(
          term = c("1", "2", "3", "4", "5"),
          estimate = c(
            0.382047603321706,
            0.780783111514665,
            0.425607573765058,
            0.558365541235078,
            0.956473848429961
          ),
          std.error = c(
            0.0465576338644502,
            0.0330218199731529,
            0.0362834986178494,
            0.0480571500648261,
            0.062215818388157
          )
        ),
        row.names = c(NA, -5L),
        class = c("tbl_df", "tbl", "data.frame")
      )

    # getting bayes factor in favor of null hypothesis
    set.seed(123)
    subtitle1 <-
      suppressWarnings(statsExpressions::bf_meta(
        data = df1,
        k = 3,
        messages = TRUE,
        iter = 1000,
        summarize = "integrate"
      ))

    testthat::expect_identical(
      subtitle1,
      ggplot2::expr(atop(
        displaystyle(NULL),
        expr = paste(
          "In favor of null: ",
          "log"["e"],
          "(BF"["01"],
          ") = ",
          "-3.341",
          ", ",
          italic("d")["mean"]^"posterior",
          " = ",
          "0.518",
          ", CI"["95%"],
          " [",
          "0.219",
          ", ",
          "0.766",
          "]"
        )
      ))
    )
  }
)
