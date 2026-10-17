# between-subjects design --------------------------------------------------

test_that("`pairwise_comparisons()` works for between-subjects design", {
  # student's t
  df1 <- pairwise_comparisons(
    data = msleep,
    x = vore,
    y = brainwt,
    type = "p",
    var.equal = TRUE,
    paired = FALSE,
    p.adjust.method = "bonferroni"
  )

  expect_snapshot(df1)
  expect_snapshot(df1[["expression"]])

  # games-howell, with an empty factor level (shouldn't change results)
  df_msleep <- dplyr::mutate(
    msleep,
    vore = factor(vore, levels = c(levels(factor(vore)), "Random"))
  )

  df2 <- pairwise_comparisons(
    data = df_msleep,
    x = vore,
    y = brainwt,
    type = "p",
    var.equal = FALSE,
    paired = FALSE,
    p.adjust.method = "bonferroni"
  )

  expect_snapshot(df2)
  expect_snapshot(df2[["expression"]])

  # Dunn test
  df3 <- pairwise_comparisons(
    data = msleep,
    x = vore,
    y = brainwt,
    type = "np",
    paired = FALSE,
    p.adjust.method = "none"
  )

  expect_snapshot(df3)
  expect_snapshot(df3[["expression"]])

  # robust t test
  df4 <- pairwise_comparisons(
    data = msleep,
    x = vore,
    y = brainwt,
    type = "r",
    paired = FALSE,
    p.adjust.method = "fdr"
  )

  expect_snapshot(df4)
  expect_snapshot(df4[["expression"]])

  # checking the edge case where factor level names contain `-`
  df5 <- pairwise_comparisons(
    data = movies_long,
    x = mpaa,
    y = rating,
    var.equal = TRUE
  )

  expect_snapshot(df5)
  expect_snapshot(df5[["expression"]])

  # games-howell with the default p-value adjustment
  df6 <- pairwise_comparisons(
    data = df_msleep,
    x = vore,
    y = brainwt
  )

  expect_snapshot(df6)
  expect_snapshot(df6[["expression"]])
})

# dropped levels --------------------------------------------------

test_that("dropped levels are not included", {
  msleep2 <- dplyr::filter(msleep, vore %in% c("carni", "omni"))

  df1 <- pairwise_comparisons(
    data = msleep2,
    x = vore,
    y = brainwt,
    p.adjust.method = "none"
  )

  expect_snapshot(df1)
  expect_snapshot(df1[["expression"]])

  df2 <- pairwise_comparisons(
    data = msleep,
    x = vore,
    y = brainwt,
    p.adjust.method = "none"
  ) |>
    dplyr::filter(group2 == "omni", group1 == "carni")

  expect_equal(df1$statistic, df2$statistic, tolerance = 0.01)
})

# data without NAs --------------------------------------------------

test_that("data without NAs", {
  df <- pairwise_comparisons(
    data = iris,
    x = Species,
    y = Sepal.Length,
    type = "p",
    p.adjust.method = "fdr",
    var.equal = TRUE
  )

  expect_snapshot(df)
  expect_snapshot(df[["expression"]])
})

# within-subjects design --------------------------------------------------

# `bugs_long` has `NA`s, `WRS2::WineTasting` doesn't
patrick::with_parameters_test_that(
  "`pairwise_comparisons()` works for within-subjects design:",
  {
    df <- pairwise_comparisons(
      data = data,
      x = condition,
      y = desire,
      type = type,
      digits = 3L,
      paired = TRUE,
      p.adjust.method = p.adjust.method
    )

    expect_snapshot(df)
    expect_snapshot(df[["expression"]])
  },
  .cases = dplyr::tibble(
    data = rep(
      list(
        bugs_long,
        rename(WRS2::WineTasting, condition = Wine, desire = Taste)
      ),
      each = 3L
    ),
    type = rep(c("p", "np", "r"), times = 2L),
    p.adjust.method = c("bonferroni", "BY", "hommel", "none", "none", "none"),
    .test_name = paste(rep(c("bugs_long", "WineTasting"), each = 3L), type)
  )
)

patrick::with_parameters_test_that(
  "Bayesian `pairwise_comparisons()` works for within-subjects design:",
  {
    set.seed(123)
    df <- pairwise_comparisons(
      data = data,
      x = condition,
      y = desire,
      type = "bf",
      paired = TRUE
    )

    expect_snapshot(df)
    expect_snapshot(df[["expression"]])
  },
  .cases = dplyr::tibble(
    data = list(
      bugs_long,
      rename(WRS2::WineTasting, condition = Wine, desire = Taste)
    ),
    .test_name = c("bugs_long", "WineTasting")
  )
)

patrick::with_parameters_test_that(
  "works with subject id:",
  {
    expect_subject_id_invariance(
      pairwise_comparisons,
      rename(WRS2::WineTasting, condition = Wine, score = Taste, id = Taster),
      type = type,
      digits = 3L
    )
  },
  .cases = dplyr::tibble(
    type = c("p", "np", "r", "bf"),
    .test_name = type
  )
)

# additional arguments are passed ---------------------------------------

alternative_cases <- tidyr::expand_grid(
  dplyr::tibble(
    design = c("within-subjects", "between-subjects"),
    data = list(bugs_long, mtcars),
    x = c("condition", "cyl"),
    y = c("desire", "wt"),
    paired = c(TRUE, FALSE),
    var.equal = c(FALSE, TRUE)
  ),
  alternative = c("less", "greater")
) |>
  dplyr::mutate(.test_name = paste(design, alternative, sep = ", "))

patrick::with_parameters_test_that(
  "additional arguments are passed to underlying methods:",
  {
    # `exec()` inlines the column names, as if they were typed in the call
    df <- rlang::exec(
      pairwise_comparisons,
      data = data,
      x = x,
      y = y,
      paired = paired,
      var.equal = var.equal,
      p.adjust.method = "none",
      alternative = alternative
    )

    expect_snapshot(df)
    expect_snapshot(df[["expression"]])
  },
  .cases = alternative_cases
)

# grouping names that clash with local variables ----------------------------

test_that("Bayesian comparisons work with `a` or `b` as grouping name", {
  set.seed(123)
  expected <- pairwise_comparisons(mtcars, cyl, wt, type = "bayes")

  for (group in c("a", "b")) {
    data <- rename(mtcars, !!group := cyl)

    set.seed(123)
    df <- pairwise_comparisons(data, !!rlang::sym(group), wt, type = "bayes")

    expect_identical(df[["expression"]], expected[["expression"]])
  }
})
