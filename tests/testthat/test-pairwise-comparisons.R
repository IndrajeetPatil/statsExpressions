# between-subjects design --------------------------------------------------

# an empty factor level shouldn't change results
msleep_empty_level <- dplyr::mutate(
  msleep,
  vore = factor(vore, levels = c(levels(factor(vore)), "Random"))
)

# `msleep` has `NA`s, `iris` doesn't; `movies_long$mpaa` has levels with `-`
patrick::with_parameters_test_that(
  "`pairwise_comparisons()` works for between-subjects design:",
  {
    # `exec()` inlines the column names, as if they were typed in the call
    df <- rlang::exec(
      pairwise_comparisons,
      data = data,
      x = x,
      y = y,
      type = type,
      var.equal = var.equal,
      p.adjust.method = p.adjust.method
    )

    expect_snapshot(dplyr::select(df, -expression))
    expect_snapshot(df[["expression"]])
  },
  .cases = dplyr::tibble(
    .test_name = c(
      "Student's t",
      "Games-Howell, empty factor level",
      "Dunn",
      "robust t",
      "level names containing `-`",
      "Games-Howell, default p-value adjustment",
      "Student's t, without NAs"
    ),
    data = list(
      msleep,
      msleep_empty_level,
      msleep,
      msleep,
      movies_long,
      msleep_empty_level,
      iris
    ),
    x = c(rep("vore", 4L), "mpaa", "vore", "Species"),
    y = c(rep("brainwt", 4L), "rating", "brainwt", "Sepal.Length"),
    type = c("p", "p", "np", "r", "p", "p", "p"),
    var.equal = c(TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE),
    p.adjust.method = c(
      "bonferroni",
      "bonferroni",
      "none",
      "fdr",
      "holm",
      "holm",
      "fdr"
    )
  )
)

# dropped levels --------------------------------------------------

test_that("dropped levels are not included", {
  msleep2 <- dplyr::filter(msleep, vore %in% c("carni", "omni"))

  df1 <- pairwise_comparisons(
    data = msleep2,
    x = vore,
    y = brainwt,
    p.adjust.method = "none"
  )

  expect_snapshot(dplyr::select(df1, -expression))
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

# within-subjects design --------------------------------------------------

wine_tasting <- dplyr::rename(
  WRS2::WineTasting,
  condition = Wine,
  desire = Taste
)

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

    expect_snapshot(dplyr::select(df, -expression))
    expect_snapshot(df[["expression"]])
  },
  .cases = dplyr::tibble(
    data = rep(list(bugs_long, wine_tasting), each = 3L),
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

    expect_snapshot(dplyr::select(df, -expression))
    expect_snapshot(df[["expression"]])
  },
  .cases = dplyr::tibble(
    data = list(bugs_long, wine_tasting),
    .test_name = c("bugs_long", "WineTasting")
  )
)

patrick::with_parameters_test_that(
  "works with subject id:",
  {
    expect_subject_id_invariance(
      pairwise_comparisons,
      dplyr::rename(
        WRS2::WineTasting,
        condition = Wine,
        score = Taste,
        id = Taster
      ),
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

    expect_snapshot(dplyr::select(df, -expression))
    expect_snapshot(df[["expression"]])
  },
  .cases = dplyr::mutate(
    tidyr::expand_grid(
      dplyr::tibble(
        design = c("within-subjects", "between-subjects"),
        data = list(bugs_long, mtcars),
        x = c("condition", "cyl"),
        y = c("desire", "wt"),
        paired = c(TRUE, FALSE),
        var.equal = c(FALSE, TRUE)
      ),
      alternative = c("less", "greater")
    ),
    .test_name = paste(design, alternative, sep = ", ")
  )
)

# grouping names that clash with local variables ----------------------------

test_that("Bayesian comparisons work with `a` or `b` as grouping name", {
  set.seed(123)
  expected <- pairwise_comparisons(mtcars, cyl, wt, type = "bayes")

  for (group in c("a", "b")) {
    data <- dplyr::rename(mtcars, !!group := cyl)

    set.seed(123)
    df <- pairwise_comparisons(data, !!rlang::sym(group), wt, type = "bayes")

    expect_identical(df[["expression"]], expected[["expression"]])
  }
})
