# within- and between-subjects data, first without and then with `NA`s
convert_example_data <- function(spread) {
  list(
    long_to_wide_converter(iris_long, condition, value, spread = spread),
    long_to_wide_converter(mtcars, am, wt, paired = FALSE, spread = spread),
    long_to_wide_converter(bugs_long, condition, desire, spread = spread),
    long_to_wide_converter(
      msleep,
      vore,
      brainwt,
      paired = FALSE,
      spread = spread
    )
  )
}

withr::local_options(list(tibble.width = Inf))

test_that("long_to_wide_converter works - spread true", {
  skip_if(
    grepl("Under development", R.version$status, fixed = TRUE),
    "Skipping on R-devel due to summary() format changes"
  )

  dfs <- convert_example_data(spread = TRUE)

  expect_snapshot(purrr::walk(dfs, dplyr::glimpse))
  skip_if(
    getRversion() < "4.6.0",
    "Skipping on R < 4.6.0: summary() changed NA label from 'NA\\'s' to 'NAs' in 4.6.0"
  )
  expect_snapshot(purrr::map(dfs, summary))
})

test_that("long_to_wide_converter works - spread false", {
  dfs <- convert_example_data(spread = FALSE)

  expect_snapshot(purrr::walk(dfs, dplyr::glimpse))
  expect_snapshot(purrr::map(dfs, summary))
})

# with .rowid - without NA ---------------------------------------------

test_that("with .rowid - without NA", {
  data <- dplyr::tibble(
    score = c(90, 90, 72.5, 45),
    condition = factor(c("4", "5", "5", "4")),
    id = c(1L, 2L, 1L, 2L)
  )

  expect_subject_id_invariance(long_to_wide_converter, data)
  expect_subject_id_invariance(long_to_wide_converter, data, spread = FALSE)
})


# with .rowid - with NA ---------------------------------------------

test_that("with .rowid - with NA", {
  convert_without_rowid <- function(...) {
    select(long_to_wide_converter(...), -.rowid)
  }

  df_arranged <- arrange(bugs_long, subject)

  for (spread in c(TRUE, FALSE)) {
    expect_identical(
      convert_without_rowid(df_arranged, condition, desire, spread = spread),
      convert_without_rowid(
        bugs_long,
        condition,
        desire,
        subject,
        spread = spread
      )
    )
  }
})


# NA in subject.id ------------------------------------------------------

test_that("rows with NA subject.id are dropped only for paired designs", {
  df <- dplyr::tibble(
    id = c(1L, 1L, NA_integer_, NA_integer_),
    group = c("a", "b", "a", "b"),
    value = c(10, 20, 30, 40)
  )

  convert <- function(paired) {
    long_to_wide_converter(
      df,
      x = group,
      y = value,
      subject.id = id,
      paired = paired,
      spread = FALSE
    )
  }

  expect_identical(convert(paired = TRUE)$.rowid, c(1L, 1L))
  expect_shape(convert(paired = FALSE), nrow = 4L)
})

# incomplete subjects --------------------------------------------------

test_that("paired designs keep only subjects observed in every condition", {
  df <- dplyr::tibble(
    id = c(1L, 1L, 2L, 2L, 3L, 4L, 4L, 4L),
    group = c("a", "b", "a", "b", "a", "a", "b", NA),
    value = c(1, 2, 3, NA, 5, 6, 7, 8)
  )

  expect_identical(
    long_to_wide_converter(df, group, value, subject.id = id)$.rowid,
    c(1L, 4L)
  )

  # without `subject.id`, a condition with fewer rows leaves subjects unpaired
  expect_identical(
    long_to_wide_converter(
      dplyr::tibble(group = c("a", "a", "a", "b", "b"), value = 1:5),
      group,
      value
    )$.rowid,
    c(1L, 2L)
  )
})

# duplicated subjects --------------------------------------------------

test_that("paired designs need at most one row per subject and condition", {
  df <- dplyr::tibble(
    id = c(1L, 1L, 1L, 2L, 2L),
    group = c("a", "a", "b", "a", "b"),
    value = c(1, 2, 3, 4, 5)
  )

  expect_snapshot_error(long_to_wide_converter(df, group, value, id))
  expect_snapshot_error(
    two_sample_test(df, group, value, subject.id = id, paired = TRUE)
  )
  expect_shape(
    long_to_wide_converter(
      df,
      group,
      value,
      id,
      paired = FALSE,
      spread = FALSE
    ),
    nrow = 5L
  )
})

# levels without any response --------------------------------------------

test_that("between-subjects designs drop levels whose responses are all missing", {
  df <- mutate(mtcars, cyl = factor(cyl), wt = replace_when(wt, cyl == 8 ~ NA))
  df_complete <- filter(df, cyl != 8)

  res <- long_to_wide_converter(df, cyl, wt, paired = FALSE, spread = FALSE)

  expect_identical(levels(res$cyl), c("4", "6"))

  # robust tests error on an empty group
  for (.f in list(oneway_anova, pairwise_comparisons)) {
    set.seed(123)
    res_na <- .f(df, cyl, wt, type = "robust")
    set.seed(123)
    expect_identical(res_na, .f(df_complete, cyl, wt, type = "robust"))
  }
})
