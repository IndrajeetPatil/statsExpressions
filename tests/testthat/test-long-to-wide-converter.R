withr::local_options(list(tibble.width = Inf))

test_that(desc = "long_to_wide_converter works - spread true", code = {
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

test_that(desc = "long_to_wide_converter works - spread false", code = {
  dfs <- convert_example_data(spread = FALSE)

  expect_snapshot(purrr::walk(dfs, dplyr::glimpse))
  expect_snapshot(purrr::map(dfs, summary))
})

# with .rowid - without NA ---------------------------------------------

test_that(desc = "with .rowid - without NA", code = {
  data <- dplyr::tibble(
    score = c(90, 90, 72.5, 45),
    condition = factor(c("4", "5", "5", "4")),
    id = c(1L, 2L, 1L, 2L)
  )

  expect_subject_id_invariance(long_to_wide_converter, data)
  expect_subject_id_invariance(long_to_wide_converter, data, spread = FALSE)
})


# with .rowid - with NA ---------------------------------------------

test_that(desc = "with .rowid - with NA", code = {
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


# NA in subject.id should not drop rows with complete x/y ----------------

test_that("NA in subject.id does not drop rows with complete measurements", {
  df <- dplyr::tibble(
    id = c(1L, 1L, NA_integer_, NA_integer_),
    group = c("a", "b", "a", "b"),
    value = c(10, 20, 30, 40)
  )

  result <- long_to_wide_converter(
    df,
    x = group,
    y = value,
    subject.id = id,
    paired = TRUE,
    spread = FALSE
  )

  expect_shape(result, nrow = 4L)
})
