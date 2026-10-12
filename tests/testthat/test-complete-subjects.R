test_that("complete_subjects keeps only subjects observed in every condition", {
  df <- dplyr::tibble(
    id = c(3L, 3L, 1L, 1L, 2L, 2L, 4L, 4L, 4L, 5L, NA_integer_, NA_integer_),
    group = c("b", "a", "a", "b", "a", "b", "a", "b", NA, "a", "a", "b"),
    value = c(6, 5, 1, 2, 3, NA, 7, 8, 9, 10, 11, 12)
  )

  expect_snapshot(complete_subjects(df, group, value, subject.id = id))
})

test_that("complete_subjects pairs by row order without subject.id", {
  df <- dplyr::tibble(
    group = c("a", "b", "a", "b", "a"),
    value = c(1, 2, 3, NA, 5)
  )

  result <- complete_subjects(df, group, value)

  expect_identical(result$.rowid, c(1L, 1L))
  expect_identical(levels(result$group), c("a", "b"))
})

test_that("complete_subjects matches the data analyzed by paired tests", {
  expect_identical(
    complete_subjects(bugs_long, condition, desire, subject.id = subject),
    long_to_wide_converter(
      bugs_long,
      condition,
      desire,
      subject.id = subject,
      spread = FALSE
    )
  )
})

test_that("complete_subjects needs at most one row per subject and condition", {
  df <- dplyr::tibble(
    id = c(1L, 1L, 1L),
    group = c("a", "a", "b"),
    value = c(1, 2, 3)
  )

  expect_snapshot_error(complete_subjects(df, group, value, subject.id = id))
})
