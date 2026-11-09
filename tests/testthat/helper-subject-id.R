# Supplying `subject.id` for paired data should give the same result as
# relying on the row order of data already sorted by subject.
expect_subject_id_invariance <- function(.f, data, ...) {
  set.seed(123)
  with_id <- .f(
    data = data,
    x = condition,
    y = score,
    subject.id = id,
    paired = TRUE,
    ...
  )

  set.seed(123)
  sorted_by_id <- .f(
    data = arrange(data, id),
    x = condition,
    y = score,
    paired = TRUE,
    ...
  )

  # alternative BLAS (e.g. MKL) can introduce floating-point noise
  expect_equal(with_id, sorted_by_id, tolerance = 1e-10)
}
