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
