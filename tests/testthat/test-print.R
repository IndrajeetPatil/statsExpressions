test_that(
  desc = "print doesn't change global options and returns object invisibly",
  code = {
    withr::local_options(list(tibble.width = 5))

    old_option <- getOption("tibble.width")
    old_tbl <- contingency_table(mtcars, am)

    capture.output({
      new_tbl <- old_tbl %>% print()
    })
    new_option <- getOption("tibble.width")

    expect_identical(new_option, old_option)
    expect_identical(new_tbl, old_tbl)
  }
)
