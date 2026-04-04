# Copilot Instructions for statsExpressions

## Package Overview

`statsExpressions` is an R package that creates tidy dataframes and
expressions with statistical details from various statistical tests. It
serves as a backend for `ggstatsplot`.

## Architecture

### Main Functions (R/)

- **Statistical tests**:
  [`oneway_anova()`](https://www.indrapatil.com/statsExpressions/reference/oneway_anova.md),
  [`two_sample_test()`](https://www.indrapatil.com/statsExpressions/reference/two_sample_test.md),
  [`one_sample_test()`](https://www.indrapatil.com/statsExpressions/reference/one_sample_test.md),
  [`corr_test()`](https://www.indrapatil.com/statsExpressions/reference/corr_test.md),
  [`contingency_table()`](https://www.indrapatil.com/statsExpressions/reference/contingency_table.md),
  [`meta_analysis()`](https://www.indrapatil.com/statsExpressions/reference/meta_analysis.md),
  [`pairwise_comparisons()`](https://www.indrapatil.com/statsExpressions/reference/pairwise_comparisons.md)
- **Each test supports 4 types** via `type` parameter: `"parametric"`,
  `"nonparametric"`, `"robust"`, `"bayes"`
- **Output**: All functions return a tibble with a special `expression`
  column containing plotmath expressions

### Key Helper Functions

- [`add_expression_col()`](https://www.indrapatil.com/statsExpressions/reference/add_expression_col.md):
  Adds plotmath expression column to statistical output
- [`tidy_model_parameters()`](https://www.indrapatil.com/statsExpressions/reference/tidy_model_parameters.md)
  / `tidy_model_effectsize()`: Convert easystats output to tidyverse
  conventions
- [`extract_stats_type()`](https://www.indrapatil.com/statsExpressions/reference/extract_stats_type.md):
  Normalizes type input (`"p"` → `"parametric"`, `"n"` →
  `"nonparametric"`, etc.)
- [`long_to_wide_converter()`](https://www.indrapatil.com/statsExpressions/reference/long_to_wide_converter.md):
  Reshapes paired data from long to wide format

### Dependencies

Core: tidyverse stack (`dplyr`, `purrr`, `tibble`, `rlang`), easystats
ecosystem (`insight`, `parameters`, `performance`, `effectsize`,
`bayestestR`, `datawizard`)

## Developer Workflow

### Common Commands (via Makefile)

``` bash
make document     # Generate documentation (roxygen2)
make build        # Build package tarball
make check        # Run R CMD check
make install      # Install package locally
make lint         # Run lintr checks
make format       # Format code with styler
make install_deps # Install/update dependencies
```

### Testing

- Framework: `testthat` (edition 3) with parallel execution
- Run tests:
  [`devtools::test()`](https://devtools.r-lib.org/reference/test.html)
  or `make check`
- **Snapshot tests**: Used extensively for expression output
  verification
- Test files mirror source files: `R/oneway-anova.R` →
  `tests/testthat/test-oneway-anova-*.R`
- Coverage requirement: **100%** (enforced by codecov)

### Adding New Tests

``` r

test_that("descriptive name", {
  df <- function_under_test(data = dataset, x = var1, y = var2)
  expect_snapshot(df[["expression"]][[1]])  # For expressions
  expect_identical(dim(df), c(1L, 15L))     # For dimensions
})
```

## Code Conventions

### Style

- **Linting**: Uses
  [`lintr::all_linters()`](https://lintr.r-lib.org/reference/all_linters.html)
  with exceptions in `.lintr`
- **Formatting**:
  [`styler::style_pkg()`](https://styler.r-lib.org/reference/style_pkg.html)
  (run via `make format`)
- **Naming**: snake_case for functions and variables
- **Pipes**: Use base R `|>` pipe (NOT magrittr `%>%`)

### Roxygen Documentation

- Uses `roxygen2` with markdown support
  (`Roxygen: list(markdown = TRUE)`)
- Uses `@autoglobal` annotation from `roxyglobals` for global variables
- Template files in `man-roxygen/` for shared documentation
- Run `make document` after modifying roxygen comments

### Function Parameters

Common parameters across functions:

- `data`: Input dataframe
- `x`, `y`: Column names (unquoted, uses tidy evaluation)
- `type`: One of `"parametric"`, `"nonparametric"`, `"robust"`,
  `"bayes"`
- `paired`: Logical for paired/within-subjects designs
- `digits`: Number of decimal places
- `conf.level`: Confidence level (0-1)

## Important Patterns

### Type Switching

Functions use internal switch mechanisms to select appropriate
statistical methods:

``` r

.f <- switch(type,
  parametric    = stats::aov,
  nonparametric = kruskal.test,
  robust        = WRS2::t1way,
  bayes         = BayesFactor::anovaBF
)
```

### Expression Generation

Statistical results are converted to plotmath expressions for plot
annotations:

``` r

df |> add_expression_col()
```

### Easystats Integration

Package heavily relies on easystats for model extraction:

``` r

parameters::model_parameters(model) |> tidy_model_parameters()
effectsize::effectsize(model) |> tidy_model_effectsize()
```

## Testing Tips

- Set seeds before Bayesian tests: `set.seed(123)`
- Use `skip_if_not_installed()` for optional dependencies
- Bayesian test values may vary across platforms; use
  `expect_snapshot()` for expressions
- Use [`suppressWarnings()`](https://rdrr.io/r/base/warning.html) when
  tests intentionally trigger warnings

## Files to Update Together

When modifying a function:

1.  `R/<function>.R` - Source code
2.  `man/<function>.Rd` - Will auto-generate from roxygen
3.  `tests/testthat/test-<function>-*.R` - Tests
4.  `man/rmd-fragments/<function>.Rmd` - Extended examples (if exists)
5.  `NEWS.md` - Document user-facing changes

## CI/CD

- GitHub Actions workflows in `.github/workflows/`
- Automated R CMD check on multiple platforms
- Code coverage uploaded to Codecov (100% required)
- Dependency updates via Dependabot
