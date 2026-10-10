# AGENTS.md

Project-level instructions for AI coding agents working on this repository.
GitHub Copilot Code Review, Copilot coding agent, Codex, and other
`AGENTS.md`-aware tools read this file directly.

## Package overview

`statsExpressions` is an R package that creates tidy data frames and plotmath
expressions with details from statistical tests. It serves as the statistical
backend for `ggstatsplot`.

## Architecture

### Main functions (`R/`)

- Statistical tests: `oneway_anova()`, `two_sample_test()`,
  `one_sample_test()`, `corr_test()`, `contingency_table()`, `meta_analysis()`,
  `pairwise_comparisons()`, and `pairwise_contingency_table()`.
- Supporting public helpers include `centrality_description()`,
  `add_expression_col()`, `tidy_model_expressions()`,
  `tidy_model_parameters()`, `extract_stats_type()`, `stats_type_switch()`, and
  `long_to_wide_converter()`.
- The package standardizes parametric, nonparametric, robust, and Bayesian
  `type` labels, but supported choices vary by function. Do not assume every
  exported function has this interface; for example,
  `pairwise_contingency_table()` does not accept `type`.
- Statistical functions return tibbles with a special `expression` column for
  plotmath output where applicable.

### Key internal helpers

- `tidy_model_effectsize()`: Convert effect-size output to tidy conventions.
- `extract_estimate_type()` and `extract_statistic_text()`: Choose plotmath
  labels for estimates and test statistics.
- `prior_switch()`: Choose prior labels for Bayesian output.

### Dependencies

Core dependencies include the tidyverse stack (`dplyr`, `purrr`, `tidyr`, and
`rlang`) and the easystats ecosystem (`insight`, `parameters`, `performance`,
`effectsize`, `bayestestR`, `datawizard`, and `correlation`). Treat
`DESCRIPTION` as the source of truth for dependency constraints and the minimum
supported R version.

## Developer workflow

Use the repository `Makefile` for routine package tasks:

```bash
make install_deps # Install dependencies declared in DESCRIPTION
make build        # Build the package tarball
make check        # Build and run R CMD check --no-manual
make install      # Build and install the package locally
make document     # Regenerate roxygen docs and render README.Rmd
make lint         # Run lintr::lint_package()
make format       # Run air format .
make hooks        # Run all prek hooks
make clean        # Remove package build and check artifacts
make update_deps  # Refresh dependency constraints (maintenance only)
```

### Versioning and changelog

- Development versions use a fourth-component `.9000` suffix.
- Keep the version in `DESCRIPTION`, `codemeta.json`, and the first `NEWS.md`
  heading synchronized.
- Record user-facing compatibility changes in `NEWS.md`; omit routine
  dependency updates and internal lint or CI maintenance.

## Testing

- The package uses `testthat` edition 3 with parallel execution.
- `make check` is the canonical full local validation command.
- Snapshot tests are used extensively for both tidy statistical output and the
  `expression` column.
- Tests cover source areas, but helper and shared source files may be exercised
  by broader test files rather than a one-to-one filename match.
- Set seeds before Bayesian or otherwise stochastic tests, and only there.
- Use `skip_if_not_installed()` for optional dependencies.
- Suppress warnings only when a test intentionally exercises a warning-producing
  path.
- Codecov requires 100% project and patch coverage.
- Use `patrick::with_parameters_test_that()` to cover combinations of inputs,
  with a `.test_name` column in `.cases` (the `test_name` column is
  deprecated). Snapshot each case's output rather than hand-writing expected
  expressions.

Follow the existing snapshot style:

```r
test_that("descriptive name", {
  df <- function_under_test(data = dataset, x = var1, y = var2)
  expect_snapshot(dplyr::select(df, -expression))
  expect_snapshot(df[["expression"]])
})
```

## Code conventions

- Use `lintr` for linting and [Air](https://posit-dev.github.io/air/) for
  formatting; CI runs `air format . --check`.
- Use snake_case for functions and variables.
- Use the base R pipe (`|>`), not the magrittr pipe (`%>%`).
- Preserve tidy evaluation for unquoted column arguments.
- Inside `mutate()` and other data-masking calls, including `glue()` called
  there, a bare name resolves to a column of `data` before a local variable.
  Build local values such as expression templates before the call and refer
  to them with `.env$name`, so input columns can't shadow them.

### Roxygen documentation

- Roxygen uses Markdown and the `pkgapi` and `roxyglobals` roclets configured in
  `DESCRIPTION`.
- Use `@autoglobal` from `roxyglobals` where appropriate.
- Shared documentation tables and prose live in `man/rmd-fragments/`; shared
  example code lives in `man/examples/`.
- `@examplesIf` cannot wrap an `@example` file. To keep a `NOT_CRAN` guard
  around a shared example file, use the explicit
  `\dontshow{if (...) withAutoprint(\{ # examplesIf}` and
  `\dontshow{\}) # examplesIf}` `@examples` lines around `@example`, as in
  `R/two-sample-test.R`.
- After changing roxygen comments, run `make document` and commit the generated
  `NAMESPACE` or `man/*.Rd` changes. Do not edit generated `.Rd` files by hand.

### Common function parameters

- `data`: Input data frame.
- `x`, `y`: Unquoted column names using tidy evaluation.
- `type`: Usually one of `"parametric"`, `"nonparametric"`, `"robust"`, or
  `"bayes"` where supported.
- `paired`: Whether the design is paired or within-subjects.
- `digits`: Number of decimal places.
- `conf.level`: Confidence level between 0 and 1.

## Important patterns

### Statistical method selection

Functions normalize supported `type` values with `extract_stats_type()` and
then select the appropriate statistical and effect-size functions. Follow the
existing per-function branching and argument construction; do not assume one
shared `switch()` shape applies to every analysis.

### Expression generation

Statistical results are standardized and passed to `add_expression_col()` or
the specialized expression helpers. Keep returned columns and attributes stable
because `ggstatsplot` consumes them.

### Easystats integration

Use `tidy_model_parameters()` and `tidy_model_effectsize()` to normalize
easystats output rather than duplicating column-renaming and confidence-interval
logic.

## Files to update together

When modifying a function, consider all relevant surfaces:

1. `R/<function>.R` or its helper file.
2. The corresponding files under `tests/testthat/`.
3. Generated `man/<function>.Rd` after roxygen regeneration.
4. `man/rmd-fragments/<function>.Rmd` when that fragment exists. Fragments are
   shared by the Rd files, `README.Rmd`, and the vignettes, so edit the
   fragment rather than its rendered copies.
5. `man/examples/examples-<function>.R` when that example file exists. These
   files are shared by the Rd files and the *Data frame outputs* article.
6. `README.Rmd` (then re-render `README.md`) and the vignettes under
   `vignettes/` when user-facing behavior or output changes.
7. `NEWS.md` for user-facing changes.

## Repository skills

Task-specific instructions live in `.agents/skills/`. Read a skill only when
the task matches it:

- `create-release`: prepare, submit, resume, or publish a CRAN release.
- `update-dependencies`: update dependencies to their latest versions, change
  the minimum R version, or add, remove, or move a dependency.
- `maintain-ci`: change or debug workflows under `.github/workflows/`.

User-invoked prompts for other tasks live in `.github/prompts/`. Keep each topic
in exactly one place: `AGENTS.md` for every-session rules, a skill or a prompt
for task-specific procedures.

## Pull requests

Open pull requests as ready for review rather than as drafts. Unless explicitly
requested, do not wait for CI/CD checks to finish after pushing; report that the
checks were triggered and include the pull request or workflow link.
