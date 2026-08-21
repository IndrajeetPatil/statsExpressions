# AGENTS.md

Project-level instructions for AI coding agents working on this
repository. GitHub Copilot Code Review, Copilot coding agent, Codex, and
other `AGENTS.md`-aware tools read this file directly.

## Package overview

`statsExpressions` is an R package that creates tidy data frames and
plotmath expressions with details from statistical tests. It serves as
the statistical backend for `ggstatsplot`.

## Architecture

### Main functions (`R/`)

- Statistical tests:
  [`oneway_anova()`](https://www.indrapatil.com/statsExpressions/reference/oneway_anova.md),
  [`two_sample_test()`](https://www.indrapatil.com/statsExpressions/reference/two_sample_test.md),
  [`one_sample_test()`](https://www.indrapatil.com/statsExpressions/reference/one_sample_test.md),
  [`corr_test()`](https://www.indrapatil.com/statsExpressions/reference/corr_test.md),
  [`contingency_table()`](https://www.indrapatil.com/statsExpressions/reference/contingency_table.md),
  [`meta_analysis()`](https://www.indrapatil.com/statsExpressions/reference/meta_analysis.md),
  [`pairwise_comparisons()`](https://www.indrapatil.com/statsExpressions/reference/pairwise_comparisons.md),
  and
  [`pairwise_contingency_table()`](https://www.indrapatil.com/statsExpressions/reference/pairwise_contingency_table.md).
- Supporting public helpers include
  [`centrality_description()`](https://www.indrapatil.com/statsExpressions/reference/centrality_description.md),
  [`add_expression_col()`](https://www.indrapatil.com/statsExpressions/reference/add_expression_col.md),
  [`tidy_model_expressions()`](https://www.indrapatil.com/statsExpressions/reference/tidy_model_expressions.md),
  [`tidy_model_parameters()`](https://www.indrapatil.com/statsExpressions/reference/tidy_model_parameters.md),
  [`extract_stats_type()`](https://www.indrapatil.com/statsExpressions/reference/extract_stats_type.md),
  [`stats_type_switch()`](https://www.indrapatil.com/statsExpressions/reference/extract_stats_type.md),
  and
  [`long_to_wide_converter()`](https://www.indrapatil.com/statsExpressions/reference/long_to_wide_converter.md).
- The package standardizes parametric, nonparametric, robust, and
  Bayesian `type` labels, but supported choices vary by function. Do not
  assume every exported function has this interface; for example,
  [`pairwise_contingency_table()`](https://www.indrapatil.com/statsExpressions/reference/pairwise_contingency_table.md)
  does not accept `type`.
- Statistical functions return tibbles with a special `expression`
  column for plotmath output where applicable.

### Key internal helpers

- `tidy_model_effectsize()`: Convert effect-size output to tidy
  conventions.
- `extract_estimate_type()` and `extract_statistic_text()`: Choose
  plotmath labels for estimates and test statistics.
- `prior_switch()`: Choose prior labels for Bayesian output.

### Dependencies

Core dependencies include the tidyverse stack (`dplyr`, `purrr`,
`tidyr`, and `rlang`) and the easystats ecosystem (`insight`,
`parameters`, `performance`, `effectsize`, `bayestestR`, `datawizard`,
and `correlation`). Treat `DESCRIPTION` as the source of truth for
dependency constraints.

The minimum supported R version is 4.5. CI covers R-devel, the current R
release, and the previous R release; keep README support wording
independent of specific version numbers.

## Developer workflow

Use the repository `Makefile` for routine package tasks:

``` bash
make install_deps # Install dependencies declared in DESCRIPTION
make build        # Build the package tarball
make check        # Build and run R CMD check --no-manual
make install      # Build and install the package locally
make document     # Regenerate roxygen docs and render README.Rmd
make lint         # Run lintr::lint_package()
make format       # Run styler::style_pkg()
make hooks        # Run all prek hooks
make clean        # Remove package build and check artifacts
make update_deps  # Refresh dependency constraints, docs, and codemeta
```

`make update_deps` is a maintenance operation that can rewrite
dependency constraints and generated metadata. Do not use it merely to
install the current dependency set.

### Versioning and changelog

- Development versions use a fourth-component `.9000` suffix.
- Keep the version in `DESCRIPTION`, `codemeta.json`, and the first
  `NEWS.md` heading synchronized.
- Record user-facing compatibility changes in `NEWS.md`; omit routine
  dependency updates and internal lint or CI maintenance.

### Creating a CRAN release

To cut a new CRAN release with a minor or patch version update: 1.
Create a release branch (e.g., `release-x.y.z`). 2. Update the version
number in `DESCRIPTION`, `NEWS.md` (remove `.9000` suffix), and
`codemeta.json`. 3. If addressing a CRAN issue, update
`cran-comments.md` to mention it. 4. Commit, push the branch, and open a
Pull Request. 5. Ensure all GitHub Actions PR checks pass and there are
no pending reviewer comments. 6. Ensure the Pull Request title contains
the exact text `CRAN Release`. 7. Do not trigger `submit-cran.yaml` from
an open Pull Request or a release branch. The reusable workflow both
submits the package to CRAN and immediately creates a GitHub Release and
tag; a successful submission does not mean CRAN has accepted the
package. 8. Only after the `CRAN Release` Pull Request is merged, verify
that `main` contains the release commit and trigger the workflow against
`main` (e.g., `gh workflow run submit-cran.yaml --ref main`). Do not
create a release or tag before this merge gate. 9. The maintainer will
receive an email from CRAN and manually confirm the submission. Track
CRAN acceptance separately and do not claim the package is released on
CRAN until it appears there.

## Testing

- The package uses `testthat` edition 3 with parallel execution.
- `make check` is the canonical full local validation command.
- Snapshot tests are used extensively for both tidy statistical output
  and the `expression` column.
- Tests cover source areas, but helper and shared source files may be
  exercised by broader test files rather than a one-to-one filename
  match.
- Set seeds before Bayesian or otherwise stochastic tests.
- Use `skip_if_not_installed()` for optional dependencies.
- Suppress warnings only when a test intentionally exercises a
  warning-producing path.
- Codecov requires 100% project and patch coverage.

Follow the existing snapshot style:

`test_that``(``"descriptive name"``, ``{`` `` ``df`` ``<-`` ``function_under_test``(``data ``=`` ``dataset``, x ``=`` ``var1``, y ``=`` ``var2``)`` `` ``expect_snapshot``(``dplyr``::`[`select`](https://dplyr.tidyverse.org/reference/select.html)`(``df``, ``-``expression``)``)`` `` ``expect_snapshot``(``df``[[``"expression"``]``]``)`` ``}``)`

## Code conventions

- Use `lintr` for linting and `styler` for formatting.
- Use snake_case for functions and variables.
- Use the base R pipe (`|>`), not the magrittr pipe (`%>%`).
- Preserve tidy evaluation for unquoted column arguments.

### Roxygen documentation

- Roxygen uses Markdown and the `pkgapi` and `roxyglobals` roclets
  configured in `DESCRIPTION`.
- Use `@autoglobal` from `roxyglobals` where appropriate.
- Shared extended examples live in `man/rmd-fragments/`.
- After changing roxygen comments, run `make document` and commit the
  generated `NAMESPACE` or `man/*.Rd` changes. Do not edit generated
  `.Rd` files by hand.

### Common function parameters

- `data`: Input data frame.
- `x`, `y`: Unquoted column names using tidy evaluation.
- `type`: Usually one of `"parametric"`, `"nonparametric"`, `"robust"`,
  or `"bayes"` where supported.
- `paired`: Whether the design is paired or within-subjects.
- `digits`: Number of decimal places.
- `conf.level`: Confidence level between 0 and 1.

## Important patterns

### Statistical method selection

Functions normalize supported `type` values with
[`extract_stats_type()`](https://www.indrapatil.com/statsExpressions/reference/extract_stats_type.md)
and then select the appropriate statistical and effect-size functions.
Follow the existing per-function branching and argument construction; do
not assume one shared [`switch()`](https://rdrr.io/r/base/switch.html)
shape applies to every analysis.

### Expression generation

Statistical results are standardized and passed to
[`add_expression_col()`](https://www.indrapatil.com/statsExpressions/reference/add_expression_col.md)
or the specialized expression helpers. Keep returned columns and
attributes stable because `ggstatsplot` consumes them.

### Easystats integration

Use
[`tidy_model_parameters()`](https://www.indrapatil.com/statsExpressions/reference/tidy_model_parameters.md)
and `tidy_model_effectsize()` to normalize easystats output rather than
duplicating column-renaming and confidence-interval logic.

## Files to update together

When modifying a function, consider all relevant surfaces:

1.  `R/<function>.R` or its helper file.
2.  The corresponding files under `tests/testthat/`.
3.  Generated `man/<function>.Rd` after roxygen regeneration.
4.  `man/rmd-fragments/<function>.Rmd` when that fragment exists.
5.  `NEWS.md` for user-facing changes.

## CI/CD

Workflows under `.github/workflows/` run standard and hard R CMD checks,
coverage, documentation and extra checks, formatting, linting, prek
hooks, pkgdown builds, and deployment tasks. Most jobs call reusable
workflows from `IndrajeetPatil/workflows`; update the callers rather
than copying those workflows into this repository.

The shared R CMD check matrix intentionally covers R-devel, release, and
oldrel. Do not reintroduce `oldrel-2` unless the package support policy
changes.

Open pull requests as ready for review rather than as drafts. Unless
explicitly requested, do not wait for CI/CD checks to finish after
pushing; report that the checks were triggered and include the pull
request or workflow link.
