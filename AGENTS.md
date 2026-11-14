# AGENTS.md

Project instructions for AI coding agents. `AGENTS.md`-aware tools read this
file directly. It records only what the code, `DESCRIPTION`, `Makefile`, and
configuration files do not already make clear; read those for the rest.

## Package overview

`statsExpressions` creates tidy data frames and plotmath expressions with
details from statistical tests. It is the statistical backend for
`ggstatsplot`, which consumes the returned columns and attributes, so keep
them stable.

## Implementation patterns

- Supported `type` values vary by function, and some functions, such as
  `pairwise_contingency_table()`, take no `type`. Normalise `type` with
  `extract_stats_type()`, then follow the function's existing branching rather
  than imposing one shared `switch()` shape.
- Normalise easystats output with `tidy_model_parameters()` and
  `tidy_model_effectsize()` instead of renaming columns or rebuilding
  confidence intervals by hand.
- Inside `mutate()` and other data-masking calls, including `glue()` called
  there, a bare name resolves to a column of `data` before a local variable.
  Build local values such as expression templates before the call and refer to
  them with `.env$name`, so input columns can't shadow them.
- Preserve tidy evaluation for unquoted column arguments, and use the base
  pipe (`|>`).

## Validation

Start with the narrowest check that proves a change, such as the affected
`testthat` files and snapshots. When a change affects shared behaviour,
dependencies, generated metadata, or workflows, also run the full gate:

```bash
air format . --check
make lint
make hooks
make check
```

## Testing

- Snapshot the output instead of hand-writing expected values, with the tidy
  columns and the `expression` column as separate snapshots:

  ```r
  test_that("descriptive name", {
    df <- function_under_test(data = dataset, x = var1, y = var2)
    expect_snapshot(dplyr::select(df, -expression))
    expect_snapshot(df[["expression"]])
  })
  ```

- Cover input combinations with `patrick::with_parameters_test_that()`, naming
  cases in a `.test_name` column (`test_name` is deprecated).
- Codecov requires 100% project and patch coverage.
- Set seeds only before Bayesian or otherwise stochastic tests.
- Use `skip_if_not_installed()` for optional dependencies, and suppress
  warnings only when a test deliberately exercises a warning path.

## Documentation

- Never edit generated files by hand. After changing roxygen comments or
  `README.Rmd`, run `make document` and commit the regenerated `NAMESPACE`,
  `man/*.Rd`, and `README.md`.
- Shared prose and tables live in `man/rmd-fragments/`, and shared example code
  in `man/examples/`. They feed the Rd files, `README.Rmd`, and the vignettes,
  so edit the source rather than a rendered copy.
- Keep the README introductory and link to articles for method tables and
  output schemas. Add examples only for a distinct workflow or important
  behaviour, not for every function or option.
- Use sentence case for prose headings, preserving function names, proper
  nouns, and published titles. Keep table labels such as
  **Hypothesis testing** and **Effect size estimation** bold.
- End each vignette with a Citation section calling
  `citation("statsExpressions")`. Write the footer inline: pkgdown can render a
  vignette from a temporary file, which breaks relative child documents.
- `@examplesIf` cannot wrap an `@example` file. For a `NOT_CRAN` guard around a
  shared example file, copy the `\dontshow{if (...) withAutoprint(\{ # examplesIf}`
  and `\dontshow{\}) # examplesIf}` lines from `R/two-sample-test.R`. This is
  the intended form, not a compatibility fallback: it keeps auto-printing, and
  pkgdown recognises the `# examplesIf` markers.

## Versioning and changelog

- Development versions use a fourth-component `.9000` suffix. Keep the version
  in `DESCRIPTION`, `codemeta.json`, and the first `NEWS.md` heading in sync.
- Keep `NEWS.md` concise and user-facing. Record behaviour and compatibility
  changes, including a higher minimum R version, a newly required package, or
  a dependency minimum that new code requires. Omit routine dependency updates
  and lint, formatting, CI, or generated-file maintenance.

## Repository skills

Task-specific procedures live in `.agents/skills/`. Read one only when the task
matches it:

- `create-release`: prepare, submit, resume, or publish a CRAN release.
- `update-dependencies`: update dependencies, change the minimum R version, or
  add, remove, or move a dependency.
- `maintain-ci`: change or debug workflows under `.github/workflows/`.

User-invoked prompts live in `.github/prompts/`. Keep each topic in exactly one
place: `AGENTS.md` for every-session rules, a skill or prompt for
task-specific procedures.

## Continuous integration

Workflows under `.github/workflows/` are thin callers of shared reusable
workflows; the `maintain-ci` skill covers changing them.

- Reference `IndrajeetPatil/workflows` reusable workflows as `@main`, never a
  commit SHA. The owner maintains them and is the only person who can
  contribute to this repository, so there is no external-contributor risk, and
  `@main` keeps it on the latest version. Code scanners (CodeQL
  `actions/unpinned-tag`, zizmor `unpinned-uses`) flag this by design: do not
  report it or propose a pin; dismiss such alerts as "won't fix" with this
  reason.

## Benchmarks

See [benchmarks/README.md](benchmarks/README.md) for API coverage policy,
workload catalogue, local execution, and interpretation of CodSpeed results.

## Pull requests

Open pull requests as ready for review. After pushing, wait for CI and fix any
failures. Squash-merge only after all required builds are green and Codex has
approved the current head, with no unresolved findings and the reviewed commit
matching the head. Because only the repository owner can contribute, a coding
agent may use admin privileges to squash-merge when GitHub's required human
approval is the only remaining blocker. Never use admin privileges to bypass
builds or Codex approval.
