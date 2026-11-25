# AGENTS.md

Project instructions for AI coding agents. `AGENTS.md`-aware tools read
this file directly. It records only what the code, `DESCRIPTION`,
`Makefile`, and configuration files do not already make clear; read
those for the rest.

## Package overview

`statsExpressions` creates tidy data frames and plotmath expressions
with details from statistical tests. It is the statistical backend for
`ggstatsplot`, which consumes the returned columns and attributes, so
keep them stable.

## Implementation patterns

- Supported `type` values vary by function, and some functions, such as
  [`pairwise_contingency_table()`](https://www.indrapatil.com/statsExpressions/reference/pairwise_contingency_table.md),
  take no `type`. Normalise `type` with
  [`extract_stats_type()`](https://www.indrapatil.com/statsExpressions/reference/extract_stats_type.md),
  then follow the function’s existing branching rather than imposing one
  shared [`switch()`](https://rdrr.io/r/base/switch.html) shape.
- Normalise easystats output with
  [`tidy_model_parameters()`](https://www.indrapatil.com/statsExpressions/reference/tidy_model_parameters.md)
  and `tidy_model_effectsize()` instead of renaming columns or
  rebuilding confidence intervals by hand.
- Inside `mutate()` and other data-masking calls, including `glue()`
  called there, a bare name resolves to a column of `data` before a
  local variable. Build local values such as expression templates before
  the call and refer to them with `.env$name`, so input columns can’t
  shadow them.
- Preserve tidy evaluation for unquoted column arguments, and use the
  base pipe (`|>`).

## Validation

Start with the narrowest check that proves a change, such as the
affected `testthat` files and snapshots. When a change affects shared
behaviour, dependencies, generated metadata, or workflows, also run the
full gate:

``` bash
air format . --check
make lint
make hooks
make check
```

## Testing

- Snapshot the output instead of hand-writing expected values, with the
  tidy columns and the `expression` column as separate snapshots:

  \
  `test_that``(``"descriptive name"``, ``{`\
  `  ``df`` ``<-`` ``function_under_test``(``data ``=`` ``dataset``, x ``=`` ``var1``, y ``=`` ``var2``)`\
  `  ``expect_snapshot``(``dplyr``::`[`select`](https://dplyr.tidyverse.org/reference/select.html)`(``df``, ``-``expression``)``)`\
  `  ``expect_snapshot``(``df``[[``"expression"``]``]``)`\
  `}``)`

- Cover input combinations with
  [`patrick::with_parameters_test_that()`](https://rdrr.io/pkg/patrick/man/with_parameters_test_that.html),
  naming cases in a `.test_name` column (`test_name` is deprecated).

- CI requires 100% line and branch coverage (covr’s
  `covr.branch_coverage`), and Codecov requires 100% project and patch
  coverage. A [`switch()`](https://rdrr.io/r/base/switch.html) without a
  default leaves an untaken “no match” branch: make its last case the
  default or use a lookup vector instead. Exclude a branch that can’t be
  covered with `# nocov`, and say why in a comment.

- Set seeds only before Bayesian or otherwise stochastic tests.

- Use `skip_if_not_installed()` for optional dependencies, and suppress
  warnings only when a test deliberately exercises a warning path.

## Documentation

- Never edit generated files by hand. After changing roxygen comments or
  `README.Rmd`, run `make document` and commit the regenerated
  `NAMESPACE`, `man/*.Rd`, and `README.md`.
- Shared prose and tables live in `man/rmd-fragments/`, and shared
  example code in `man/examples/`. They feed the Rd files, `README.Rmd`,
  and the vignettes, so edit the source rather than a rendered copy.
- Keep the README introductory and link to articles for method tables
  and output schemas. Add examples only for a distinct workflow or
  important behaviour, not for every function or option.
- Use sentence case for prose headings, preserving function names,
  proper nouns, and published titles. Keep table labels such as
  **Hypothesis testing** and **Effect size estimation** bold.
- End each vignette with a Citation section calling
  `citation("statsExpressions")`. Write the footer inline: pkgdown can
  render a vignette from a temporary file, which breaks relative child
  documents.
- `@examplesIf` cannot wrap an `@example` file. For a `NOT_CRAN` guard
  around a shared example file, copy the
  `\dontshow{if (...) withAutoprint(\{ # examplesIf}` and
  `\dontshow{\}) # examplesIf}` lines from `R/two-sample-test.R`. This
  is the intended form, not a compatibility fallback: it keeps
  auto-printing, and pkgdown recognises the `# examplesIf` markers.

## Versioning and changelog

- Development versions use a fourth-component `.9000` suffix. Keep the
  version in `DESCRIPTION`, `codemeta.json`, and the first `NEWS.md`
  heading in sync.
- Keep `NEWS.md` concise and user-facing. Record behaviour and
  compatibility changes, including a higher minimum R version, a newly
  required package, or a dependency minimum that new code requires. Omit
  routine dependency updates and lint, formatting, CI, or generated-file
  maintenance.

## Repository skills

Task-specific procedures live in `.agents/skills/`. Read one only when
the task matches it:

- `create-release`: prepare, submit, resume, or publish a CRAN release.
- `update-dependencies`: update dependencies, change the minimum R
  version, or add, remove, or move a dependency.
- `maintain-ci`: change or debug workflows under `.github/workflows/`.

User-invoked prompts live in `.github/prompts/`. Keep each topic in
exactly one place: `AGENTS.md` for every-session rules, a skill or
prompt for task-specific procedures.

## Continuous integration

Workflows under `.github/workflows/` are thin callers of shared reusable
workflows; the `maintain-ci` skill covers changing them.

- Reference `IndrajeetPatil/workflows` reusable workflows as `@main`,
  never a commit SHA. The owner maintains them and is the only person
  who can contribute to this repository, so there is no
  external-contributor risk, and `@main` keeps it on the latest version.
  Code scanners (CodeQL `actions/unpinned-tag`, zizmor `unpinned-uses`)
  flag this by design: do not report it or propose a pin; dismiss such
  alerts as “won’t fix” with this reason.

## Benchmarks

See
[benchmarks/README.md](https://www.indrapatil.com/statsExpressions/benchmarks/README.md)
for API coverage policy, workload catalogue, local execution, and
interpretation of CodSpeed results.

## Learn from mistakes

When a check fails, a reviewer raises a finding, or you notice a mistake
in your own work, fix it, then ask what led you to it. If `AGENTS.md`, a
skill, its scripts or templates, a prompt, or a doc gave missing,
ambiguous, or wrong guidance, correct that guidance in the same pull
request, stating the general rule rather than the incident. Never weaken
a quality gate instead.

## Pull requests

Follow these rules without relying on any instructions outside this
repository:

- Write every commit message and PR title as a Conventional Commit,
  `type(scope): description`, lowercase and imperative, with an allowed
  type (`build`, `chore`, `ci`, `docs`, `feat`, `fix`, `perf`,
  `refactor`, `revert`, `style`, `test`). Keep the PR title and body in
  sync with the net diff.

- Open every PR ready for review with `gh pr create`, never as a draft.

- Never add agent or tool attribution to commits, PRs, or comments.

- After opening or pushing, wait for hosted CI to finish. Inspect failed
  jobs, fix the cause, run the local checks (`make check` and
  `make hooks`), push, and repeat. Skipped, queued, or failing required
  checks are not success.

- If an automatic AI review (Codex, for the owner) runs on the PR, wait
  for its result instead of requesting one. Fix merited findings, reply
  to and resolve addressed threads, and continue until the review
  approves the current head with no unresolved findings. Never write
  `@codex` anywhere in a PR, even in backticks: it cancels the automatic
  review. Say “a Codex review request” instead.

- If Codex reports exhausted usage, leave the PR open and report it; no
  other reviewer or silence counts as approval. Once usage refreshes,
  comment `@codex review` once; only this case needs the comment.

- Once CI is green and the review gate is met, squash-merge, then verify
  the merged state. Recheck the head, threads, and checks immediately
  before merging, and never bypass branch protections. Because only the
  repository owner can contribute, a coding agent may use admin
  privileges to squash-merge when GitHub’s required human approval is
  the only remaining blocker. Never use admin privileges to bypass
  builds or Codex approval.

- Update a PR branch from `main` only to resolve conflicts or pick up a
  change the PR depends on, never just to bring it up to date.

- Report local validation, hosted CI, review, and merge state
  separately. If an external blocker stops completion, leave the PR open
  and say so.
