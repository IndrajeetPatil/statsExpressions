---
name: address-review
description: Address code review comments and reply to them
disable-model-invocation: true
---

# Address Code Review Comments

Use the gh CLI to fetch the current pull request's thread-aware review state. For
every unresolved comment, determine whether it has merit. Fix valid findings;
for invalid or conflicting findings, reply with concrete repository evidence.
Reply to every unresolved comment on my behalf, and resolve a thread only after
the response and any required fix have been pushed and verified.

When a review concerns a dependency, R version, or tooling inconsistency, search
the entire repository for every declaration and generated surface that must stay
aligned. `DESCRIPTION` is the source of truth for R and package dependency
constraints. Regenerate `codemeta.json`, `NAMESPACE`, `R/globals.R`, and
`man/*.Rd` through the repository's maintenance or documentation commands when
their sources change; do not edit generated files by hand.

Do not move a package from `Imports` to `Suggests` based only on a search for
direct namespace calls. Trace runtime paths through the easystats stack and
verify that every advertised statistical mode still works with hard dependencies
only. In particular, retain `bayestestR` and `rstantools` as core dependencies
for the Bayesian summary and ANOVA paths unless the package's public behavior is
intentionally changed.

If the package version changes, keep `DESCRIPTION`, `codemeta.json`, and the
first `NEWS.md` heading synchronized. Keep NEWS concise and user-facing; do not
add entries for routine dependency, lint, CI, or review maintenance.

If roxygen comments change, run `make document` and commit the relevant
generated documentation. Preserve the package's tidy-evaluation interfaces,
returned columns, expression output, and snapshot conventions. Add or update
tests for behavioral fixes, set seeds for stochastic tests, and keep project and
patch coverage at 100%.

When a comment concerns GitHub Actions, preserve the reusable workflows from
`IndrajeetPatil/workflows` and update this repository's callers rather than
copying shared workflow implementations here.

Choose the narrowest relevant validation first, then run the broader gates when
the change affects shared behavior, dependency resolution, generated metadata,
or workflows. Common checks are:

- the affected `testthat` files and snapshots
- `air format . --check`
- `make lint`
- `make hooks`
- `make check`

Commit and push validated fixes to the pull request branch. Re-fetch review
threads to prove they are resolved, and report the live pull request and check
state without claiming that in-progress CI is green.
