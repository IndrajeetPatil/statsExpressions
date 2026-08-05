---
name: update-deps
description: Update dependencies and ensure the package is compatible with their latest versions
---

# Update Dependencies and Refactor the Package

Run `make update_deps` to refresh dependency constraints, roxygen-generated
metadata, and `codemeta.json`. This is a maintenance operation; use
`make install_deps` when the goal is only to install the dependencies currently
declared in `DESCRIPTION`.

Inspect the resulting diff and confirm that every updated constraint represents
the latest suitable stable release. Use current upstream documentation and
changelogs for upgraded packages. Prefer Context7 for library API documentation,
as required by this repository's instructions.

Fix breaking API changes, statistical-output regressions, snapshot changes,
documentation drift, coverage regressions, lint failures, and build or check
failures introduced by the upgrades. Preserve public return columns, attributes,
plotmath expressions, tidy-evaluation behavior, and the statistical semantics
consumed by `ggstatsplot`.

Review whether new dependency APIs can remove local adapters or workarounds, but
only keep simplifications that reduce code without changing results. Validate
each candidate against the affected snapshots. Do not replace explicit
statistical calls with a generic when confidence intervals, pooled-SD choices,
effect-size corrections, or missing-data behavior differ.

Treat `DESCRIPTION` as the source of truth for package constraints. Regenerate
`codemeta.json` and roxygen outputs instead of editing them by hand. Audit
`Imports` versus `Suggests` using runtime behavior, not only direct namespace
references: every advertised statistical mode must work on a hard-dependency
installation. Keep `bayestestR` and `rstantools` in `Imports` while the Bayesian
summary and ANOVA paths require them.

If the minimum R version changes, update `DESCRIPTION` and any genuinely linked
configuration together. Keep README support wording expressed as R-devel,
current release, and previous release rather than hard-coding version numbers.
Do not reintroduce `oldrel-2` without an explicit support-policy change.

If the package version changes, synchronize `DESCRIPTION`, `codemeta.json`, and
the first `NEWS.md` heading. Keep NEWS user-facing and omit routine dependency,
lint, CI, and generated-metadata maintenance.

The workflows in `.github/workflows/` primarily call reusable workflows from
`IndrajeetPatil/workflows`. Update the callers when their interface changes; do
not copy shared workflow implementations into this repository. If a caller
directly uses a public action, verify its latest stable release and preserve the
repository's established pinning convention.

Start with focused tests for affected functions, then iterate until the full
local gate passes:

- affected `testthat` files and snapshots
- `air format . --check`
- `make lint`
- `make hooks`
- `make check`

Keep the work serial when R package validation shares an installation library.
Clean build and check artifacts with `make clean`, then verify that only intended
files remain changed.

Commit and push the validated update. If the branch already has a pull request,
update that pull request's body with the dependency groups, compatibility fixes,
simplifications, and validation commands. Otherwise, open a ready-for-review
pull request using the gh CLI. Do not wait for CI unless requested, but report
the live pull request and check state accurately.
