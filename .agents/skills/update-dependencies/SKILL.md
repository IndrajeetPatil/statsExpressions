---
name: update-dependencies
description: Update dependencies to their latest versions and keep the package compatible, change the minimum supported R version, or add, remove, or move a package dependency in DESCRIPTION. Use only for dependency maintenance, not for installing the current dependency set.
---

# Update dependencies

`DESCRIPTION` is the source of truth for R and package dependency constraints.
Regenerate `codemeta.json`, `NAMESPACE`, `R/globals.R`, and `man/*.Rd` from it
instead of editing them by hand.

## Refresh constraints

1. Run `make update_deps`. It tidies `DESCRIPTION`, rewrites every constraint
   to the latest CRAN version, re-runs roxygen, and regenerates
   `codemeta.json`. Use `make install_deps` instead when the goal is only to
   install the dependencies already declared.
2. Inspect the diff and confirm that every updated constraint is the latest
   suitable stable release. Read the upstream changelogs and documentation for
   upgraded packages.
3. Fix breaking API changes, statistical-output regressions, snapshot changes,
   documentation drift, coverage regressions, and lint or check failures. Keep
   public return columns, attributes, plotmath expressions, tidy-evaluation
   behavior, and the statistical semantics consumed by `ggstatsplot` stable.
4. Where a new dependency API can remove a local adapter or workaround, keep the
   simplification only if the affected snapshots are unchanged. Do not replace
   explicit statistical calls with a generic when confidence intervals,
   pooled-SD choices, effect-size corrections, or missing-data behavior differ.

## Add, remove, or move a dependency

- Edit `DESCRIPTION` directly, then run `make document` and
  `Rscript -e 'codemetar::write_codemeta()'`.
- Audit `Imports` versus `Suggests` by tracing runtime paths through the
  easystats stack, not only by searching for direct namespace calls. Every
  advertised statistical mode must work on a hard-dependency installation. Keep
  `bayestestR` and `rstantools` in `Imports` while the Bayesian summary and
  ANOVA paths require them.
- Guard `Suggests` packages with `skip_if_not_installed()` in tests and with
  conditional use in code and examples.
- Declare packages needed only by CI checks in `Config/Needs/check` (or the
  matching `Config/Needs/*` field), not in workflow caller inputs.

## Minimum R version

- Change `Depends` in `DESCRIPTION` together with any genuinely linked
  configuration, such as the CI matrix described in the `maintain-ci` skill.
- Keep README support wording as "R-devel, the current R release, and the
  previous R release" rather than hard-coding version numbers.

## Validate and publish

1. Start with the affected `testthat` files and snapshots, then iterate until
   the full gate passes: `air format . --check`, `make lint`, `make hooks`, and
   `make check`.
2. Keep validation serial when R processes share an installation library. Run
   `make clean` afterwards and verify that only intended files changed.
3. Follow the `NEWS.md` policy in `AGENTS.md`: record a higher minimum R version
   or a newly required package, but omit routine constraint bumps. Commit
   dependency bumps as `chore(deps): ...`.
4. If the branch already has a pull request, update its body with the
   dependency groups, compatibility fixes, simplifications, and validation
   commands; otherwise open one following `AGENTS.md`.
