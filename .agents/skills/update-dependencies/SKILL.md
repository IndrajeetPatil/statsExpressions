---
name: update-dependencies
description: Refresh dependency constraints, change the minimum supported R version, or add, remove, or move a package dependency in DESCRIPTION. Use only for dependency maintenance, not for installing the current dependency set.
---

# Update dependencies

`DESCRIPTION` is the source of truth for dependency constraints. Generated
metadata (`codemeta.json`, `man/`) follows from it.

## Install versus refresh

- To install the current dependency set, use `make install_deps`.
- `make update_deps` is a maintenance operation. It tidies `DESCRIPTION`,
  rewrites every constraint to the latest CRAN version, re-runs roxygen, and
  regenerates `codemeta.json`. Run it only when the task is to refresh
  dependency minimums, never merely to get a working environment.

## Changing dependencies

1. Edit `DESCRIPTION` directly for targeted changes (adding, removing, or
   moving a package between `Imports` and `Suggests`).
2. Packages needed only by CI checks belong in `Config/Needs/check` (or the
   matching `Config/Needs/*` field), not in reusable-workflow caller inputs.
3. A package moved to `Suggests` must be guarded with `skip_if_not_installed()`
   in tests and conditional use in code and examples.
4. Regenerate metadata with `make document` and `codemetar::write_codemeta()`
   (both are included in `make update_deps`).

## Supported R versions

- The minimum supported R version is declared in `Depends` in `DESCRIPTION`.
- CI covers R-devel, the current R release, and the previous R release. When
  the support policy changes, update `DESCRIPTION` and the shared check matrix
  together.
- Keep README support wording independent of specific version numbers.

## Validate and record

1. Run `make check`, `make lint`, and `make hooks`.
2. Record a dependency change in `NEWS.md` only when it affects users, such as
   a higher minimum R version or a newly required package. Omit routine
   constraint bumps.
3. Commit dependency bumps as `chore(deps): ...`.
