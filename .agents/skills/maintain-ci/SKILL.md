---
name: maintain-ci
description: Change, debug, or add GitHub Actions workflows under .github/workflows/ for this R package. Use only for CI/CD work, not for ordinary code changes.
---

# Maintain CI/CD

## Workflow layout

Workflows under `.github/workflows/` run standard and hard R CMD checks,
coverage, documentation and extra checks, formatting, linting, prek hooks,
pkgdown builds, SEO files, and CRAN submission.

Nearly every job is a thin caller of a reusable workflow in
`IndrajeetPatil/workflows` (`uses: IndrajeetPatil/workflows/.github/workflows/<name>.yaml@main`).

- Change the caller's `with:` inputs rather than copying a reusable workflow
  into this repository.
- When a fix belongs in the shared workflow, make it in
  `IndrajeetPatil/workflows` and keep this repository's caller compatible.
- Declare package-specific check tools in `DESCRIPTION` (for example
  `Config/Needs/check`) rather than in caller inputs.

## Check matrix

The shared R CMD check matrix intentionally covers R-devel, release, and
oldrel. Do not reintroduce `oldrel-2` unless the package support policy
changes.

## Reproduce locally

| CI job           | Local command                                 |
| ---------------- | --------------------------------------------- |
| R-CMD-check      | `make check`                                  |
| lint             | `make lint`                                   |
| check-formatting | `air format . --check`                        |
| pre-commit       | `make hooks`                                  |
| check-docs       | `lychee .` (links) and `typos` (spelling)     |

Coverage must stay at 100% for both project and patch (see `codecov.yaml`).
