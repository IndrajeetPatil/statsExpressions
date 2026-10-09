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

Add or update tests for behavioral fixes. Follow `AGENTS.md` for generated
files, versioning, `NEWS.md`, snapshots, and coverage. When a comment concerns dependencies or the R version, follow the
`update-dependencies` skill and search the entire repository for every
declaration and generated surface that must stay aligned. When a comment
concerns GitHub Actions, follow the `maintain-ci` skill.

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
