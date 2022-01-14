## Test environments

* local OS X install, R 4.1.2

* ubuntu 18.04 (on github actions ci), R 4.1.2

* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 0 note

  - Fixes failing tests due to `{insight}` package update.

## revdepcheck results

One reverse dependency (`{ggstatsplot}`) is adversely affected. I am the
maintainer of this package and will be submitting an updated version to CRAN as
soon as `{statsExpressions}` update makes it to CRAN.
