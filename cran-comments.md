## Test environments
* local Windows install, R 4.0.0
* ubuntu 14.04 on travis-ci (devel and release)
* win-builder on appveyor (devel and release)

## R CMD check results

0 errors | 0 warnings | 0 note

  - Fixes failing CRAN tests due to `effectsize` and `metaBMA` package updates.
  
  - All `donttest` examples have been checked on CI platforms.
  
## Reverse dependencies check

  - Expect failures in `ggstatsplot` package, which I maintain. I will be
    submitting a new version of that package soon.
