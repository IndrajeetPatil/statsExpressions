## Test environments
* local Windows install, R 3.6.2
* ubuntu 14.04 on travis-ci (devel and release)
* win-builder on appveyor (devel and release)

## R CMD check results

0 errors | 0 warnings | 1 note

  - This is the 7th update in the past 6 months because the package belongs to
    an ecosystem of packages which are all being simultaneously updated to
    reduce dependency load for each package. 
    
    In particular, the current update reduces the number of `Imports` for the
    `ggstatsplot` package to less than 20. This fixes a new `NOTE` recently
    implemented in `r-devel-linux-x86_64-fedora-clang`.
