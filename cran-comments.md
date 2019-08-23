## Test environments
* local OS X install, R 3.6.0
* ubuntu 14.04 on travis-ci (devel and release)
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 1 notes

  - I get 1 `NOTE` about a couple of URLs that are currently invalid because
    they link to `CRAN` documentation for this package and those links will
    become active only when the package is on `CRAN`.

  - Added `\donttest{}` to few examples to reduce the check time to less than 10
    minutes. The code coverage still stands at `100%`, so this should in no way
    compromise integrity of code.
    
  - There is no reference corresponding to this package so I have not included
    any link in the `DESCRIPTION` file.
    
  - I was asked to remove the `LICENSE` files, but I would prefer to retain
    them. A lot of users explore the package on `GitHub` and it would better to
    have this file front and center in the home directory. This is a practice I
    have followed in all of my other packages on `CRAN` and I have not been
    asked to remove them.
    
  - I have added `value` section to `.Rd` files for exported functions.
