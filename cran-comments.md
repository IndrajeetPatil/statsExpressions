## Test environments
* local OS X install, R 3.6.0
* ubuntu 14.04 on travis-ci (devel and release)
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 1 notes

  - I get 1 `NOTE` about invalid URLs in `README`. These URLs
    are correct and will become active once this package is on `CRAN`.
    
      Found the following (possibly) invalid URLs:
    - URL: https://CRAN.R-project.org/package=statsExpressions/vignettes/
      From: README.md
      Status: 404
      Message: Not Found
      
    - URL: https://cran.r-project.org/web/checks/check_results_statsExpressions.html
      From: README.md
      Status: 404
      Message: Not Found
    
  - There is no reference corresponding to this package so I have not included
    any link in the `DESCRIPTION` file.
    
  - I was asked to remove the `LICENSE` files, but I would prefer to retain
    them. This is because a lot of users explore the package on GitHub and it
    would better to have this file front and center in the home directory. This
    is a practice I have followed in all of my other packages and I have not
    been asked to remove them.
    
  - I have added `value` section to `.Rd` files for exported functions.
