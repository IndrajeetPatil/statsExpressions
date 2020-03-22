# statsExpressions 0.4.0.9000

  - All Bayes Factor related functions have now moved to the new `tidyBF`
    package and are re-exported from there.
  
  - Removes the experimental `corr_objects` function.

# statsExpressions 0.3.1
 
  - Adds a new function `corr_objects` to reduce dependency load of
    `ggstatsplot`. This is an experimental function and should be avoided until
    it stabilizes.

# statsExpressions 0.3.0

NEW FEATURES

  - New functions to carry out meta-analysis: `expr_meta_bayes`.

# statsExpressions 0.2.1

NEW FEATURES

  - New functions to carry out meta-analysis: `expr_meta_parametric`,
    `expr_meta_robust`, `bf_meta`.

# statsExpressions 0.2.0

BREAKING CHANGES

  - `expr_template` function now expects two dataframes: `stats.df` and
    `effsize.df` that contain the details needed for creating expressions
    instead of providing each individual values. This makes the function more
    friendly work with using modeling packages like `broom`.

MINOR CHANGES

  - Minor tweaks to how widehat is displayed in some of the expressions. 
  - Cramer's *V* is bias-corrected by default.
 
# statsExpressions 0.1.3

MAJOR CHANGES

  - Removes `MCMCpack` from `Depends`.
  
  - All effect size texts now contain `^` on top to signify that these are
    estimates.

# statsExpressions 0.1.2

MINOR CHANGES

  - Maintenance release to fix additional check issues on `CRAN`.

# statsExpressions 0.1.1

MINOR CHANGES

  - Fixing tests for the new release of `rcompanion` dependency.
  - Minor code refactoring.

# statsExpressions 0.1.0

  - First release of the package.
