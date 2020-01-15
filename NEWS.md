# statsExpressions 0.2.0.9000

NEW FEATURES

  - New functions to carry out meta-analysis: `expr_meta_parametric`.

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
