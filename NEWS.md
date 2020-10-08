# statsExpressions 0.5.1.9000

BREAKING CHANGES

  - Removes the alias `expr_onesample_proptest`.

MINOR CHANGES

  - Retires the vestigial `stat.title` argument. It was originally intended to
    give more info on the tests, but now the expressions themselves contain
    these details.

# statsExpressions 0.5.1

MINOR CHANGES

  - Adapts tests to changes made in the `correlation` package.
  
  - Subtitles for correlation tests make clear the type of statistic.
  
  - Small *p*-values (< 0.001) are now shown in scientific format.
  
# statsExpressions 0.5.0

MINOR CHANGES

  - Adapts to changes made in `tidyBF` package.
  
  - Re-exports `correlation::correlation` needed for `ggstatsplot`.
  
  - The `expr_t_nonparametric` subtitle now clarifies whether it's a Wilcoxon
    test or a Mann-Whitney test.

# statsExpressions 0.4.2

MINOR CHANGES

  - Thanks to Sarah, the package has a hexsticker. :)
  
  - Confidence intervals for Spearman's rho are computed using `correlation`
    instead of `rcompanion`.
    
  - All relevant functions get rid of `messages` argument as the functions no
    longer print a message when bootstrapped CIs are used.
    
  - The effect size measure for paired robust *t*-test is now changed to robust
    (trimmed-Winsorized) standardized difference similar to Cohen's *d*.

# statsExpressions 0.4.1

BUG FIXES

  - Major bug introduced in `0.4.0` release for `expr_anova_parametric`: 
    changing `conf.level` doesn't work and function defaults to `0.90` CIs (#32).

MINOR CHANGES

  - Removes extra space contained in subtitles for Bayes Factor results (#31).

# statsExpressions 0.4.0

BREAKING CHANGES

  - Removes the experimental `corr_objects` function.
  
  - All Bayes Factor related functions have now moved to the new `tidyBF`
    package and are re-exported from there.
  
MAJOR CHANGES

  - Minimum R version bumped to `R 3.6.0`.

  - Retires the internal `effsize_t_parametric` helper function in favor of
    relying functions from `effectsize`, which is now added as a dependency.
    Similarly, `statsExpressions` now relies on `effectsize` to compute effect
    sizes for ANOVA designs, instead of `sjstats`.

  - For parametric *t*-tests and ANOVAs, confidence intervals for effect sizes
    are estimated using the noncentrality parameter method. Centrality-based
    methods are deprecated.

  - Correlation analysis is carried out using `correlation` package, which is
    now added as a dependency.

MINOR CHANGES
  
  - All expressions now contain name of the statistical test carried out.

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
