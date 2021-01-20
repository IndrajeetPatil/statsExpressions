# statsExpressions 0.7.0.9000

MAJOR CHANGES

  - `expr_template` gains a new argument `bayesian`, which can return an
    expression for Bayesian analysis, which has a slightly different template.

# statsExpressions 0.7.0

BREAKING CHANGES

  - To be consistent with `ggstatsplot`'s overall syntax philosophy the `type`
    argument can be used to specify which type of statistical approach is to be
    used for all functions.

    * `expr_t_parametric`, `expr_t_nonparametric`, `expr_t_robust`,
      `expr_t_bayes` are now removed in favor of a single function
      `expr_t_twosample`.

    * `expr_anova_parametric`, `expr_anova_nonparametric`, `expr_anova_robust`,
      `expr_anova_bayes` are now removed in favor of a single function
      `expr_oneway_anova`.

  - `statsExpressions` no longer internally relies on `tidyBF`. All Bayesian
    analysis is carried out in this package itself. This was done to make the
    maintenance of this package easier and helps with some major internal code
    refactoring. As such, all re-exported functions from `tidyBF` have also been
    removed.

BUG FIXES

  - `expr_contingency_tab` ignored `ratio` argument while computing Cramer's *V*
    for one-sample test. This is fixed.

MAJOR CHANGES

  - All non-parametric functions now use `effectsize` package to compute effect
    sizes and not `rcompanion`. This would lead to some changes in effect sizes
    and their confidence intervals reported by the respective functions.

  - Robust one-sample test is changed from one-sample percentile bootstrap to
    bootstrap-*t* method for one-sample test, which uses trimmed mean like the
    rest of the robust functions in this package.

MINOR CHANGES

  - Package internally relies on `afex` instead of `ez` for within-subjects
    ANOVA.

  - `expr_template` gains `paired` argument.

# statsExpressions 0.6.2

MINOR CHANGES

  - Internal refactoring to catch up with changes made to `effectsize`. Tests
    are adapted to these changes as well.

  - Sample size information in expressions is pretty-formatted.

# statsExpressions 0.6.1

MAJOR CHANGES

  - Adds two new helper functions: `tidy_model_parameters` and
    `tidy_model_performance` to toggle between `easystats` and `tidymodels`
    naming conventions.

  - Drops `broomExtra` from dependencies in favor of `parameters` +
    `performance`.

  - Removes the unused and vestigial `Titanic_full` dataset.

# statsExpressions 0.6.0

BREAKING CHANGES

  - Removes the alias `expr_onesample_proptest`.

  - The `expr_template` function retires `effsize.df` argument. Now all details
    need to be entered only in `stats.df`.

  - All meta-analyses are now carried out using `expr_meta_random` and the
    individual functions have been removed.

MAJOR CHANGES

  - All effect sizes for contingency tabs are now calculated via `effectsize`
    instead of `rcompanion`. This would lead to slight differences in effect
    sizes and their CIs but the computations will be faster. Additionally, the
    lower bound will never be negative and will be restricted to [0,1].

  - `expr_contingency_tab` function has been made less robust. It now fails
    instead of returning `NULL` when it is not supposed to work. This is done to
    be consistent with the other functions in the package which also fail
    instead of returning `NULL`.

  - `expr_anova_parametric` always applies sphericity correction for *p*-values
    for repeated measures ANOVA.

  - `expr_anova_parametric` retires non-partial variants of effect sizes
    (eta-squared and omega-squared, i.e.) for parametric analyses.

  - The *t*-test and ANOVA tests get `subject.id` argument relevant for repeated
    measures design.

MINOR CHANGES

  - Retires the vestigial `stat.title` argument. It was originally intended to
    give more info on the tests, but now the expressions themselves contain
    these details.

  - For paired ANOVA designs, `partial = TRUE` is recognized by effect sizes.

  - Retires `bias.correct` argument for contingency table analysis. It is rarely
    justifiable to use the biased version of Cramer's *V*.

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
    changing `conf.level` doesn't work and function defaults to `0.90` CIs
    (#32).

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

