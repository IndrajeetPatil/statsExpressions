# statsExpressions 1.3.3

MINOR CHANGES

  - When any of the necessary numeric column values are missing, for these rows
    `tidy_model_expressions()` now returns a `NULL` instead of an expression with
    empty strings.

# statsExpressions 1.3.2

NEW FUNCTIONS

  - The `pairwise_comparisons()` function to carry out pairwise comparison tests
    for one-way designs.

MAJOR CHANGES

  - Expressions with statistical details were sometimes in a column named
    `label`, while other times in `expression` column. Now it will be
    consistently in the `expression` column.

  - Additionally, glue expressions were stored parsed in some data frame
    outputs, while unparsed in others. Now it is consistently parsed.

  - The `top.text` parameter has been removed from all functions. It was
    relevant only in the context of `{ggstatsplot}` package. As that package no
    longer uses it, it is no longer necessary to retain it.

# statsExpressions 1.3.1

  - Fixes breakages due to `{insight}` package update.

# statsExpressions 1.3.0

BREAKING CHANGES

  - The `format_num()` has been removed in favor of `insight::format_value()`.

MINOR CHANGES

  - The `expr_template()` has been renamed to more informative
    `add_expression_col()` function and has a different API. It returns a
    dataframe with the additional expression column instead of just the
    expression.

# statsExpressions 1.2.0

BREAKING CHANGES

  - A number of effect size estimates and their confidence intervals have
    changed due to respective changes made in `{effectsize}` package version
    `0.5` release. For full details of these changes, see:
    <https://easystats.github.io/effectsize/news/index.html>

  - For the same reason, the effect size for one-way contingency table has
    changed from Cramer's *V* to Pearson's *C*.

NEW FUNCTIONS

  - `centrality_description()` function added to describe distribution for each
    level of a grouping variable and create an expression describing a
    centrality measure.

  - Adds new experimental function `tidy_model_expressions()` to create
    expressions for dataframes containing tidied results from regression model
    objects.

MAJOR CHANGES

  - Removes the redundant `bf_extractor` function. The `tidy_model_parameters`
    does the same thing.

  - Exports more utility functions (`long_to_wide_converter`, `format_num`,
    `stats_type_switch`) to get rid of reliance on `ipmisc` package.

  - To be consistent with the expressions, the dataframe for Bayesian analysis
    now also contain log of Bayes Factor values.

  - The `tidy_model_effectsize()` function is no longer exported as it is
    helpful only for the internal workings of the package.

  - Given that these values can be really high, the statistic values for
    non-parametric tests were shown on a log scale, but this is a highly
    non-standard practice that has caused a lot of confusion among users. In
    light of this feedback, the functions no longer return these values on a log
    scale but in a scientific notation to keep the statistical expressions
    short.

MINOR CHANGES

  - Removes `VR_dilemma` dataset, which lacked enough variation to be a good
    dataset to use in examples or tests.

# statsExpressions 1.1.0

MAJOR CHANGES

  - There is a new _JOSS_ paper about `{statsExpressions}` package!!
    <https://joss.theoj.org/papers/10.21105/joss.03236>

  - The effect size for independent trimmed means two-sample test has been
    changed from explanatory measure of effect size to AKP's delta, which is
    easier to understand and interpret since its a robust cousin of Cohen's
    *d*.

  - `one_sample_test` and `two_sample_test` gain `alternative` argument to
    specify alternative hypothesis (#86).

  - Cohen's *d* and Hedge's *g* use non-pooled standard deviation (cf.
    https://psyarxiv.com/tu6mp/).

MINOR CHANGES

  - The output dataframes now contain columns with additional information about
    how confidence intervals are computed (thanks to `effectsize` package).

# statsExpressions 1.0.1

BREAKING CHANGES

  - Retires all vestigial `expr_*` functions.

MINOR CHANGES

  - Adapts failing tests due to changes in `effectsize`.

# statsExpressions 1.0.0

This is the first **stable** release of `{statsExpressions}`!

There is good news and there is bad news that accompanies this milestone.

  - The **bad news**: The `API` for the package has changed **completely**: All
    functions return a *dataframe*, and not an *expression*, as a default. The
    expression is contained in a list column in the dataframe itself. So, to
    salvage your functions from breaking, you will have to add
    `$expression[[1]]` to your function calls. For example, if you were using
    the function `expr_t_onesample()`, you will now have to specify
    `expr_t_onesample()$expression[[1]]`, so on and so forth. But, in general,
    the advice is to **not** use any of the `expr_*` functions, which are
    vestigial names for new avatars of these function and will be removed in
    future. The new names are more intuitive, e.g., `expr_t_onesample()` is now
    called `one_sample_test()`, etc.

  - The **good news**: There will not be any new changes to any of the current
    functions, except for any change necessary for maintenance or bug squashing.
    Well, to be more precise, this is true only for the functions that have
    **"stable"** [badge](https://lifecycle.r-lib.org/articles/stages.html).

BUG FIXES

  - If the entered dataframe is `grouped`-tibble, the function internally
    ungroups this (#79).

MINOR CHANGES

  - To reduce dependency load, `afex` has moved from `Imports` to `Suggests`.

# statsExpressions 0.7.1

BREAKING CHANGES

  - To avoid confusion among users, the trimming level for all functions is now
    changed from `tr = 0.1` to `tr = 0.2` (which is what `WRS2` defaults to).

MAJOR CHANGES

  - `expr_template` gains a new argument `bayesian`, which can return an
    expression for Bayesian analysis, which has a slightly different template.
    Additionally, it has changed its conventions about the column names it
    expects.

  - Retires the additional caption-making functionality that was unique to
    `expr_meta_random` when `type = "parametric"`. This was the only context in
    which this feature was supported and was therefore inconsistent with the
    rest of the package API.

  - Removes `tidy_model_performance` function, which is no longer used
    internally.

  - Removes column containing `log` values of Bayes Factor as they are relevant
    only for expressions.

  - All meta-analysis packages move from `Imports` to `Suggests` to reduce the
    installation time for the user.

  - All robust tests in this package were based on trimmed means, except for
    correlation test. This has been changed: the robust correlation measure is
    now Winsorized correlation, which is based on trimming. Therefore, the
    `beta` argument has been replaced by `tr` argument. This should result only
    in minor changes in correlation coefficient estimates.

# statsExpressions 0.7.0

BREAKING CHANGES

  - To be consistent with `ggstatsplot`'s overall syntax philosophy the `type`
    argument can be used to specify which type of statistical approach is to be
    used for all functions.

    * `t_parametric`, `t_nonparametric`, `t_robust`, `t_bayes` are now removed
      in favor of a single function `two_sample_test`.

    * `expr_anova_parametric`, `expr_anova_nonparametric`, `expr_anova_robust`,
      `expr_anova_bayes` are now removed in favor of a single function
      `oneway_anova`.

  - `{statsExpressions}` no longer internally relies on `tidyBF`. All Bayesian
    analysis is carried out in this package itself. This was done to make the
    maintenance of this package easier and helps with some major internal code
    refactoring. As such, all re-exported functions from `tidyBF` have also been
    removed.

BUG FIXES

  - `contingency_table` ignored `ratio` argument while computing Cramer's *V*
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
    need to be entered only in `data`.

  - All meta-analyses are now carried out using `expr_meta_random` and the
    individual functions have been removed.

MAJOR CHANGES

  - All effect sizes for contingency tabs are now calculated via `effectsize`
    instead of `rcompanion`. This would lead to slight differences in effect
    sizes and their CIs but the computations will be faster. Additionally, the
    lower bound will never be negative and will be restricted to [0,1].

  - `contingency_table` function has been made less robust. It now fails instead
    of returning `NULL` when it is not supposed to work. This is done to be
    consistent with the other functions in the package which also fail instead
    of returning `NULL`.

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

  - The `t_nonparametric` subtitle now clarifies whether it's a Wilcoxon test or
    a Mann-Whitney test.

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
    Similarly, `{statsExpressions}` now relies on `effectsize` to compute effect
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

  - `expr_template` function now expects two dataframes: `data` and `effsize.df`
    that contain the details needed for creating expressions instead of
    providing each individual values. This makes the function more friendly work
    with using modeling packages like `broom`.

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

