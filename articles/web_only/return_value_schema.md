# Return value schema

## Introduction

Every exported analysis function in
[statsExpressions](https://www.indrapatil.com/statsExpressions/) returns
a **tidy tibble** (with the additional class `"statsExpressions"`)
rather than a model object. This means the output is immediately
pipe-friendly and ready for downstream use in data wrangling or graphing
pipelines.

Two design principles are worth keeping in mind while reading this
article:

1.  **The exact columns depend on the analysis.** All functions follow
    the same tidy/[`{broom}`](https://broom.tidymodels.org/)-style
    naming conventions, but the precise set of columns returned depends
    on the test and, for functions that accept a `type` argument, on the
    chosen analysis (`"parametric"`, `"nonparametric"`, `"robust"`, or
    `"bayes"`). For example, most Bayesian analyses return Bayes-factor
    columns (`bf10`, and usually `log_e_bf10`) that frequentist analyses
    do not.

2.  **The `expression` column is the core deliverable.** Regardless of
    the test, almost every function returns a pre-formatted `expression`
    column intended for annotating plots. This is what powers the
    statistical details you see in
    [`{ggstatsplot}`](https://www.indrapatil.com/ggstatsplot/) plots.

This article explains the `expression` column and lists representative
output columns for each analysis function. For worked examples, see
[Data frame
outputs](https://www.indrapatil.com/statsExpressions/articles/web_only/dataframe_outputs.md);
for the meaning of the tests and effect sizes, see [Test and effect size
details](https://www.indrapatil.com/statsExpressions/articles/stats_details.md).

## The `expression` column

The `expression` column is a **list-column**: each element is a
[plotmath](https://rdrr.io/r/grDevices/plotmath.html) `language` object
(not a character string), which is why it can render mathematical
notation (subscripts, Greek letters, italicized statistics) when used as
a plot label. Extract a single element with `[[` before using it.

\
[`set.seed`](https://rdrr.io/r/base/Random.html)`(``123``)`\
`res`` ``<-`` `[`two_sample_test`](https://www.indrapatil.com/statsExpressions/reference/two_sample_test.md)`(``ToothGrowth``, ``supp``, ``len``)`\
\
`` # the column is a list; each element is a `language` object, not a string ``\
[`class`](https://rdrr.io/r/base/class.html)`(``res``$``expression``)`\
`#> [1] "list"`\
[`class`](https://rdrr.io/r/base/class.html)`(``res``$``expression``[[``1``]``]``)`\
`#> [1] "call"`

Because each element is a `language` object, it can be dropped directly
into any [ggplot2](https://ggplot2.tidyverse.org) layer that accepts a
plotmath expression, such as
[`labs()`](https://ggplot2.tidyverse.org/reference/labs.html),
[`annotate()`](https://ggplot2.tidyverse.org/reference/annotate.html),
or [`ggtitle()`](https://ggplot2.tidyverse.org/reference/labs.html):

\
[`library`](https://rdrr.io/r/base/library.html)`(`[`ggplot2`](https://ggplot2.tidyverse.org)`)`\
\
[`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)`(``ToothGrowth``, `[`aes`](https://ggplot2.tidyverse.org/reference/aes.html)`(``supp``, ``len``)``)`` ``+`\
`  `[`geom_boxplot`](https://ggplot2.tidyverse.org/reference/geom_boxplot.html)`(``)`` ``+`\
`  `[`labs`](https://ggplot2.tidyverse.org/reference/labs.html)`(``subtitle ``=`` ``res``$``expression``[[``1``]``]``)`

## The `add_expression_col()` engine

Most of the `expression` columns are produced by a single internal
engine:
[`add_expression_col()`](https://www.indrapatil.com/statsExpressions/reference/add_expression_col.md).
It is the main path for the hypothesis-testing functions
([`one_sample_test()`](https://www.indrapatil.com/statsExpressions/reference/one_sample_test.md),
[`two_sample_test()`](https://www.indrapatil.com/statsExpressions/reference/two_sample_test.md),
[`oneway_anova()`](https://www.indrapatil.com/statsExpressions/reference/oneway_anova.md),
[`corr_test()`](https://www.indrapatil.com/statsExpressions/reference/corr_test.md),
[`contingency_table()`](https://www.indrapatil.com/statsExpressions/reference/contingency_table.md),
[`meta_analysis()`](https://www.indrapatil.com/statsExpressions/reference/meta_analysis.md)).
The one exception is the Bayesian one-way (goodness-of-fit)
[`contingency_table()`](https://www.indrapatil.com/statsExpressions/reference/contingency_table.md)
analysis, which builds its own minimal expression. Understanding it
helps clarify why the output looks the way it does.

The engine works as follows:

- It consumes a tidy data frame of statistical details – ideally the
  output of
  [`tidy_model_parameters()`](https://www.indrapatil.com/statsExpressions/reference/tidy_model_parameters.md),
  which standardizes the results from the various modeling backends
  ([effectsize](https://easystats.github.io/effectsize/),
  [parameters](https://easystats.github.io/parameters/),
  [BayesFactor](https://richarddmorey.github.io/BayesFactor/), etc.)
  into consistent column names.

- It inspects the available columns to decide which expression
  *template* to use. The key branch points are:

  - whether the analysis is **Bayesian** (detected via the presence of a
    `bf10` column), and

  - the number of **degrees-of-freedom parameters** present (`0`, `1`,
    or `2`), which determines whether the statistic is displayed as,
    e.g., `t = ...`, `t(df) = ...`, or `F(df1, df2) = ...`.

- It then formats the numeric values (respecting the `digits`,
  `digits.df`, and `digits.df.error` arguments), assembles the plotmath
  string via [`{glue}`](https://glue.tidyverse.org/), converts it to a
  `language` object, and appends it as the `expression` column.

Not every function routes through this engine.
[`centrality_description()`](https://www.indrapatil.com/statsExpressions/reference/centrality_description.md),
[`pairwise_comparisons()`](https://www.indrapatil.com/statsExpressions/reference/pairwise_comparisons.md),
and
[`pairwise_contingency_table()`](https://www.indrapatil.com/statsExpressions/reference/pairwise_contingency_table.md)
build their own [glue](https://glue.tidyverse.org/) templates and call
the shared `.glue_to_expression()` helper directly to attach the
`expression` column. The end result is the same kind of list-column of
plotmath expressions, but the assembly path is specialized rather than
going through
[`add_expression_col()`](https://www.indrapatil.com/statsExpressions/reference/add_expression_col.md).

> **Note:**
> [`add_expression_col()`](https://www.indrapatil.com/statsExpressions/reference/add_expression_col.md)
> is exported so that its behavior is documented and can be inspected,
> but it is **not stable** and is intended for internal use only. Its
> signature and behavior may change without a deprecation cycle. Build
> downstream code against the *output columns* documented here rather
> than by calling this function directly.

## Per-function schema

The tables list representative columns, with type-specific differences
noted below each table. Use `names(result)` or `dplyr::glimpse(result)`
to inspect the exact columns returned by a particular call and package
version. `conf.*` and `prior.*` below abbreviate the available interval
and prior columns.

### `one_sample_test()`

| Column | Description |
|:---|:---|
| `mu` | the null value the mean is tested against (`test.value`) |
| `statistic`, `df.error`, `p.value` | test statistic, its degrees of freedom, and *p*-value (non-Bayesian) |
| `effectsize`, `estimate` | name and value of the effect size (e.g. Hedges’ *g*) |
| `conf.level`, `conf.low`, `conf.high`, `conf.method`, `conf.distribution` | interval details for the effect size |
| `bf10`, `log_e_bf10`, `prior.*` | Bayes factor, its log, and the prior (Bayesian only) |
| `n.obs` | number of observations |
| `expression` | plotmath expression with the test details |

The Bayesian variant (`type = "bayes"`) replaces
`statistic`/`df.error`/`p.value` with `term`, `pd`, `bf10`,
`log_e_bf10`, and the `prior.*` columns.

### `two_sample_test()`

| Column | Description |
|:---|:---|
| `parameter1`, `parameter2` | the outcome and grouping variable names |
| `mean.parameter1`, `mean.parameter2` | group means (parametric between-subjects) |
| `statistic`, `df.error`, `p.value` | test statistic, its degrees of freedom, and *p*-value |
| `effectsize`, `estimate`, `conf.*` | effect size and its interval |
| `n.obs`, `expression` | sample size and the plotmath expression |

As with
[`one_sample_test()`](https://www.indrapatil.com/statsExpressions/reference/one_sample_test.md),
the Bayesian variant returns `term`, `pd`, `bf10`, `log_e_bf10`, and
`prior.*` in place of the frequentist test statistic and *p*-value.

### `oneway_anova()`

| Column | Description |
|:---|:---|
| `statistic`, `df`, `df.error`, `p.value` | *F*-statistic with its two degrees of freedom, and *p*-value |
| `effectsize`, `estimate`, `conf.*` | effect size (e.g. omega-squared) and its interval |
| `n.obs`, `expression` | sample size and the plotmath expression |

For a two-degrees-of-freedom statistic like ANOVA, note that **both**
`df` and `df.error` are present. The non-parametric variant
(`type = "np"`) instead returns a single-parameter statistic with
`parameter1`/`parameter2` and a `conf.iterations` column (the number of
bootstrap iterations), while the Bayesian variant returns the usual
`bf10`/`log_e_bf10`/`prior.*` columns.

### `corr_test()`

| Column | Description |
|:---|:---|
| `parameter1`, `parameter2` | the two correlated variable names |
| `effectsize`, `estimate` | name (e.g. Pearson’s *r*) and value of the correlation |
| `conf.level`, `conf.low`, `conf.high`, `conf.method` | interval details |
| `statistic`, `df.error`, `p.value` | test statistic, degrees of freedom, and *p*-value |
| `n.obs`, `expression` | sample size and the plotmath expression |

The Bayesian variant returns `bf10`, `pd`, `rope.percentage`, and the
`prior.*` columns in place of `statistic`, `df.error`, and `p.value`.
Unlike the Bayesian *t*-tests, it does not return `log_e_bf10`.

### `contingency_table()`

| Column | Description |
|:---|:---|
| `statistic`, `df`, `p.value` | chi-squared statistic, its degrees of freedom, and *p*-value |
| `effectsize`, `estimate`, `conf.*` | effect size (e.g. Cramer’s *V*) and its interval |
| `bf10`, `prior.scale` | Bayes factor and prior (Bayesian only) |
| `n.obs`, `expression` | sample size and the plotmath expression |

The two Bayesian paths differ in their columns. The **two-way** Bayesian
analysis returns the full set of Bayesian columns (`term`, `bf10`,
`log_e_bf10`, `prior.distribution`/`prior.location`/`prior.scale`, plus
the effect-size interval). The **one-way** (goodness-of-fit) Bayesian
analysis is more minimal, returning only `bf10`, `prior.scale`,
`method`, and `expression` – in particular it does **not** include a
`log_e_bf10` column.

### `pairwise_comparisons()`

| Column | Description |
|:---|:---|
| `group1`, `group2` | the two levels being compared |
| `statistic`, `p.value` | test statistic and the (adjusted) *p*-value |
| `p.adjust.method`, `test` | multiple-comparison adjustment method and the test used |
| `expression` | plotmath expression with the (adjusted) *p*-value |

Unlike the single-test functions, this returns **one row per pairwise
comparison**. Note that the adjusted *p*-value is folded directly into
the `p.value` column here (there is no separate `p.value.adj` column,
unlike in
[`pairwise_contingency_table()`](https://www.indrapatil.com/statsExpressions/reference/pairwise_contingency_table.md)).
The Bayesian variant returns posterior estimates, intervals, `bf10`,
`log_e_bf10`, and `prior.*` columns instead of `statistic`, `p.value`,
and `p.adjust.method`; no *p*-value adjustment is applied to Bayes
factors.

### `pairwise_contingency_table()`

| Column | Description |
|:---|:---|
| `group1`, `group2` | the two levels being compared |
| `p.value`, `p.value.adj` | unadjusted and multiplicity-adjusted *p*-values |
| `effectsize`, `estimate`, `conf.*` | Cramer’s *V* effect size and its interval |
| `p.adjust.method`, `test`, `expression` | adjustment method, test, and plotmath expression |

### `meta_analysis()`

| Column | Description |
|:---|:---|
| `term`, `effectsize` | the summary term and effect-size label |
| `estimate`, `std.error` | meta-analytic summary estimate and its standard error |
| `conf.level`, `conf.low`, `conf.high`, `conf.method` | interval details |
| `statistic`, `p.value`, `weight` | test statistic, *p*-value, and summary weight (may be `NA`) |
| `n.obs`, `expression` | number of studies and the plotmath expression |

The Bayesian variant additionally returns MCMC diagnostics and
Bayes-factor columns: `bf10`, `log_e_bf10`, `rhat`, `ess`, `component`,
and the `prior.*` columns.

### `centrality_description()`

| Column | Description |
|:---|:---|
| `<x>` (e.g. `Species`) | the grouping variable |
| `<y>` (e.g. `Sepal.Length`) | the requested centrality measure (mean/median/trimmed mean/MAP) |
| `std.dev` or `mad`, `iqr`, `min`, `max`, `skewness`, `kurtosis` | dispersion and shape statistics |
| `conf.low`, `conf.high` | interval for the centrality estimate |
| `n.obs`, `missing.obs` | retained observations and missing values after filtering (see below) |
| `expression` | plotmath expression with the centrality estimate |
| `n.expression` | a label combining the group name and its sample size (handy as a facet/axis label) |

The dispersion column depends on `type`: `std.dev` for `"parametric"`
and `"robust"`, `mad` (median absolute deviation) for `"nonparametric"`,
and neither for `"bayes"`. Rows with missing `x` or `y` are removed
before the summaries are computed, so `missing.obs` is `0` rather than
the number of missing values in the original input. This is the only
function that returns an `n.expression` column.

## Suggestions

If you find any bugs or have any suggestions/remarks, please file an
issue on GitHub:
<https://github.com/IndrajeetPatil/statsExpressions/issues>

## Citation

    To cite package 'statsExpressions' in publications use:

      Patil, I., (2021). statsExpressions: R Package for Tidy Dataframes
      and Expressions with Statistical Details. Journal of Open Source
      Software, 6(61), 3236, https://doi.org/10.21105/joss.03236

    A BibTeX entry for LaTeX users is

      @Article{,
        doi = {10.21105/joss.03236},
        year = {2021},
        publisher = {{The Open Journal}},
        volume = {6},
        number = {61},
        pages = {3236},
        author = {Indrajeet Patil},
        title = {{statsExpressions: {R} Package for Tidy Dataframes and Expressions with Statistical Details}},
        journal = {{Journal of Open Source Software}},
      }
