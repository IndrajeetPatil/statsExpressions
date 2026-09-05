# Return value schema

This vignette can be cited as:

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
    [`{ggstatsplot}`](https://indrajeetpatil.github.io/ggstatsplot/)
    plots.

The rest of this article documents the `expression` column, the engine
that builds it, and then provides a schema for each exported function.
Because the precise columns vary with the test and `type`, the tables
below describe the *representative* columns you can expect; the
accompanying
[`dplyr::glimpse()`](https://pillar.r-lib.org/reference/glimpse.html)
output is the authoritative list for each call. Where a column is
specific to a particular `type`, this is called out in the notes.

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
Understanding it helps clarify why the output looks the way it does.

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

The examples below use
[`dplyr::glimpse()`](https://pillar.r-lib.org/reference/glimpse.html) so
that every returned column (and its type) is visible at a glance. The
tables list the *representative* columns; the
[`glimpse()`](https://pillar.r-lib.org/reference/glimpse.html) output is
authoritative for any given call, and type-specific differences are
noted below each table.

### `one_sample_test()`

\
[`set.seed`](https://rdrr.io/r/base/Random.html)`(``123``)`\
[`one_sample_test`](https://www.indrapatil.com/statsExpressions/reference/one_sample_test.md)`(``mtcars``, ``wt``, test.value ``=`` ``3``)`` ``|>`` ``dplyr``::`[`glimpse`](https://pillar.r-lib.org/reference/glimpse.html)`(``)`\
`#> Rows: 1`\
`#> Columns: 15`\
`#> $ mu                ``<dbl>`` 3`\
`#> $ statistic         ``<dbl>`` 1.256009`\
`#> $ df.error          ``<dbl>`` 31`\
`#> $ p.value           ``<dbl>`` 0.2184965`\
`#> $ method            ``<chr>`` "One Sample t-test"`\
`#> $ alternative       ``<chr>`` "two.sided"`\
`#> $ effectsize        ``<chr>`` "Hedges' g"`\
`#> $ estimate          ``<dbl>`` 0.2166103`\
`#> $ conf.level        ``<dbl>`` 0.95`\
`#> $ conf.low          ``<dbl>`` -0.1273401`\
`#> $ conf.high         ``<dbl>`` 0.5571645`\
`#> $ conf.method       ``<chr>`` "ncp"`\
`#> $ conf.distribution ``<chr>`` "t"`\
`#> $ n.obs             ``<int>`` 32`\
`#> $ expression        ``<list>`` <list(italic("t")["Student"] * "(" * 31 * ")" == "1.…`\
\
`# Bayesian variant returns Bayes-factor columns instead of a test statistic`\
[`set.seed`](https://rdrr.io/r/base/Random.html)`(``123``)`\
[`one_sample_test`](https://www.indrapatil.com/statsExpressions/reference/one_sample_test.md)`(``mtcars``, ``wt``, test.value ``=`` ``3``, type ``=`` ``"bayes"``)`` ``|>`` ``dplyr``::`[`glimpse`](https://pillar.r-lib.org/reference/glimpse.html)`(``)`\
`#> Rows: 1`\
`#> Columns: 16`\
`#> $ term               ``<chr>`` "Difference"`\
`#> $ effectsize         ``<chr>`` "Bayesian t-test"`\
`#> $ estimate           ``<dbl>`` 0.1922752`\
`#> $ conf.level         ``<dbl>`` 0.95`\
`#> $ conf.low           ``<dbl>`` -0.161031`\
`#> $ conf.high          ``<dbl>`` 0.5425252`\
`#> $ pd                 ``<dbl>`` 0.85875`\
`#> $ prior.distribution ``<chr>`` "cauchy"`\
`#> $ prior.location     ``<dbl>`` 0`\
`#> $ prior.scale        ``<dbl>`` 0.707`\
`#> $ bf10               ``<dbl>`` 0.3868976`\
`#> $ method             ``<chr>`` "Bayesian t-test"`\
`#> $ conf.method        ``<chr>`` "ETI"`\
`#> $ log_e_bf10         ``<dbl>`` -0.9495953`\
`#> $ n.obs              ``<int>`` 32`\
`#> $ expression         ``<list>`` <list(log[e] * (BF["01"]) == "0.95", widehat(delta)…`

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
`log_e_bf10`, and the `prior.*` columns, as the second
[`glimpse()`](https://pillar.r-lib.org/reference/glimpse.html) above
shows.

### `two_sample_test()`

\
[`set.seed`](https://rdrr.io/r/base/Random.html)`(``123``)`\
[`two_sample_test`](https://www.indrapatil.com/statsExpressions/reference/two_sample_test.md)`(``ToothGrowth``, ``supp``, ``len``)`` ``|>`` ``dplyr``::`[`glimpse`](https://pillar.r-lib.org/reference/glimpse.html)`(``)`\
`#> Rows: 1`\
`#> Columns: 18`\
`#> $ parameter1        ``<chr>`` "len"`\
`#> $ parameter2        ``<chr>`` "supp"`\
`#> $ mean.parameter1   ``<dbl>`` 20.66333`\
`#> $ mean.parameter2   ``<dbl>`` 16.96333`\
`#> $ statistic         ``<dbl>`` 1.915268`\
`#> $ df.error          ``<dbl>`` 55.30943`\
`#> $ p.value           ``<dbl>`` 0.06063451`\
`#> $ method            ``<chr>`` "Welch Two Sample t-test"`\
`#> $ alternative       ``<chr>`` "two.sided"`\
`#> $ effectsize        ``<chr>`` "Hedges' g"`\
`#> $ estimate          ``<dbl>`` 0.4877788`\
`#> $ conf.level        ``<dbl>`` 0.95`\
`#> $ conf.low          ``<dbl>`` -0.02168715`\
`#> $ conf.high         ``<dbl>`` 0.9929802`\
`#> $ conf.method       ``<chr>`` "ncp"`\
`#> $ conf.distribution ``<chr>`` "t"`\
`#> $ n.obs             ``<int>`` 60`\
`#> $ expression        ``<list>`` <list(italic("t")["Welch"] * "(" * 55.31 * ")" == "1…`

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

\
[`set.seed`](https://rdrr.io/r/base/Random.html)`(``123``)`\
[`oneway_anova`](https://www.indrapatil.com/statsExpressions/reference/oneway_anova.md)`(``iris``, ``Species``, ``Sepal.Length``)`` ``|>`` ``dplyr``::`[`glimpse`](https://pillar.r-lib.org/reference/glimpse.html)`(``)`\
`#> Rows: 1`\
`#> Columns: 14`\
`#> $ statistic         ``<dbl>`` 138.9083`\
`#> $ df                ``<dbl>`` 2`\
`#> $ df.error          ``<dbl>`` 92.21115`\
`#> $ p.value           ``<dbl>`` 1.505059e-28`\
`#> $ method            ``<chr>`` "One-way analysis of means (not assuming equal varia…`\
`#> $ effectsize        ``<chr>`` "Omega2"`\
`#> $ estimate          ``<dbl>`` 0.7433854`\
`#> $ conf.level        ``<dbl>`` 0.95`\
`#> $ conf.low          ``<dbl>`` 0.6708128`\
`#> $ conf.high         ``<dbl>`` 1`\
`#> $ conf.method       ``<chr>`` "ncp"`\
`#> $ conf.distribution ``<chr>`` "F"`\
`#> $ n.obs             ``<int>`` 150`\
`#> $ expression        ``<list>`` <list(italic("F")["Welch"](2, 92.21) == "138.91", it…`

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

\
[`set.seed`](https://rdrr.io/r/base/Random.html)`(``123``)`\
[`corr_test`](https://www.indrapatil.com/statsExpressions/reference/corr_test.md)`(``mtcars``, ``wt``, ``mpg``)`` ``|>`` ``dplyr``::`[`glimpse`](https://pillar.r-lib.org/reference/glimpse.html)`(``)`\
`#> Rows: 1`\
`#> Columns: 14`\
`#> $ parameter1  ``<chr>`` "wt"`\
`#> $ parameter2  ``<chr>`` "mpg"`\
`#> $ effectsize  ``<chr>`` "Pearson correlation"`\
`#> $ estimate    ``<dbl>`` -0.8676594`\
`#> $ conf.level  ``<dbl>`` 0.95`\
`#> $ conf.low    ``<dbl>`` -0.9338264`\
`#> $ conf.high   ``<dbl>`` -0.7440872`\
`#> $ statistic   ``<dbl>`` -9.559044`\
`#> $ df.error    ``<int>`` 30`\
`#> $ p.value     ``<dbl>`` 1.293959e-10`\
`#> $ method      ``<chr>`` "Pearson correlation"`\
`#> $ n.obs       ``<int>`` 32`\
`#> $ conf.method ``<chr>`` "normal"`\
`#> $ expression  ``<list>`` <list(italic("t")["Student"] * "(" * 30 * ")" == "-9.56", …`

| Column | Description |
|:---|:---|
| `parameter1`, `parameter2` | the two correlated variable names |
| `effectsize`, `estimate` | name (e.g. Pearson’s *r*) and value of the correlation |
| `conf.level`, `conf.low`, `conf.high`, `conf.method` | interval details |
| `statistic`, `df.error`, `p.value` | test statistic, degrees of freedom, and *p*-value |
| `n.obs`, `expression` | sample size and the plotmath expression |

The Bayesian variant adds `bf10`, `log_e_bf10`, `pd`, `rope.percentage`,
and the `prior.*` columns.

### `contingency_table()`

\
`# two-way association test`\
[`set.seed`](https://rdrr.io/r/base/Random.html)`(``123``)`\
[`contingency_table`](https://www.indrapatil.com/statsExpressions/reference/contingency_table.md)`(``mtcars``, ``am``, ``vs``)`` ``|>`` ``dplyr``::`[`glimpse`](https://pillar.r-lib.org/reference/glimpse.html)`(``)`\
`#> Rows: 1`\
`#> Columns: 13`\
`#> $ statistic         ``<dbl>`` 0.9068826`\
`#> $ df                ``<int>`` 1`\
`#> $ p.value           ``<dbl>`` 0.3409429`\
`#> $ method            ``<chr>`` "Pearson's Chi-squared test"`\
`#> $ effectsize        ``<chr>`` "Cramer's V (adj.)"`\
`#> $ estimate          ``<dbl>`` 0`\
`#> $ conf.level        ``<dbl>`` 0.95`\
`#> $ conf.low          ``<dbl>`` 0`\
`#> $ conf.high         ``<dbl>`` 0.4902678`\
`#> $ conf.method       ``<chr>`` "ncp"`\
`#> $ conf.distribution ``<chr>`` "chisq"`\
`#> $ n.obs             ``<int>`` 32`\
`#> $ expression        ``<list>`` <list(chi["Pearson"]^2 * "(" * 1 * ")" == "0.91", it…`\
\
`# one-way goodness-of-fit test`\
[`set.seed`](https://rdrr.io/r/base/Random.html)`(``123``)`\
[`contingency_table`](https://www.indrapatil.com/statsExpressions/reference/contingency_table.md)`(`[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html)`(``HairEyeColor``)``, ``Eye``, counts ``=`` ``Freq``)`` ``|>`` ``dplyr``::`[`glimpse`](https://pillar.r-lib.org/reference/glimpse.html)`(``)`\
`#> Rows: 1`\
`#> Columns: 13`\
`#> $ statistic         ``<dbl>`` 133.473`\
`#> $ df                ``<dbl>`` 3`\
`#> $ p.value           ``<dbl>`` 9.65088e-29`\
`#> $ method            ``<chr>`` "Chi-squared test for given probabilities"`\
`#> $ effectsize        ``<chr>`` "Pearson's C"`\
`#> $ estimate          ``<dbl>`` 0.4289296`\
`#> $ conf.level        ``<dbl>`` 0.95`\
`#> $ conf.low          ``<dbl>`` 0.3636286`\
`#> $ conf.high         ``<dbl>`` 0.4833173`\
`#> $ conf.method       ``<chr>`` "ncp"`\
`#> $ conf.distribution ``<chr>`` "chisq"`\
`#> $ n.obs             ``<int>`` 592`\
`#> $ expression        ``<list>`` <list(chi["gof"]^2 * "(" * 3 * ")" == "133.47", ital…`

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

\
[`set.seed`](https://rdrr.io/r/base/Random.html)`(``123``)`\
[`pairwise_comparisons`](https://www.indrapatil.com/statsExpressions/reference/pairwise_comparisons.md)`(`\
`  ``mtcars``, ``cyl``, ``wt``,`\
`  type ``=`` ``"nonparametric"``,`\
`  p.adjust.method ``=`` ``"none"`\
`)`` ``|>`` ``dplyr``::`[`glimpse`](https://pillar.r-lib.org/reference/glimpse.html)`(``)`\
`#> Rows: 3`\
`#> Columns: 9`\
`#> $ group1          ``<chr>`` "4"``, ``"4"``, ``"6"`\
`#> $ group2          ``<chr>`` "6"``, ``"8"``, ``"8"`\
`#> $ statistic       ``<dbl>`` 1.836260``, ``4.755942``, ``2.221605`\
`#> $ p.value         ``<dbl>`` 6.631922e-02``, ``1.975232e-06``, ``2.631000e-02`\
`#> $ alternative     ``<chr>`` "two.sided"``, ``"two.sided"``, ``"two.sided"`\
`#> $ distribution    ``<chr>`` "z"``, ``"z"``, ``"z"`\
`#> $ p.adjust.method ``<chr>`` "None"``, ``"None"``, ``"None"`\
`#> $ test            ``<chr>`` "Dunn"``, ``"Dunn"``, ``"Dunn"`\
`#> $ expression      ``<list>`` <list(italic(p)[unadj.] == "0.07")>``, ``<list(italic(p)[u…`

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
The Bayesian variant additionally returns `estimate`, `bf10`,
`log_e_bf10`, and the `prior.*` columns.

### `pairwise_contingency_table()`

\
[`set.seed`](https://rdrr.io/r/base/Random.html)`(``123``)`\
[`pairwise_contingency_table`](https://www.indrapatil.com/statsExpressions/reference/pairwise_contingency_table.md)`(``mtcars``, ``cyl``, ``am``, p.adjust.method ``=`` ``"holm"``)`` ``|>`` ``dplyr``::`[`glimpse`](https://pillar.r-lib.org/reference/glimpse.html)`(``)`\
`#> Rows: 3`\
`#> Columns: 14`\
`#> $ group1            ``<chr>`` "4"``, ``"4"``, ``"6"`\
`#> $ group2            ``<chr>`` "6"``, ``"8"``, ``"8"`\
`#> $ p.value           ``<dbl>`` 0.332202112``, ``0.005138646``, ``0.280013760`\
`#> $ p.value.adj       ``<dbl>`` 0.56002752``, ``0.01541594``, ``0.56002752`\
`#> $ estimate          ``<dbl>`` 0.1797189``, ``0.5678175``, ``0.2294157`\
`#> $ conf.level        ``<dbl>`` 0.95``, ``0.95``, ``0.95`\
`#> $ conf.low          ``<dbl>`` 0``, ``0``, ``0`\
`#> $ conf.high         ``<dbl>`` 0.7431410``, ``0.9834544``, ``0.7279561`\
`#> $ effectsize        ``<chr>`` "Cramer's V (adj.)"``, ``"Cramer's V (adj.)"``, ``"Cramer's …`\
`#> $ conf.method       ``<chr>`` "ncp"``, ``"ncp"``, ``"ncp"`\
`#> $ conf.distribution ``<chr>`` "chisq"``, ``"chisq"``, ``"chisq"`\
`#> $ p.adjust.method   ``<chr>`` "Holm"``, ``"Holm"``, ``"Holm"`\
`#> $ test              ``<chr>`` "Fisher's exact test"``, ``"Fisher's exact test"``, ``"Fishe…`\
`#> $ expression        ``<list>`` <list(italic(p)["Holm" - adj.] == "0.56")>``, ``<list(it…`

| Column | Description |
|:---|:---|
| `group1`, `group2` | the two levels being compared |
| `p.value`, `p.value.adj` | unadjusted and multiplicity-adjusted *p*-values |
| `effectsize`, `estimate`, `conf.*` | Cramer’s *V* effect size and its interval |
| `p.adjust.method`, `test`, `expression` | adjustment method, test, and plotmath expression |

### `meta_analysis()`

\
`` # renaming columns to `{statsExpressions}` conventions ``\
[`data`](https://rdrr.io/r/utils/data.html)`(``mag``, package ``=`` ``"metaplus"``)`\
`df`` ``<-`` ``dplyr``::`[`rename`](https://dplyr.tidyverse.org/reference/rename.html)`(``mag``, estimate ``=`` ``yi``, std.error ``=`` ``sei``)`\
\
[`set.seed`](https://rdrr.io/r/base/Random.html)`(``123``)`\
[`meta_analysis`](https://www.indrapatil.com/statsExpressions/reference/meta_analysis.md)`(``df``)`` ``|>`` ``dplyr``::`[`glimpse`](https://pillar.r-lib.org/reference/glimpse.html)`(``)`\
`#> Rows: 1`\
`#> Columns: 14`\
`#> $ term        ``<chr>`` "Overall"`\
`#> $ effectsize  ``<chr>`` "meta-analytic summary estimate"`\
`#> $ estimate    ``<dbl>`` -0.7665893`\
`#> $ std.error   ``<dbl>`` 0.2118008`\
`#> $ conf.level  ``<dbl>`` 0.95`\
`#> $ conf.low    ``<dbl>`` -1.181711`\
`#> $ conf.high   ``<dbl>`` -0.3514672`\
`#> $ statistic   ``<dbl>`` -3.619387`\
`#> $ p.value     ``<dbl>`` 0.0002953014`\
`#> $ weight      ``<dbl>`` ``NA`\
`#> $ method      ``<chr>`` "Meta-analysis using 'metafor'"`\
`#> $ conf.method ``<chr>`` "Wald"`\
`#> $ n.obs       ``<int>`` 16`\
`#> $ expression  ``<list>`` <list(italic("z") == "-3.62", italic(p) == "2.95e-04", wid…`

| Column | Description |
|:---|:---|
| `term`, `effectsize` | the summary term and effect-size label |
| `estimate`, `std.error` | meta-analytic summary estimate and its standard error |
| `conf.level`, `conf.low`, `conf.high`, `conf.method` | interval details |
| `statistic`, `p.value`, `weight` | test statistic, *p*-value, and study weight |
| `n.obs`, `expression` | number of studies and the plotmath expression |

The Bayesian variant additionally returns MCMC diagnostics and
Bayes-factor columns: `bf10`, `log_e_bf10`, `rhat`, `ess`, `component`,
and the `prior.*` columns.

### `centrality_description()`

\
[`set.seed`](https://rdrr.io/r/base/Random.html)`(``123``)`\
[`centrality_description`](https://www.indrapatil.com/statsExpressions/reference/centrality_description.md)`(``iris``, ``Species``, ``Sepal.Length``)`` ``|>`` ``dplyr``::`[`glimpse`](https://pillar.r-lib.org/reference/glimpse.html)`(``)`\
`#> Rows: 3`\
`#> Columns: 14`\
`#> $ Species      ``<fct>`` setosa``, ``versicolor``, ``virginica`\
`#> $ Sepal.Length ``<dbl>`` 5.006``, ``5.936``, ``6.588`\
`#> $ std.dev      ``<dbl>`` 0.3524897``, ``0.5161711``, ``0.6358796`\
`#> $ iqr          ``<dbl>`` 0.40``, ``0.70``, ``0.75`\
`#> $ conf.low     ``<dbl>`` 4.90390``, ``5.80370``, ``6.39065`\
`#> $ conf.high    ``<dbl>`` 5.09925``, ``6.07105``, ``6.79030`\
`#> $ min          ``<dbl>`` 4.3``, ``4.9``, ``4.9`\
`#> $ max          ``<dbl>`` 5.8``, ``7.0``, ``7.9`\
`#> $ skewness     ``<dbl>`` 0.1200870``, ``0.1053776``, ``0.1180151`\
`#> $ kurtosis     ``<dbl>`` -0.25268880``, ``-0.53300954``, ``0.03290442`\
`#> $ n.obs        ``<int>`` 50``, ``50``, ``50`\
`#> $ missing.obs  ``<int>`` 0``, ``0``, ``0`\
`#> $ expression   ``<list>`` <list(widehat(mu)[mean] == "5.01")>``, ``<list(widehat(mu)[me…`\
`#> $ n.expression ``<chr>`` "setosa\n(n = 50)"``, ``"versicolor\n(n = 50)"``, ``"virginica\n(…`

| Column | Description |
|:---|:---|
| `<x>` (e.g. `Species`) | the grouping variable |
| `<y>` (e.g. `Sepal.Length`) | the requested centrality measure (mean/median/trimmed mean/MAP) |
| `std.dev` or `mad`, `iqr`, `min`, `max`, `skewness`, `kurtosis` | dispersion and shape statistics |
| `conf.low`, `conf.high` | interval for the centrality estimate |
| `n.obs`, `missing.obs` | count of observations and missing values |
| `expression` | plotmath expression with the centrality estimate |
| `n.expression` | a label combining the group name and its sample size (handy as a facet/axis label) |

The dispersion column depends on `type`: `std.dev` for `"parametric"`
and `"robust"`, `mad` (median absolute deviation) for `"nonparametric"`,
and neither for `"bayes"`. This is the only function that returns an
`n.expression` column.

## Suggestions

If you find any bugs or have any suggestions/remarks, please file an
issue on GitHub:
<https://github.com/IndrajeetPatil/statsExpressions/issues>
