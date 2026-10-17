# `{statsExpressions}`: Tidy dataframes and expressions with statistical details

[![Total
downloads](https://cranlogs.r-pkg.org/badges/grand-total/statsExpressions?color=blue)](https://CRAN.R-project.org/package=statsExpressions)
[![lifecycle](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![DOI](https://joss.theoj.org/papers/10.21105/joss.03236/status.svg)](https://doi.org/10.21105/joss.03236)

# Introduction

The [statsExpressions](https://www.indrapatil.com/statsExpressions/)
package has two key aims:

- to provide a consistent syntax to do statistical analysis with tidy
  data (in a pipe-friendly manner),
- to provide statistical expressions (pre-formatted in-text statistical
  results) for plotting functions.

It covers common statistical tests (*t*-tests, one-way ANOVA,
correlation analyses, contingency table analyses, and meta-analyses)
with parametric, nonparametric, robust, and Bayesian approaches,
depending on the test.

Statistical packages exhibit substantial diversity in terms of their
syntax and expected input and output data type. For example, some
functions expect vectors as inputs, while others expect data frames.
Depending on whether it is a repeated measures design or not, functions
from the same package might expect data to be in wide or long format.
Some functions can internally omit missing values, while other functions
error in their presence. Furthermore, the objects returned by test
functions might not have all required information (e.g., degrees of
freedom, significance, Bayes factor, etc.) accessible in a consistent
data type: depending on the function, details may be returned as a list,
a matrix, an array, or a data frame. This diversity makes it difficult
to access all needed information for hypothesis testing and estimation,
to use the results downstream in a workflow, and to switch from one
statistical approach to another.

This is where
[statsExpressions](https://www.indrapatil.com/statsExpressions/) comes
in: It can be thought of as a unified portal through which most of the
functionality in these underlying packages can be accessed, with a
simpler interface and no requirement to change data format.

This package forms the statistical processing backend for the
[`ggstatsplot`](https://www.indrapatil.com/ggstatsplot/) package.

For more documentation, see the dedicated
[website](https://www.indrapatil.com/statsExpressions/).

# Installation

Note: This package supports R-devel, the current R release, and the
previous R release.

| Type        | Command                                       |
|:------------|:----------------------------------------------|
| Release     | `install.packages("statsExpressions")`        |
| Development | `pak::pak("IndrajeetPatil/statsExpressions")` |

On Linux,
[statsExpressions](https://www.indrapatil.com/statsExpressions/)
installation may require additional system dependencies, which can be
checked using:

\
`pak``::`[`pkg_sysreqs`](https://pak.r-lib.org/reference/pkg_sysreqs.html)`(``"statsExpressions"``)`

# Citation

The package can be cited as:

``` r
citation("statsExpressions")
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
```

# General workflow

![Diagram showing the general workflow of the statsExpressions
package](reference/figures/card.png)

# Summary of functionality

| Function | Analysis | Parametric | Non-parametric | Robust | Bayesian |
|:---|:---|:---|:---|:---|:---|
| [`one_sample_test()`](https://www.indrapatil.com/statsExpressions/reference/one_sample_test.md) | one-sample test | ✅ | ✅ | ✅ | ✅ |
| [`two_sample_test()`](https://www.indrapatil.com/statsExpressions/reference/two_sample_test.md) | two-sample test | ✅ | ✅ | ✅ | ✅ |
| [`oneway_anova()`](https://www.indrapatil.com/statsExpressions/reference/oneway_anova.md) | one-way ANOVA | ✅ | ✅ | ✅ | ✅ |
| [`corr_test()`](https://www.indrapatil.com/statsExpressions/reference/corr_test.md) | correlation | ✅ | ✅ | ✅ | ✅ |
| [`contingency_table()`](https://www.indrapatil.com/statsExpressions/reference/contingency_table.md) | one- or two-way contingency table | ✅ | ✅ | ❌ | ✅ |
| [`meta_analysis()`](https://www.indrapatil.com/statsExpressions/reference/meta_analysis.md) | random-effects meta-analysis | ✅ | ❌ | ✅ | ✅ |
| [`pairwise_comparisons()`](https://www.indrapatil.com/statsExpressions/reference/pairwise_comparisons.md) | pairwise group comparisons | ✅ | ✅ | ✅ | ✅ |
| [`centrality_description()`](https://www.indrapatil.com/statsExpressions/reference/centrality_description.md) | centrality and distribution | ✅ | ✅ | ✅ | ✅ |

❌ means that no dedicated analysis of that type exists. For contingency
tables, `type = "nonparametric"` and `type = "robust"` both use the same
frequentist tests as `type = "parametric"`.

[`pairwise_contingency_table()`](https://www.indrapatil.com/statsExpressions/reference/pairwise_contingency_table.md)
provides pairwise Fisher’s exact tests with Cramer’s *V* effect sizes.
It does not have a `type` argument.

# Tidy dataframes from statistical analysis

To illustrate the simplicity of this syntax, let’s say we want to run a
one-way ANOVA. If we first run a non-parametric ANOVA and then decide to
run a robust ANOVA instead, the syntax remains the same and the
statistical approach can be modified by changing a single argument:

\
`mtcars`` ``|>`` `[`oneway_anova`](https://www.indrapatil.com/statsExpressions/reference/oneway_anova.md)`(``cyl``, ``wt``, type ``=`` ``"nonparametric"``)`\
`#> # A tibble: 1 × 15`\
`#>   parameter1 parameter2 statistic df.error   p.value`\
`#>   <chr>      <chr>          <dbl>    <int>     <dbl>`\
`#> 1 wt         cyl             22.8        2 0.0000112`\
`#>   method                       effectsize      estimate conf.level conf.low`\
`#>   <chr>                        <chr>              <dbl>      <dbl>    <dbl>`\
`#> 1 Kruskal-Wallis rank sum test Epsilon2 (rank)    0.736       0.95    0.624`\
`#>   conf.high conf.method          conf.iterations n.obs expression`\
`#>       <dbl> <chr>                          <int> <int> <list>    `\
`#> 1         1 percentile bootstrap             100    32 <language>`\
\
`mtcars`` ``|>`` `[`oneway_anova`](https://www.indrapatil.com/statsExpressions/reference/oneway_anova.md)`(``cyl``, ``wt``, type ``=`` ``"robust"``)`\
`#> # A tibble: 1 × 12`\
`#>   statistic    df df.error p.value`\
`#>       <dbl> <dbl>    <dbl>   <dbl>`\
`#> 1      12.7     2     12.2 0.00102`\
`#>   method                                           `\
`#>   <chr>                                            `\
`#> 1 A heteroscedastic one-way ANOVA for trimmed means`\
`#>   effectsize                         estimate conf.level conf.low conf.high`\
`#>   <chr>                                 <dbl>      <dbl>    <dbl>     <dbl>`\
`#> 1 Explanatory measure of effect size     1.05       0.95    0.843      1.50`\
`#>   n.obs expression`\
`#>   <int> <list>    `\
`#> 1    32 <language>`

For worked examples of these data frames, see [Data frame
outputs](https://www.indrapatil.com/statsExpressions/articles/web_only/dataframe_outputs.html);
for their columns and attributes, see [Return value
schema](https://www.indrapatil.com/statsExpressions/articles/web_only/return_value_schema.html).

The data frames also work with
[`knitr::kable()`](https://rdrr.io/pkg/knitr/man/kable.html) to generate
a table:

\
[`set.seed`](https://rdrr.io/r/base/Random.html)`(``123``)`\
\
`# one-sample robust t-test`\
`` # we will leave `expression` column out; it's not needed for using only the dataframe ``\
`mtcars`` ``|>`\
`  `[`one_sample_test`](https://www.indrapatil.com/statsExpressions/reference/one_sample_test.md)`(``wt``, test.value ``=`` ``3``, type ``=`` ``"robust"``)`` ``|>`\
`  ``dplyr``::`[`select`](https://dplyr.tidyverse.org/reference/select.html)`(``-``expression``)`` ``|>`\
`  ``knitr``::`[`kable`](https://rdrr.io/pkg/knitr/man/kable.html)`(``)`

| statistic | p.value | n.obs | method | effectsize | estimate | conf.level | conf.low | conf.high |
|---:|---:|---:|:---|:---|---:|---:|---:|---:|
| 1.179181 | 0.275 | 32 | Bootstrap-t method for one-sample test | Trimmed mean | 3.197 | 0.95 | 2.854246 | 3.539754 |

These functions are also compatible with other popular data manipulation
packages.

For example, let’s say we want to run a one-sample *t*-test for all
levels of a certain grouping variable. We can use `dplyr` to do so:

\
`# for reproducibility`\
[`set.seed`](https://rdrr.io/r/base/Random.html)`(``123``)`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`dplyr`](https://dplyr.tidyverse.org)`)`\
\
`# grouped operation`\
`` # running one-sample test for all levels of grouping variable `cyl` ``\
`mtcars`` ``|>`\
`  `[`group_by`](https://dplyr.tidyverse.org/reference/group_by.html)`(``cyl``)`` ``|>`\
`  `[`group_modify`](https://dplyr.tidyverse.org/reference/group_map.html)`(``~`` `[`one_sample_test`](https://www.indrapatil.com/statsExpressions/reference/one_sample_test.md)`(``.x``, ``wt``, test.value ``=`` ``3``)``, .keep ``=`` ``TRUE``)`` ``|>`\
`  `[`ungroup`](https://dplyr.tidyverse.org/reference/group_by.html)`(``)`\
`#> # A tibble: 3 × 16`\
`#>     cyl    mu statistic df.error  p.value method            alternative`\
`#>   <dbl> <dbl>     <dbl>    <dbl>    <dbl> <chr>             <chr>      `\
`#> 1     4     3    -4.16        10 0.00195  One Sample t-test two.sided  `\
`#> 2     6     3     0.870        6 0.418    One Sample t-test two.sided  `\
`#> 3     8     3     4.92        13 0.000278 One Sample t-test two.sided  `\
`#>   effectsize estimate conf.level conf.low conf.high conf.method`\
`#>   <chr>         <dbl>      <dbl>    <dbl>     <dbl> <chr>      `\
`#> 1 Hedges' g    -1.16        0.95   -1.88     -0.402 ncp        `\
`#> 2 Hedges' g     0.286       0.95   -0.388     0.937 ncp        `\
`#> 3 Hedges' g     1.24        0.95    0.544     1.91  ncp        `\
`#>   conf.distribution n.obs expression`\
`#>   <chr>             <int> <list>    `\
`#> 1 t                    11 <language>`\
`#> 2 t                     7 <language>`\
`#> 3 t                    14 <language>`

# Using expressions in custom plots

Note that *expression* here means **a pre-formatted in-text statistical
result**. In addition to other details contained in the dataframe, there
is also a column titled `expression`, which contains expression with
statistical details and can be displayed in a plot.

For **all** statistical test expressions, the default template attempts
to follow the gold standard for statistical reporting.

For example, here are results from Welch’s *t*-test:

![Example of statistical reporting format showing Welch's t-test results
with test statistic, degrees of freedom, p-value, and effect
size](reference/figures/stats_reporting_format.png)

To display a single result in a plot, you have two options:

- extract the expression from the list column
  (`results_data$expression[[1]]`) without parsing
- use the list column as is, in which case you will need to parse it
  (`parse(text = results_data$expression)`)

For several row-specific labels in a geometric layer, map `expression`
to `label` and use `parse = TRUE`, as in the centrality example below.

Here are a few examples for supported analyses. Let’s load the needed
library for visualization:

\
[`library`](https://rdrr.io/r/base/library.html)`(`[`ggplot2`](https://ggplot2.tidyverse.org)`)`

## Expressions for centrality measure

\
`` # displaying mean for each level of `cyl` ``\
[`centrality_description`](https://www.indrapatil.com/statsExpressions/reference/centrality_description.md)`(``mtcars``, ``cyl``, ``wt``)`` ``|>`\
`  `[`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)`(`[`aes`](https://ggplot2.tidyverse.org/reference/aes.html)`(``cyl``, ``wt``)``)`` ``+`\
`  `[`geom_point`](https://ggplot2.tidyverse.org/reference/geom_point.html)`(``)`` ``+`\
`  `[`geom_label`](https://ggplot2.tidyverse.org/reference/geom_text.html)`(`[`aes`](https://ggplot2.tidyverse.org/reference/aes.html)`(``label ``=`` ``expression``)``, parse ``=`` ``TRUE``)`

![Example ggplot showing centrality_description output: mean wt by cyl
with labeled statistical
expressions.](reference/figures/README-centrality-1.png)

## Expressions for one-way ANOVAs

### Between-subjects design

\
[`set.seed`](https://rdrr.io/r/base/Random.html)`(``123``)`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`ggridges`](https://wilkelab.org/ggridges/)`)`\
\
`results_data`` ``<-`` `[`oneway_anova`](https://www.indrapatil.com/statsExpressions/reference/oneway_anova.md)`(``iris``, ``Species``, ``Sepal.Length``, type ``=`` ``"robust"``)`\
\
`# create a ridgeplot`\
[`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)`(``iris``, `[`aes`](https://ggplot2.tidyverse.org/reference/aes.html)`(``x ``=`` ``Sepal.Length``, y ``=`` ``Species``)``)`` ``+`\
`  ``geom_density_ridges``(``)`` ``+`\
`  `[`labs`](https://ggplot2.tidyverse.org/reference/labs.html)`(`\
`    title ``=`` ``"A heteroscedastic one-way ANOVA for trimmed means"``,`\
`    subtitle ``=`` ``results_data``$``expression``[[``1``]``]`\
`  ``)`

![Example ridge density plot for a heteroscedastic one-way ANOVA on
Sepal.Length by Species](reference/figures/README-anova_rob1-1.png)

### Within-subjects design

\
[`set.seed`](https://rdrr.io/r/base/Random.html)`(``123``)`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`WRS2`](https://r-forge.r-project.org/projects/psychor/)`)`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`ggbeeswarm`](https://github.com/eclarke/ggbeeswarm)`)`\
\
`results_data`` ``<-`` `[`oneway_anova`](https://www.indrapatil.com/statsExpressions/reference/oneway_anova.md)`(`\
`  ``WineTasting``,`\
`  ``Wine``,`\
`  ``Taste``,`\
`  paired ``=`` ``TRUE``,`\
`  subject.id ``=`` ``Taster``,`\
`  type ``=`` ``"np"`\
`)`\
\
[`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)`(``WineTasting``, `[`aes`](https://ggplot2.tidyverse.org/reference/aes.html)`(``Wine``, ``Taste``, color ``=`` ``Wine``)``)`` ``+`\
`  ``geom_quasirandom``(``)`` ``+`\
`  `[`labs`](https://ggplot2.tidyverse.org/reference/labs.html)`(`\
`    title ``=`` ``"Friedman's rank sum test"``,`\
`    subtitle ``=`` `[`parse`](https://rdrr.io/r/base/parse.html)`(``text ``=`` ``results_data``$``expression``)`\
`  ``)`

![Quasirandom dot plot of wine taste ratings by wine type with
Friedman's rank sum test
subtitle](reference/figures/README-anova_parametric2-1.png)

## Expressions for two-sample tests

### Between-subjects design

\
[`set.seed`](https://rdrr.io/r/base/Random.html)`(``123``)`\
\
`results_data`` ``<-`` `[`two_sample_test`](https://www.indrapatil.com/statsExpressions/reference/two_sample_test.md)`(``ToothGrowth``, ``supp``, ``len``)`\
\
[`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)`(``ToothGrowth``, `[`aes`](https://ggplot2.tidyverse.org/reference/aes.html)`(``supp``, ``len``)``)`` ``+`\
`  `[`geom_boxplot`](https://ggplot2.tidyverse.org/reference/geom_boxplot.html)`(``)`` ``+`\
`  `[`labs`](https://ggplot2.tidyverse.org/reference/labs.html)`(`\
`    title ``=`` ``"Two-Sample Welch's t-test"``,`\
`    subtitle ``=`` `[`parse`](https://rdrr.io/r/base/parse.html)`(``text ``=`` ``results_data``$``expression``)`\
`  ``)`

![Boxplot of ToothGrowth data with Two-Sample Welch's t-test expression
in the subtitle](reference/figures/README-t_two-1.png)

### Within-subjects design

\
[`set.seed`](https://rdrr.io/r/base/Random.html)`(``123``)`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`tidyr`](https://tidyr.tidyverse.org)`)`\
[`library`](https://rdrr.io/r/base/library.html)`(``PairedData``)`\
[`data`](https://rdrr.io/r/utils/data.html)`(``PrisonStress``)`\
\
`# get data in tidy format`\
`df`` ``<-`` `[`pivot_longer`](https://tidyr.tidyverse.org/reference/pivot_longer.html)`(``PrisonStress``, `[`starts_with`](https://tidyselect.r-lib.org/reference/starts_with.html)`(``"PSS"``)``, names_to ``=`` ``"PSS"``, values_to ``=`` ``"stress"``)`\
\
`results_data`` ``<-`` `[`two_sample_test`](https://www.indrapatil.com/statsExpressions/reference/two_sample_test.md)`(`\
`  data ``=`` ``df``,`\
`  x ``=`` ``PSS``,`\
`  y ``=`` ``stress``,`\
`  paired ``=`` ``TRUE``,`\
`  subject.id ``=`` ``Subject``,`\
`  type ``=`` ``"np"`\
`)`\
\
`# plot`\
[`suppressWarnings`](https://rdrr.io/r/base/warning.html)`(``paired.plotProfiles``(`\
`  ``PrisonStress``, ``"PSSbefore"``, ``"PSSafter"``,`\
`  subjects ``=`` ``"Subject"`\
`)``)`` ``+`\
`  `[`labs`](https://ggplot2.tidyverse.org/reference/labs.html)`(`\
`    title ``=`` ``"Two-sample Wilcoxon paired test"``,`\
`    subtitle ``=`` `[`parse`](https://rdrr.io/r/base/parse.html)`(``text ``=`` ``results_data``$``expression``)`\
`  ``)`

![Example paired profiles plot for a two-sample Wilcoxon paired
test](reference/figures/README-t_two_paired1-1.png)

## Expressions for one-sample tests

\
[`set.seed`](https://rdrr.io/r/base/Random.html)`(``123``)`\
\
`# dataframe with results`\
`results_data`` ``<-`` `[`one_sample_test`](https://www.indrapatil.com/statsExpressions/reference/one_sample_test.md)`(``mtcars``, ``wt``, test.value ``=`` ``3``, type ``=`` ``"bayes"``)`\
\
`# creating a histogram plot`\
[`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)`(``mtcars``, `[`aes`](https://ggplot2.tidyverse.org/reference/aes.html)`(``wt``)``)`` ``+`\
`  `[`geom_histogram`](https://ggplot2.tidyverse.org/reference/geom_histogram.html)`(``alpha ``=`` ``0.5``)`` ``+`\
`  `[`geom_vline`](https://ggplot2.tidyverse.org/reference/geom_abline.html)`(``xintercept ``=`` `[`mean`](https://rdrr.io/r/base/mean.html)`(``mtcars``$``wt``)``, color ``=`` ``"red"``)`` ``+`\
`  `[`labs`](https://ggplot2.tidyverse.org/reference/labs.html)`(``subtitle ``=`` `[`parse`](https://rdrr.io/r/base/parse.html)`(``text ``=`` ``results_data``$``expression``)``)`

![Histogram of car weights with a vertical line at the mean and Bayesian
one-sample t-test subtitle](reference/figures/README-t_one-1.png)

## Expressions for correlation analysis

Let’s look at another example where we want to run correlation analysis:

\
[`set.seed`](https://rdrr.io/r/base/Random.html)`(``123``)`\
\
`# dataframe with results`\
`results_data`` ``<-`` `[`corr_test`](https://www.indrapatil.com/statsExpressions/reference/corr_test.md)`(``mtcars``, ``mpg``, ``wt``, type ``=`` ``"nonparametric"``)`\
\
`# create a scatter plot`\
[`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)`(``mtcars``, `[`aes`](https://ggplot2.tidyverse.org/reference/aes.html)`(``mpg``, ``wt``)``)`` ``+`\
`  `[`geom_point`](https://ggplot2.tidyverse.org/reference/geom_point.html)`(``)`` ``+`\
`  `[`geom_smooth`](https://ggplot2.tidyverse.org/reference/geom_smooth.html)`(``method ``=`` ``"lm"``, formula ``=`` ``y`` ``~`` ``x``)`` ``+`\
`  `[`labs`](https://ggplot2.tidyverse.org/reference/labs.html)`(`\
`    title ``=`` ``"Spearman's rank correlation coefficient"``,`\
`    subtitle ``=`` `[`parse`](https://rdrr.io/r/base/parse.html)`(``text ``=`` ``results_data``$``expression``)`\
`  ``)`

![Scatterplot of car fuel efficiency (mpg) versus weight (wt) with
linear fit and Spearman correlation
expression](reference/figures/README-corr-1.png)

## Expressions for contingency table analysis

For categorical/nominal data - one-sample:

\
[`set.seed`](https://rdrr.io/r/base/Random.html)`(``123``)`\
\
`# dataframe with results`\
`results_data`` ``<-`` `[`contingency_table`](https://www.indrapatil.com/statsExpressions/reference/contingency_table.md)`(`\
`  `[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html)`(`[`table`](https://rdrr.io/r/base/table.html)`(``mpg``$``class``)``)``,`\
`  ``Var1``,`\
`  counts ``=`` ``Freq``,`\
`  type ``=`` ``"bayes"`\
`)`\
\
`# create a pie chart`\
[`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)`(`[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html)`(`[`table`](https://rdrr.io/r/base/table.html)`(``mpg``$``class``)``)``, `[`aes`](https://ggplot2.tidyverse.org/reference/aes.html)`(``x ``=`` ``""``, y ``=`` ``Freq``, fill ``=`` `[`factor`](https://rdrr.io/r/base/factor.html)`(``Var1``)``)``)`` ``+`\
`  `[`geom_bar`](https://ggplot2.tidyverse.org/reference/geom_bar.html)`(``width ``=`` ``1``, stat ``=`` ``"identity"``)`` ``+`\
`  `[`theme`](https://ggplot2.tidyverse.org/reference/theme.html)`(``axis.line ``=`` `[`element_blank`](https://ggplot2.tidyverse.org/reference/element.html)`(``)``)`` ``+`\
`  ``# cleaning up the chart and adding results from one-sample proportion test`\
`  `[`coord_polar`](https://ggplot2.tidyverse.org/reference/coord_radial.html)`(``theta ``=`` ``"y"``, start ``=`` ``0``)`` ``+`\
`  `[`labs`](https://ggplot2.tidyverse.org/reference/labs.html)`(`\
`    fill ``=`` ``"Class"``,`\
`    x ``=`` ``NULL``,`\
`    y ``=`` ``NULL``,`\
`    title ``=`` ``"Pie Chart of class (type of car)"``,`\
`    caption ``=`` `[`parse`](https://rdrr.io/r/base/parse.html)`(``text ``=`` ``results_data``$``expression``)`\
`  ``)`

![Pie chart of car classes showing example output with statistical
caption](reference/figures/README-gof-1.png)

You can also use these functions to get the expression in return without
having to display them in plots:

\
[`set.seed`](https://rdrr.io/r/base/Random.html)`(``123``)`\
\
`# Pearson's chi-squared test of independence`\
[`contingency_table`](https://www.indrapatil.com/statsExpressions/reference/contingency_table.md)`(``mtcars``, ``am``, ``vs``)``$``expression``[[``1``]``]`\
`#> list(chi["Pearson"]^2 * "(" * 1 * ")" == "0.91", italic(p) == `\
`#>     "0.34", widehat(italic("V"))["Cramer"] == "0.00", CI["95%"] ~ `\
`#>     "[" * "0.00", "0.49" * "]", italic("n")["obs"] == "32")`

## Expressions for meta-analysis

\
[`set.seed`](https://rdrr.io/r/base/Random.html)`(``123``)`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`metaviz`](https://github.com/Mkossmeier/metaviz)`)`\
\
`# dataframe with results`\
`results_data`` ``<-`` `[`meta_analysis`](https://www.indrapatil.com/statsExpressions/reference/meta_analysis.md)`(``dplyr``::`[`rename`](https://dplyr.tidyverse.org/reference/rename.html)`(``mozart``, estimate ``=`` ``d``, std.error ``=`` ``se``)``)`\
\
`# meta-analysis forest plot with results random-effects meta-analysis`\
[`suppressWarnings`](https://rdrr.io/r/base/warning.html)`(``viz_forest``(`\
`  x ``=`` ``mozart``[``, `[`c`](https://rdrr.io/r/base/c.html)`(``"d"``, ``"se"``)``]``,`\
`  study_labels ``=`` ``mozart``[``, ``"study_name"``]``,`\
`  xlab ``=`` ``"Cohen's d"``,`\
`  variant ``=`` ``"thick"``,`\
`  type ``=`` ``"cumulative"`\
`)``)`` ``+`\
`  `[`labs`](https://ggplot2.tidyverse.org/reference/labs.html)`(`\
`    title ``=`` ``"Meta-analysis of Pietschnig, Voracek, and Formann (2010) on the Mozart effect"``,`\
`    subtitle ``=`` `[`parse`](https://rdrr.io/r/base/parse.html)`(``text ``=`` ``results_data``$``expression``)`\
`  ``)`` ``+`\
`  `[`theme`](https://ggplot2.tidyverse.org/reference/theme.html)`(``text ``=`` `[`element_text`](https://ggplot2.tidyverse.org/reference/element.html)`(``size ``=`` ``12``)``)`

![Meta-analysis forest plot of the Mozart effect example, showing study
estimates and the overall random-effects
result.](reference/figures/README-metaanalysis-1.png)

# Customizing details to your liking

Sometimes you may not wish to include so many details in the subtitle.
In that case, you can extract the expression and copy-paste only the
part you wish to include. For example, here only statistic and
*p*-values are included:

\
[`set.seed`](https://rdrr.io/r/base/Random.html)`(``123``)`\
\
`# extracting detailed expression`\
`(``res_expr`` ``<-`` `[`oneway_anova`](https://www.indrapatil.com/statsExpressions/reference/oneway_anova.md)`(``iris``, ``Species``, ``Sepal.Length``, var.equal ``=`` ``TRUE``)``$``expression``[[``1``]``]``)`\
`#> list(italic("F")["Fisher"](2, 147) == "119.26", italic(p) == `\
`#>     "1.67e-31", widehat(omega["p"]^2) == "0.61", CI["95%"] ~ `\
`#>     "[" * "0.53", "1.00" * "]", italic("n")["obs"] == "150")`\
\
`# adapting the details to your liking`\
[`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)`(``iris``, `[`aes`](https://ggplot2.tidyverse.org/reference/aes.html)`(``x ``=`` ``Species``, y ``=`` ``Sepal.Length``)``)`` ``+`\
`  `[`geom_boxplot`](https://ggplot2.tidyverse.org/reference/geom_boxplot.html)`(``)`` ``+`\
`  `[`labs`](https://ggplot2.tidyverse.org/reference/labs.html)`(``subtitle ``=`` ``ggplot2``::`[`expr`](https://rlang.r-lib.org/reference/expr.html)`(`[`paste`](https://rdrr.io/r/base/paste.html)`(`\
`    ``NULL``, ``italic``(``"F"``)``, ``"("``, ``"2"``, ``","``, ``"147"``, ``") = "``, ``"119.26"``, ``", "``,`\
`    ``italic``(``"p"``)``, ``" = "``, ``"1.67e-31"`\
`  ``)``)``)`

![Example ggplot boxplot of iris Sepal.Length by Species with a custom
statistical expression
subtitle](reference/figures/README-custom_expr-1.png)

# Tests and effect sizes

See [Test and effect size
details](https://www.indrapatil.com/statsExpressions/articles/stats_details.html)
for the methods, effect sizes, and underlying functions used by each
analysis. The same method tables are included in the individual function
help pages.

# Usage in `{ggstatsplot}`

Note that these functions were initially written to display results from
statistical tests on ready-made [ggplot2](https://ggplot2.tidyverse.org)
plots implemented in
[ggstatsplot](https://www.indrapatil.com/ggstatsplot/).

For detailed documentation, see the package website:
<https://www.indrapatil.com/ggstatsplot/>

Here is an example from
[ggstatsplot](https://www.indrapatil.com/ggstatsplot/) of what the plots
look like when the expressions are displayed in the subtitle:

![Example plot from ggstatsplot package showing statistical results in
subtitle](reference/figures/ggstatsplot.png)

# Acknowledgments

The hexsticker and the schematic illustration of general workflow were
generously designed by Sarah Otterstetter (Max Planck Institute for
Human Development, Berlin).
