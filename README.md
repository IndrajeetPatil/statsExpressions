
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `statsExpressions`: Tidy dataframes and expressions with statistical details

| Package                                                                                                                                                   | Status                                                                                                                                                                                                 | Usage                                                                                                                                                       | GitHub                                                                                                                                                                   | References                                                                                                                                                                |
|-----------------------------------------------------------------------------------------------------------------------------------------------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|-------------------------------------------------------------------------------------------------------------------------------------------------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| [![CRAN\_Release\_Badge](http://www.r-pkg.org/badges/version-ago/statsExpressions)](https://CRAN.R-project.org/package=statsExpressions)                  | [![Build Status](https://travis-ci.org/IndrajeetPatil/statsExpressions.svg?branch=master)](https://travis-ci.org/IndrajeetPatil/statsExpressions)                                                      | [![Daily downloads badge](https://cranlogs.r-pkg.org/badges/last-day/statsExpressions?color=blue)](https://CRAN.R-project.org/package=statsExpressions)     | [![GitHub version](https://img.shields.io/badge/GitHub-1.0.0-orange.svg?style=flat-square)](https://github.com/IndrajeetPatil/statsExpressions/)                         | [![Website](https://img.shields.io/badge/website-statsExpressions-orange.svg?colorB=E91E63)](https://indrajeetpatil.github.io/statsExpressions/)                          |
| [![CRAN Checks](https://cranchecks.info/badges/summary/statsExpressions)](https://cran.r-project.org/web/checks/check_results_statsExpressions.html)      | [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/IndrajeetPatil/statsExpressions?branch=master&svg=true)](https://ci.appveyor.com/project/IndrajeetPatil/statsExpressions) | [![Weekly downloads badge](https://cranlogs.r-pkg.org/badges/last-week/statsExpressions?color=blue)](https://CRAN.R-project.org/package=statsExpressions)   | [![Forks](https://img.shields.io/badge/forks-15-blue.svg)](https://github.com/IndrajeetPatil/statsExpressions/)                                                          | [![Features](https://img.shields.io/badge/features-statsExpressions-orange.svg?colorB=2196F3)](https://indrajeetpatil.github.io/statsExpressions/reference/index.html)    |
| [![minimal R version](https://img.shields.io/badge/R%3E%3D-3.6.0-6666ff.svg)](https://cran.r-project.org/)                                                | [![lifecycle](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html)                                                                        | [![Monthly downloads badge](https://cranlogs.r-pkg.org/badges/last-month/statsExpressions?color=blue)](https://CRAN.R-project.org/package=statsExpressions) | [![Github Issues](https://img.shields.io/badge/issues-11-red.svg)](https://github.com/IndrajeetPatil/statsExpressions/issues)                                            | [![pkgdown](https://github.com/IndrajeetPatil/pairwiseComparisons/workflows/pkgdown/badge.svg)](https://github.com/IndrajeetPatil/pairwiseComparisons/actions)            |
| [![code size](https://img.shields.io/github/languages/code-size/IndrajeetPatil/statsExpressions.svg)](https://github.com/IndrajeetPatil/statsExpressions) | [![Coverage Status](https://coveralls.io/repos/github/IndrajeetPatil/statsExpressions/badge.svg?branch=master)](https://coveralls.io/github/IndrajeetPatil/statsExpressions?branch=master)             | [![Total downloads badge](https://cranlogs.r-pkg.org/badges/grand-total/statsExpressions?color=blue)](https://CRAN.R-project.org/package=statsExpressions)  | [![Github Stars](https://img.shields.io/github/stars/IndrajeetPatil/statsExpressions.svg?style=social&label=Github)](https://github.com/IndrajeetPatil/statsExpressions) | [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.2074621.svg)](https://doi.org/10.5281/zenodo.3386122)                                                                 |
| [![Licence](https://img.shields.io/badge/licence-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html)                                          | [![Codecov test coverage](https://codecov.io/gh/IndrajeetPatil/statsExpressions/branch/master/graph/badge.svg)](https://codecov.io/gh/IndrajeetPatil/statsExpressions?branch=master)                   | [![lints](https://github.com/IndrajeetPatil/statsExpressions/workflows/lint/badge.svg)](https://github.com/IndrajeetPatil/statsExpressions)                 | [![Last-changedate](https://img.shields.io/badge/last%20change-2021--05--05-yellowgreen.svg)](https://github.com/IndrajeetPatil/statsExpressions/commits/master)         | [![GitHub last commit](https://img.shields.io/github/last-commit/IndrajeetPatil/statsExpressions.svg)](https://github.com/IndrajeetPatil/statsExpressions/commits/master) |
| [![status](https://tinyverse.netlify.com/badge/statsExpressions)](https://CRAN.R-project.org/package=statsExpressions)                                    | [![R build status](https://github.com/IndrajeetPatil/statsExpressions/workflows/R-CMD-check/badge.svg)](https://github.com/IndrajeetPatil/statsExpressions)                                            | [![Gitter chat](https://badges.gitter.im/gitterHQ/gitter.png)](https://gitter.im/statsExpressions/community)                                                | [![Project Status](http://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)                                                              | [![contributions welcome](https://img.shields.io/badge/contributions-welcome-brightgreen.svg?style=flat)](https://github.com/IndrajeetPatil/statsExpressions/issues)      |

# Introduction <img src="man/figures/logo.png" align="right" width="240" />

The `statsExpressions` package has two key aims:

-   to provide a consistent syntax to do statistical analysis with tidy
    data,
-   to provide statistical expressions (i.e., a pre-formatted in-text
    statistical result) for plotting functions.

Currently, it supports common types of statistical approaches and tests:
parametric, nonparametric, robust, and Bayesian *t*-test, one-way ANOVA,
correlation analyses, contingency table analyses, and meta-analyses. The
functions are pipe-friendly and compatible with tidy data.

Statistical packages exhibit substantial diversity in terms of their
syntax and expected input type. This can make it difficult to switch
from one statistical approach to another. For example, some functions
expect vectors as inputs, while others expect dataframes. Depending on
whether it is a repeated measures design or not, different functions
might expect data to be in wide or long format. Some functions can
internally omit missing values, while other functions error in their
presence. Furthermore, if someone wishes to utilize the objects returned
by these packages downstream in their workflow, this is not
straightforward either because even functions from the same package can
return a list, a matrix, an array, a dataframe, etc., depending on the
function.

This is where `statsExpressions` comes in: It can be thought of as a
unified portal through which most of the functionality in these
underlying packages can be accessed, with a simpler interface and no
requirement to change data format.

This package forms the statistical processing backend for
[`ggstatsplot`](https://indrajeetpatil.github.io/ggstatsplot/) package.

------------------------------------------------------------------------

**Please note that the API for the package has changed significantly in
`1.0.0`** **release and will break any code that used prior versions of
this package.**

See: <https://indrajeetpatil.github.io/statsExpressions/news/index.html>

------------------------------------------------------------------------

# Installation

To get the latest, stable `CRAN` release:

``` r
install.packages("statsExpressions")
```

You can get the **development** version of the package from `GitHub`. To
see what new changes (and bug fixes) have been made to the package since
the last release on `CRAN`, you can check the detailed log of changes
here:
<https://indrajeetpatil.github.io/statsExpressions/news/index.html>

If you are in hurry and want to reduce the time of installation, prefer-

``` r
# needed package to download from GitHub repo
install.packages("remotes")

# downloading the package from GitHub
remotes::install_github(
  repo = "IndrajeetPatil/statsExpressions", # package path on GitHub
  dependencies = FALSE, # assumes you have already installed needed packages
  quick = TRUE # skips docs, demos, and vignettes
)
```

If time is not a constraint-

``` r
remotes::install_github(
  repo = "IndrajeetPatil/statsExpressions", # package path on GitHub
  dependencies = TRUE, # installs packages which statsExpressions depends on
  upgrade_dependencies = TRUE # updates any out of date dependencies
)
```

# Citation

If you want to cite this package in a scientific journal or in any other
context:

``` r
citation("statsExpressions")
#> 
#>   Patil, I., (2021). statsExpressions: R Package for Tidy Dataframes
#>   and Expressions with Statistical Details. PsyArxiv.
#>   https://doi.org/10.31234/osf.io/ntbvy
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Article{,
#>     title = {{statsExpressions}: {R} Package for Tidy Dataframes and Expressions with Statistical Details},
#>     author = {Indrajeet Patil},
#>     year = {2021},
#>     journal = {PsyArxiv},
#>     doi = {10.31234/osf.io/ntbvy},
#>   }
```

# Documentation and Examples

To see the documentation relevant for the **development** version of the
package, see the dedicated website for `statsExpressions`, which is
updated after every new commit:
<https://indrajeetpatil.github.io/statsExpressions/>.

# Summary of types of statistical analyses

Currently, it supports only the most common types of statistical tests.
Specifically, **parametric**, **non-parametric**, **robust**, and
**bayesian** versions of:

| Test                       | Function            | Lifecycle                                                                                                                       |
|----------------------------|---------------------|---------------------------------------------------------------------------------------------------------------------------------|
| one-sample *t*-test        | `one_sample_test`   | [![lifecycle](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html) |
| two-sample *t*-test        | `two_sample_test`   | [![lifecycle](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html) |
| one-way ANOVA              | `oneway_anova`      | [![lifecycle](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html) |
| correlation analysis       | `corr_test`         | [![lifecycle](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html) |
| contingency table analysis | `contingency_table` | [![lifecycle](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html) |
| meta-analysis              | `meta_analysis`     | [![lifecycle](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html) |

The table below summarizes all the different types of analyses currently
supported in this package-

| Description                                       | Parametric | Non-parametric | Robust | Bayesian |
|---------------------------------------------------|------------|----------------|--------|----------|
| Between group/condition comparisons               | ✅          | ✅              | ✅      | ✅        |
| Within group/condition comparisons                | ✅          | ✅              | ✅      | ✅        |
| Distribution of a numeric variable                | ✅          | ✅              | ✅      | ✅        |
| Correlation between two variables                 | ✅          | ✅              | ✅      | ✅        |
| Association between categorical variables         | ✅          | `NA`           | `NA`   | ✅        |
| Equal proportions for categorical variable levels | ✅          | `NA`           | `NA`   | ✅        |
| Random-effects meta-analysis                      | ✅          | `NA`           | ✅      | ✅        |

# Statistical reporting

For **all** statistical test expressions, the default template attempt
to follow the gold standard for statistical reporting.

For example, here are results from Welch’s *t*-test:

<img src="man/figures/stats_reporting_format.png" align="center" />

# Summary of statistical tests and effect sizes

Here is a summary table of all the statistical tests currently supported
across various functions:
<https://indrajeetpatil.github.io/statsExpressions/articles/stats_details.html>

# Tidy dataframes

The dataframe will contain the following columns (the exact columns will
depend on the test and the statistical approach):

-   `statistic`: the numeric value of a statistic.

-   `df`: the numeric value of a parameter being modeled (often degrees
    of freedom for the test); note that if `no.parameters = 0L` (e.g.,
    for non-parametric tests), this column will be irrelevant.

-   `df.error` and `df`: relevant only if the statistic in question has
    two degrees of freedom (e.g., anova).

-   `p.value`: the two-sided *p*-value associated with the observed
    statistic.

-   `method`: the details of the statistical test carried out.

-   `estimate`: estimated value of the effect size.

-   `conf.low`: lower bound for the effect size estimate.

-   `conf.high`: upper bound for the effect size estimate.

-   `conf.level`: width of the confidence interval.

-   `effectsize`: the type of the effect size.

All possible outputs from all functions are tabulated here:
<https://indrajeetpatil.github.io/statsExpressions/articles/web_only/dataframe_outputs.html>

But here is one quick example:

``` r
# setup
library(statsExpressions)
set.seed(123)

# one-way ANOVA - parametric
mtcars %>% oneway_anova(x = cyl, y = wt)
#> # A tibble: 1 x 11
#>   statistic    df df.error   p.value
#>       <dbl> <dbl>    <dbl>     <dbl>
#> 1      20.2     2     19.0 0.0000196
#>   method                                                   estimate conf.level
#>   <chr>                                                       <dbl>      <dbl>
#> 1 One-way analysis of means (not assuming equal variances)    0.637       0.95
#>   conf.low conf.high effectsize expression
#>      <dbl>     <dbl> <chr>      <list>    
#> 1    0.309     0.785 Omega2     <language>
```

Needless to say this will also work with the `kable` function to
generate a table:

``` r
# setup
library(statsExpressions)
set.seed(123)

# one-sample robust t-test
# we will leave `expression` column out; it's not needed for using only the dataframe
mtcars %>%
  one_sample_test(wt, test.value = 3, type = "robust") %>%
  dplyr::select(-expression) %>%
  knitr::kable()
```

| statistic | p.value | method                                 | estimate | conf.low | conf.high | conf.level | effectsize   |
|----------:|--------:|:---------------------------------------|---------:|---------:|----------:|-----------:|:-------------|
|  1.179181 |    0.22 | Bootstrap-t method for one-sample test |    3.197 | 2.872163 |  3.521837 |       0.95 | Trimmed mean |

These functions also play nicely with `dplyr` function. For example,
let’s say we want to run a one-sample *t*-test for all levels of a
certain grouping variable. Here is how you can do it:

``` r
# for reproducibility
set.seed(123)
library(dplyr)

# grouped operation
# running one-sample test for all levels of grouping variable `cyl`
mtcars %>%
  group_by(cyl) %>%
  group_modify(~ one_sample_test(.x, wt, test.value = 3), .keep = TRUE) %>%
  ungroup()
#> # A tibble: 3 x 12
#>     cyl    mu statistic df.error  p.value method            estimate conf.level
#>   <dbl> <dbl>     <dbl>    <dbl>    <dbl> <chr>                <dbl>      <dbl>
#> 1     4     3    -4.16        10 0.00195  One Sample t-test   -1.16        0.95
#> 2     6     3     0.870        6 0.418    One Sample t-test    0.286       0.95
#> 3     8     3     4.92        13 0.000278 One Sample t-test    1.24        0.95
#>   conf.low conf.high effectsize expression
#>      <dbl>     <dbl> <chr>      <list>    
#> 1   -1.97     -0.422 Hedges' g  <language>
#> 2   -0.419     1.01  Hedges' g  <language>
#> 3    0.565     1.98  Hedges' g  <language>
```

# Using expressions in custom plots

Note that *expression* here means **a pre-formatted in-text statistical
result**.

In addition to other details contained in the dataframe, there is also a
column titled `expression`, which contains expression with statistical
details and can be displayed in a plot.

There is also a gallery containing additional examples here:
<https://indrajeetpatil.github.io/statsExpressions/articles/gallery.html>

## Example: Expressions for one-way ANOVAs

### Between-subjects design

Let’s say we want to check differences in weight of the vehicle based on
number of cylinders in the engine and wish to carry out robust
trimmed-means ANOVA:

``` r
# setup
set.seed(123)
library(ggplot2)
library(statsExpressions)
library(ggridges)

# create a ridgeplot
ggplot(iris, aes(x = Sepal.Length, y = Species)) +
  geom_density_ridges(
    jittered_points = TRUE, quantile_lines = TRUE,
    scale = 0.9, vline_size = 1, vline_color = "red",
    position = position_raincloud(adjust_vlines = TRUE)
  ) + # use the expression in the dataframe to display results in the subtitle
  labs(
    title = "A heteroscedastic one-way ANOVA for trimmed means",
    subtitle = oneway_anova(iris, Species, Sepal.Length, type = "robust")$expression[[1]]
  )
```

<img src="man/figures/README-anova_rob1-1.png" width="100%" />

### Within-subjects design

Let’s now see an example of a repeated measures one-way ANOVA.

``` r
# setup
set.seed(123)
library(ggplot2)
library(WRS2)
library(ggbeeswarm)
library(statsExpressions)

ggplot2::ggplot(WineTasting, aes(Wine, Taste, color = Wine)) +
  geom_quasirandom() +
  labs(
    title = "Friedman's rank sum test",
    subtitle = oneway_anova(
      WineTasting,
      Wine,
      Taste,
      paired = TRUE,
      subject.id = Taster,
      type = "np"
    )$expression[[1]]
  )
```

<img src="man/figures/README-anova_parametric2-1.png" width="100%" />

## Example: Expressions for two-sample tests

### Between-subjects design

``` r
# setup
set.seed(123)
library(ggplot2)
library(gghalves)
library(ggbeeswarm)
library(hrbrthemes)
library(statsExpressions)

# create a plot
ggplot(ToothGrowth, aes(supp, len)) +
  geom_half_boxplot() +
  geom_beeswarm(beeswarmArgs = list(side = 1)) +
  theme_ipsum_rc() +
  # adding a subtitle with
  labs(
    title = "Two-Sample Welch's t-test",
    subtitle = two_sample_test(ToothGrowth, supp, len)$expression[[1]]
  )
```

<img src="man/figures/README-t_two-1.png" width="100%" />

### Within-subjects design

We can also have a look at a repeated measures design and the related
expressions.

``` r
# setup
set.seed(123)
library(ggplot2)
library(statsExpressions)
library(tidyr)
library(PairedData)
data(PrisonStress)

# plot
paired.plotProfiles(PrisonStress, "PSSbefore", "PSSafter", subjects = "Subject") +
  # `statsExpressions` needs data in the tidy format
  labs(
    title = "Two-sample Wilcoxon paired test",
    subtitle = two_sample_test(
      data = pivot_longer(PrisonStress, starts_with("PSS"), "PSS", values_to = "stress"),
      x = PSS,
      y = stress,
      paired = TRUE,
      subject.id = Subject,
      type = "np"
    )$expression[[1]]
  )
```

<img src="man/figures/README-t_two_paired1-1.png" width="100%" />

## Example: Expressions for one-sample tests

``` r
# setup
set.seed(123)
library(ggplot2)
library(statsExpressions)

# creating a histogram plot
ggplot(mtcars, aes(wt)) +
  geom_histogram(alpha = 0.5) +
  geom_vline(xintercept = mean(mtcars$wt), color = "red") +
  # adding a caption with a non-parametric one-sample test
  labs(
    title = "One-Sample Wilcoxon Signed Rank Test",
    subtitle = one_sample_test(mtcars, wt, test.value = 3, type = "nonparametric")$expression[[1]]
  )
```

<img src="man/figures/README-t_one-1.png" width="100%" />

## Example: Expressions for correlation analyses

Let’s look at another example where we want to run correlation analysis:

``` r
# setup
set.seed(123)
library(ggplot2)
library(statsExpressions)

# create a scatter plot
ggplot(mtcars, aes(mpg, wt)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x) +
  labs(
    title = "Spearman's rank correlation coefficient",
    subtitle = corr_test(mtcars, mpg, wt, type = "nonparametric")$expression[[1]]
  )
```

<img src="man/figures/README-corr-1.png" width="100%" />

## Example: Expressions for contingency table analysis

For categorical/nominal data - one-sample:

``` r
# setup
set.seed(123)
library(ggplot2)
library(statsExpressions)

# basic pie chart
ggplot(as.data.frame(table(mpg$class)), aes(x = "", y = Freq, fill = factor(Var1))) +
  geom_bar(width = 1, stat = "identity") +
  theme(axis.line = element_blank()) +
  # cleaning up the chart and adding results from one-sample proportion test
  coord_polar(theta = "y", start = 0) +
  labs(
    fill = "Class",
    x = NULL,
    y = NULL,
    title = "Pie Chart of class (type of car)",
    subtitle = contingency_table(as.data.frame(table(mpg$class)), Var1, counts = Freq)$expression[[1]],
    caption = "One-sample goodness of fit proportion test"
  )
```

<img src="man/figures/README-gof-1.png" width="100%" />

You can also use these function to get the expression in return without
having to display them in plots:

``` r
# setup
set.seed(123)
library(ggplot2)
library(statsExpressions)

# Pearson's chi-squared test of independence
contingency_table(mtcars, am, cyl)$expression[[1]]
#> paste(chi["Pearson"]^2, "(", "2", ") = ", "8.74", ", ", italic("p"), 
#>     " = ", "0.013", ", ", widehat(italic("V"))["Cramer"], " = ", 
#>     "0.46", ", CI"["95%"], " [", "0.00", ", ", "0.78", "]", ", ", 
#>     italic("n")["obs"], " = ", "32")
```

## Example: Expressions for meta-analysis

``` r
# setup
set.seed(123)
library(metaviz)
library(ggplot2)
library(metaplus)

# meta-analysis forest plot with results random-effects meta-analysis
viz_forest(
  x = mozart[, c("d", "se")],
  study_labels = mozart[, "study_name"],
  xlab = "Cohen's d",
  variant = "thick",
  type = "cumulative"
) + # use `statsExpressions` to create expression containing results
  labs(
    title = "Meta-analysis of Pietschnig, Voracek, and Formann (2010) on the Mozart effect",
    subtitle = meta_analysis(dplyr::rename(mozart, estimate = d, std.error = se))$expression[[1]]
  ) +
  theme(text = element_text(size = 12))
```

<img src="man/figures/README-metaanalysis-1.png" width="100%" />

# Customizing details to your liking

Sometimes you may not wish include so many details in the subtitle. In
that case, you can extract the expression and copy-paste only the part
you wish to include. For example, here only statistic and *p*-values are
included:

``` r
# setup
set.seed(123)
library(ggplot2)
library(statsExpressions)

# extracting detailed expression
(res_expr <- oneway_anova(iris, Species, Sepal.Length, var.equal = TRUE)$expression[[1]])
#> paste(italic("F")["Fisher"], "(", "2", ",", "147", ") = ", "119.26", 
#>     ", ", italic("p"), " = ", "1.67e-31", ", ", widehat(omega["p"]^2), 
#>     " = ", "0.61", ", CI"["95%"], " [", "0.52", ", ", "0.68", 
#>     "]", ", ", italic("n")["obs"], " = ", "150")

# adapting the details to your liking
ggplot(iris, aes(x = Species, y = Sepal.Length)) +
  geom_boxplot() +
  labs(subtitle = ggplot2::expr(paste(
    NULL, italic("F"), "(", "2",
    ",", "147", ") = ", "119.26", ", ",
    italic("p"), " = ", "1.67e-31"
  )))
```

<img src="man/figures/README-custom_expr-1.png" width="100%" />

# Summary of tests and effect sizes

Here a go-to summary about statistical test carried out and the returned
effect size for each function is provided. This should be useful if one
needs to find out more information about how an argument is resolved in
the underlying package or if one wishes to browse the source code. So,
for example, if you want to know more about how one-way
(between-subjects) ANOVA, you can run `?stats::oneway.test` in your R
console.

## `two_sample_test` + `oneway_anova`

No. of groups: `2` =&gt; `two_sample_test`<br> No. of groups: `> 2`
=&gt; `oneway_anova`

### between-subjects

Following (between-subjects) tests are carried out for each type of
analyses-

| Type           | No. of groups | Test                                            | Function used          |
|----------------|---------------|-------------------------------------------------|------------------------|
| Parametric     | &gt; 2        | Fisher’s or Welch’s one-way ANOVA               | `stats::oneway.test`   |
| Non-parametric | &gt; 2        | Kruskal–Wallis one-way ANOVA                    | `stats::kruskal.test`  |
| Robust         | &gt; 2        | Heteroscedastic one-way ANOVA for trimmed means | `WRS2::t1way`          |
| Bayes Factor   | &gt; 2        | Fisher’s ANOVA                                  | `BayesFactor::anovaBF` |
| Parametric     | 2             | Student’s or Welch’s *t*-test                   | `stats::t.test`        |
| Non-parametric | 2             | Mann–Whitney *U* test                           | `stats::wilcox.test`   |
| Robust         | 2             | Yuen’s test for trimmed means                   | `WRS2::yuen`           |
| Bayesian       | 2             | Student’s *t*-test                              | `BayesFactor::ttestBF` |

Following effect sizes (and confidence intervals/CI) are available for
each type of test-

| Type           | No. of groups | Effect size                                                  | CI? | Function used                                          |
|----------------|---------------|--------------------------------------------------------------|-----|--------------------------------------------------------|
| Parametric     | &gt; 2        | *η*<sub>*p*</sub><sup>2</sup>, *ω*<sub>*p*</sub><sup>2</sup> | ✅   | `effectsize::omega_squared`, `effectsize::eta_squared` |
| Non-parametric | &gt; 2        | *ϵ*<sub>*o**r**d**i**n**a**l*</sub><sup>2</sup>              | ✅   | `effectsize::rank_epsilon_squared`                     |
| Robust         | &gt; 2        | *ξ* (Explanatory measure of effect size)                     | ✅   | `WRS2::t1way`                                          |
| Bayes Factor   | &gt; 2        | *R*<sub>*p**o**s**t**e**r**i**o**r*</sub><sup>2</sup>        | ✅   | `performance::r2_bayes`                                |
| Parametric     | 2             | Cohen’s *d*, Hedge’s *g*                                     | ✅   | `effectsize::cohens_d`, `effectsize::hedges_g`         |
| Non-parametric | 2             | *r* (rank-biserial correlation)                              | ✅   | `effectsize::rank_biserial`                            |
| Robust         | 2             | *ξ* (Explanatory measure of effect size)                     | ✅   | `WRS2::yuen.effect.ci`                                 |
| Bayesian       | 2             | *δ*<sub>*p**o**s**t**e**r**i**o**r*</sub>                    | ✅   | `bayestestR::describe_posterior`                       |

### within-subjects

Following (within-subjects) tests are carried out for each type of
analyses-

| Type           | No. of groups | Test                                                              | Function used          |
|----------------|---------------|-------------------------------------------------------------------|------------------------|
| Parametric     | &gt; 2        | One-way repeated measures ANOVA                                   | `afex::aov_ez`         |
| Non-parametric | &gt; 2        | Friedman rank sum test                                            | `stats::friedman.test` |
| Robust         | &gt; 2        | Heteroscedastic one-way repeated measures ANOVA for trimmed means | `WRS2::rmanova`        |
| Bayes Factor   | &gt; 2        | One-way repeated measures ANOVA                                   | `BayesFactor::anovaBF` |
| Parametric     | 2             | Student’s *t*-test                                                | `stats::t.test`        |
| Non-parametric | 2             | Wilcoxon signed-rank test                                         | `stats::wilcox.test`   |
| Robust         | 2             | Yuen’s test on trimmed means for dependent samples                | `WRS2::yuend`          |
| Bayesian       | 2             | Student’s *t*-test                                                | `BayesFactor::ttestBF` |

Following effect sizes (and confidence intervals/CI) are available for
each type of test-

| Type           | No. of groups | Effect size                                                                                     | CI? | Function used                                                   |
|----------------|---------------|-------------------------------------------------------------------------------------------------|-----|-----------------------------------------------------------------|
| Parametric     | &gt; 2        | *η*<sub>*p*</sub><sup>2</sup>, *ω*<sub>*p*</sub><sup>2</sup>                                    | ✅   | `effectsize::omega_squared`, `effectsize::eta_squared`          |
| Non-parametric | &gt; 2        | *W*<sub>*K**e**n**d**a**l**l*</sub> (Kendall’s coefficient of concordance)                      | ✅   | `effectsize::kendalls_w`                                        |
| Robust         | &gt; 2        | *δ*<sub>*R* − *a**v**g*</sub><sup>*A**K**P*</sup>                                               | ✅   | Algina-Keselman-Penfield robust standardized difference average |
| Bayes Factor   | &gt; 2        | *R*<sub>*p**o**s**t**e**r**i**o**r*</sub><sup>2</sup>                                           | ✅   | `performance::r2_bayes`                                         |
| Parametric     | 2             | Cohen’s *d*, Hedge’s *g*                                                                        | ✅   | `effectsize::cohens_d`, `effectsize::hedges_g`                  |
| Non-parametric | 2             | *r* (rank-biserial correlation)                                                                 | ✅   | `effectsize::rank_biserial`                                     |
| Robust         | 2             | *δ*<sub>*R*</sub><sup>*A**K**P*</sup> (Algina-Keselman-Penfield robust standardized difference) | ✅   | `WRS2::dep.effect`                                              |
| Bayesian       | 2             | *δ*<sub>*p**o**s**t**e**r**i**o**r*</sub>                                                       | ✅   | `bayestestR::describe_posterior`                                |

## `one_sample_test`

Following tests are carried out for each type of analyses-

| Type           | Test                                     | Function used          |
|----------------|------------------------------------------|------------------------|
| Parametric     | One-sample Student’s *t*-test            | `stats::t.test`        |
| Non-parametric | One-sample Wilcoxon test                 | `stats::wilcox.test`   |
| Robust         | Bootstrap-*t* method for one-sample test | `trimcibt` (custom)    |
| Bayesian       | One-sample Student’s *t*-test            | `BayesFactor::ttestBF` |

Following effect sizes (and confidence intervals/CI) are available for
each type of test-

| Type           | Effect size                               | CI? | Function used                                  |
|----------------|-------------------------------------------|-----|------------------------------------------------|
| Parametric     | Cohen’s *d*, Hedge’s *g*                  | ✅   | `effectsize::cohens_d`, `effectsize::hedges_g` |
| Non-parametric | *r* (rank-biserial correlation)           | ✅   | `effectsize::rank_biserial`                    |
| Robust         | trimmed mean                              | ✅   | `trimcibt` (custom)                            |
| Bayes Factor   | *δ*<sub>*p**o**s**t**e**r**i**o**r*</sub> | ✅   | `bayestestR::describe_posterior`               |

## `corr_test`

Following tests are carried out for each type of analyses. Additionally,
the correlation coefficients (and their confidence intervals) are used
as effect sizes-

| Type           | Test                                       | CI? | Function used              |
|----------------|--------------------------------------------|-----|----------------------------|
| Parametric     | Pearson’s correlation coefficient          | ✅   | `correlation::correlation` |
| Non-parametric | Spearman’s rank correlation coefficient    | ✅   | `correlation::correlation` |
| Robust         | Winsorized Pearson correlation coefficient | ✅   | `correlation::correlation` |
| Bayesian       | Pearson’s correlation coefficient          | ✅   | `correlation::correlation` |

## `contingency_table`

Following tests are carried out for each type of analyses-

| Type of data | Design                      | Test                                   | Function used         |
|--------------|-----------------------------|----------------------------------------|-----------------------|
| Unpaired     | *n* × *p* contingency table | Pearson’s *χ*<sup>2</sup> test         | `stats::chisq.test`   |
| Paired       | *n* × *p* contingency table | McNemar’s *χ*<sup>2</sup> test         | `stats::mcnemar.test` |
| Frequency    | *n* × 1 contingency table   | Goodness of fit (*χ*<sup>2</sup> test) | `stats::chisq.test`   |

Following effect sizes (and confidence intervals/CI) are available for
each type of test-

| Test                           | Effect size  | CI? | Function used           |
|--------------------------------|--------------|-----|-------------------------|
| Pearson’s *χ*<sup>2</sup> test | Cramer’s *V* | ✅   | `effectsize::cramers_v` |
| McNemar’s test                 | Cohen’s *g*  | ✅   | `effectsize::cohens_g`  |
| Goodness of fit                | Cramer’s *V* | ✅   | `effectsize::cramers_v` |

## `meta_analysis`

| Type       | Test                                             | Effect size | CI? | Function used          |
|------------|--------------------------------------------------|-------------|-----|------------------------|
| Parametric | Meta-analysis via random-effects models          | *β*         | ✅   | `metafor::metafor`     |
| Robust     | Meta-analysis via robust random-effects models   | *β*         | ✅   | `metaplus::metaplus`   |
| Bayes      | Meta-analysis via Bayesian random-effects models | *β*         | ✅   | `metaBMA::meta_random` |

# Usage in `ggstatsplot`

Note that these functions were initially written to display results from
statistical tests on ready-made `ggplot2` plots implemented in
`ggstatsplot`.

For detailed documentation, see the package website:
<https://indrajeetpatil.github.io/ggstatsplot/>

Here is an example from `ggstatsplot` of what the plots look like when
the expressions are displayed in the subtitle-

<img src="man/figures/ggstatsplot.png" align="center" />

# Acknowledgments

The hexsticker was generously designed by Sarah Otterstetter (Max Planck
Institute for Human Development, Berlin).

# Contributing

I’m happy to receive bug reports, suggestions, questions, and (most of
all) contributions to fix problems and add features. I personally prefer
using the `GitHub` issues system over trying to reach out to me in other
ways (personal e-mail, Twitter, etc.). Pull Requests for contributions
are encouraged.

Here are some simple ways in which you can contribute (in the increasing
order of commitment):

-   Read and correct any inconsistencies in the
    [documentation](https://indrajeetpatil.github.io/statsExpressions/)

-   Raise issues about bugs or wanted features

-   Review code

-   Add new functionality (in the form of new plotting functions or
    helpers for preparing subtitles)

Please note that this project is released with a [Contributor Code of
Conduct](https://github.com/IndrajeetPatil/statsExpressions/blob/master/.github/CODE_OF_CONDUCT.md).
By participating in this project you agree to abide by its terms.
