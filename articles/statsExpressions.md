# statsExpressions: R Package for Tidy Dataframes and Expressions with Statistical Details

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

## Summary

The [statsExpressions](https://www.indrapatil.com/statsExpressions/)
package has two key aims: to provide a consistent syntax to do
statistical analysis with tidy data, and to provide statistical
expressions (i.e., pre-formatted in-text statistical results) for
plotting functions. Currently, it supports common types of statistical
approaches and tests: parametric, nonparametric, robust, and Bayesian
*t*-test, one-way ANOVA, correlation analyses, contingency table
analyses, and meta-analyses. The functions are pipe-friendly and
compatible with tidy data.

## Statement of Need

Statistical packages exhibit substantial diversity in terms of their
syntax and expected input and output data type. For example, some
functions expect vectors as inputs, while others expect data frames.
Depending on whether it is a repeated measures design or not, functions
from the same package might expect data to be in wide or tidy format.
Some functions can internally omit missing values, while others do not.
Furthermore, the statistical test objects returned by the test functions
might not have all required information (e.g., degrees of freedom,
significance, Bayes factor, etc.) accessible in a consistent data type.
Depending on the specific test object and statistic in question, details
may be returned as a list, a matrix, an array, or a data frame. This
diversity can make it difficult to easily access all needed information
for hypothesis testing and estimation, and to switch from one
statistical approach to another.

This is where
[statsExpressions](https://www.indrapatil.com/statsExpressions/) comes
in: It can be thought of as a unified portal through which most of the
functionality in these underlying packages can be accessed, with a
simpler interface and with tidy data format.

## Comparison to Other Packages

Unlike [broom](https://broom.tidymodels.org/) ([Robinson, Hayes, &
Couch, 2021](#ref-Robinson2021)) or
[parameters](https://easystats.github.io/parameters/) ([Lüdecke,
Ben-Shachar, Patil, & Makowski, 2020](#ref-L%C3%BCdecke2020parameters)),
the goal of
[statsExpressions](https://www.indrapatil.com/statsExpressions/) is not
to convert model objects into tidy data frames, but to provide a
consistent and easy syntax to carry out statistical tests. Additionally,
none of these packages return statistical expressions.

## Consistent Syntax for Statistical Analysis

The package offers functions that allow users choose a statistical
approach without changing the syntax (i.e., by only specifying a single
argument). The functions always require a data frame in tidy format
([Wickham et al., 2019](#ref-Wickham2019)), and work with missing data.
Moreover, they always return a data frame that can be further utilized
downstream in the workflow (such as visualization).

| Function            | Parametric | Non-parametric | Robust | Bayesian |
|:--------------------|:-----------|:---------------|:-------|:---------|
| `one_sample_test`   | ✅         | ✅             | ✅     | ✅       |
| `two_sample_test`   | ✅         | ✅             | ✅     | ✅       |
| `oneway_anova`      | ✅         | ✅             | ✅     | ✅       |
| `corr_test`         | ✅         | ✅             | ✅     | ✅       |
| `contingency_table` | ✅         | ✅             | \-     | ✅       |
| `meta_analysis`     | ✅         | \-             | ✅     | ✅       |

A summary table listing the primary functions in the package and the
statistical approaches they support. More detailed description of the
tests and outputs from these functions can be found on the package
website: <https://www.indrapatil.com/statsExpressions/articles/>.
{.table}

[statsExpressions](https://www.indrapatil.com/statsExpressions/)
internally relies on `{stats}` package for parametric and non-parametric
([R Core Team, 2021](#ref-base2021)),
[WRS2](https://r-forge.r-project.org/projects/psychor/) package for
robust ([Mair & Wilcox, 2020](#ref-Mair2020)), and
[BayesFactor](https://richarddmorey.github.io/BayesFactor/) package for
Bayesian statistics ([Morey & Rouder, 2020](#ref-Morey2020)). The
random-effects meta-analysis is carried out using
[metafor](https://www.metafor-project.org) (parametric) ([Viechtbauer,
2010](#ref-Viechtbauer2010)), `{metaplus}` (robust) ([Beath,
2016](#ref-Beath2016)), and
[metaBMA](https://github.com/danheck/metaBMA) (Bayesian) ([Heck et al.,
2019](#ref-Heck2019)) packages. Additionally, it relies on `easystats`
packages ([Ben-Shachar, Lüdecke, & Makowski,
2020](#ref-Ben-Shachar2020); [Lüdecke et al.,
2020](#ref-L%C3%BCdecke2020parameters); [Lüdecke, Ben-Shachar, Patil,
Waggoner, & Makowski, 2021](#ref-L%C3%BCdecke2020performance); [Lüdecke,
Waggoner, & Makowski, 2019](#ref-L%C3%BCdecke2019); [Makowski,
Ben-Shachar, & Lüdecke, 2019](#ref-Makowski2019); [Makowski,
Ben-Shachar, Patil, & Lüdecke, 2020](#ref-Makowski2020)) to compute
appropriate effect size/posterior estimates and their
confidence/credible intervals.

## Tidy Dataframes from Statistical Analysis

To illustrate the simplicity of this syntax, let’s say we want to run a
one-way ANOVA. If we first run a non-parametric ANOVA and then decide to
run a robust ANOVA instead, the syntax remains the same and the
statistical approach can be modified by changing a single argument:

``` r

oneway_anova(mtcars, cyl, wt, type = "nonparametric")
#> # A tibble: 1 × 15
#>   parameter1 parameter2 statistic df.error   p.value
#>   <chr>      <chr>          <dbl>    <int>     <dbl>
#> 1 wt         cyl             22.8        2 0.0000112
#>   method                       effectsize      estimate conf.level conf.low
#>   <chr>                        <chr>              <dbl>      <dbl>    <dbl>
#> 1 Kruskal-Wallis rank sum test Epsilon2 (rank)    0.736       0.95    0.624
#>   conf.high conf.method          conf.iterations n.obs expression
#>       <dbl> <chr>                          <int> <int> <list>    
#> 1         1 percentile bootstrap             100    32 <language>

oneway_anova(mtcars, cyl, wt, type = "robust")
#> # A tibble: 1 × 12
#>   statistic    df df.error p.value
#>       <dbl> <dbl>    <dbl>   <dbl>
#> 1      12.7     2     12.2 0.00102
#>   method                                           
#>   <chr>                                            
#> 1 A heteroscedastic one-way ANOVA for trimmed means
#>   effectsize                         estimate conf.level conf.low conf.high
#>   <chr>                                 <dbl>      <dbl>    <dbl>     <dbl>
#> 1 Explanatory measure of effect size     1.05       0.95    0.843      1.50
#>   n.obs expression
#>   <int> <list>    
#> 1    32 <language>
```

These functions are also compatible with other popular data manipulation
packages. For example, we can use combination of
[dplyr](https://dplyr.tidyverse.org) and
[statsExpressions](https://www.indrapatil.com/statsExpressions/) to
repeat the same statistical analysis across grouping variables.

``` r

# running one-sample proportion test for `vs` at all levels of `am`
mtcars |>
  group_by(am) |>
  group_modify(~ contingency_table(.x, vs), .keep = TRUE) |>
  ungroup()
#> # A tibble: 2 × 14
#>      am statistic    df p.value method   effectsize estimate conf.level conf.low
#>   <dbl>     <dbl> <dbl>   <dbl> <chr>    <chr>         <dbl>      <dbl>    <dbl>
#> 1     0    1.32       1   0.251 Chi-squ… Pearson's…   0.254        0.95        0
#> 2     1    0.0769     1   0.782 Chi-squ… Pearson's…   0.0767       0.95        0
#> # ℹ 5 more variables: conf.high <dbl>, conf.method <chr>,
#> #   conf.distribution <chr>, n.obs <int>, expression <list>
```

## Custom Statistical Workflows

### Pairwise comparisons with automatic *p*-value adjustment

When an omnibus test (e.g., one-way ANOVA) is significant, you typically
follow up with pairwise comparisons. The
[`pairwise_comparisons()`](https://www.indrapatil.com/statsExpressions/reference/pairwise_comparisons.md)
function handles this with built-in *p*-value adjustment across all
pairwise tests:

``` r

pairwise_comparisons(
  data            = mtcars,
  x               = cyl,
  y               = wt,
  type            = "parametric",
  var.equal       = FALSE,
  p.adjust.method = "holm"
)
#> # A tibble: 3 × 9
#>   group1 group2 statistic   p.value alternative distribution p.adjust.method
#>   <chr>  <chr>      <dbl>     <dbl> <chr>       <chr>        <chr>          
#> 1 4      6           5.39 0.00831   two.sided   q            Holm           
#> 2 4      8           9.11 0.0000124 two.sided   q            Holm           
#> 3 6      8           5.12 0.00831   two.sided   q            Holm           
#>   test         expression
#>   <chr>        <list>    
#> 1 Games-Howell <language>
#> 2 Games-Howell <language>
#> 3 Games-Howell <language>
```

Each row contains a formatted `expression` column with the adjusted
*p*-value, ready for use as a plot annotation. The `p.adjust.method`
argument supports all methods from
[`stats::p.adjust()`](https://rdrr.io/r/stats/p.adjust.html) (e.g.,
`"holm"`, `"bonferroni"`, `"BH"`, `"fdr"`, `"none"`).

### Chaining multiple tests and adjusting *p*-values

You can chain different statistical tests on the same data, consolidate
the results into a single data frame, and apply *p*-value correction
across all comparisons:

``` r

multi_test_results <- dplyr::bind_rows(
  two_sample_test(mtcars, am, wt, type = "parametric"),
  two_sample_test(mtcars, am, wt, type = "nonparametric"),
  two_sample_test(mtcars, am, wt, type = "robust")
) |>
  dplyr::mutate(p.value = stats::p.adjust(p.value, method = "holm"))

dplyr::select(multi_test_results, method, effectsize, estimate, p.value)
#> # A tibble: 3 × 4
#>   method                                              
#>   <chr>                                               
#> 1 Welch Two Sample t-test                             
#> 2 Wilcoxon rank sum test                              
#> 3 Yuen's test on trimmed means for independent samples
#>   effectsize                                              estimate   p.value
#>   <chr>                                                      <dbl>     <dbl>
#> 1 Hedges' g                                                  1.88  0.0000188
#> 2 r (rank biserial)                                          0.866 0.0000869
#> 3 Algina-Keselman-Penfield robust standardized difference    2.48  0.0000869
```

### Generating formatted expressions from custom results

The
[`add_expression_col()`](https://www.indrapatil.com/statsExpressions/reference/add_expression_col.md)
function creates publication-ready plotmath expressions from any data
frame containing statistical details. This is useful when you run your
own tests outside
[statsExpressions](https://www.indrapatil.com/statsExpressions/) and
want to generate a formatted expression for plot annotations:

``` r

# suppose you have run your own statistical test
custom_stats <- cbind.data.frame(
  statistic  = 2.18,
  df         = 18,
  p.value    = 0.041,
  estimate   = 0.65,
  conf.level = 0.95,
  conf.low   = 0.03,
  conf.high  = 1.27,
  method     = "Student's t-test"
)

# generate a formatted expression
add_expression_col(
  data           = custom_stats,
  statistic.text = list(quote(italic("t"))),
  effsize.text   = list(quote(italic("d")["Cohen"])),
  n              = 20L,
  digits         = 2L,
  digits.df      = 0L
)$expression[[1]]
#> list(italic("t") * "(" * 18 * ")" == "2.18", italic(p) == "0.04", 
#>     italic("d")["Cohen"] == "0.65", CI["95%"] ~ "[" * "0.03", 
#>     "1.27" * "]", italic("n")["obs"] == "20")
```

## Expressions for Plots

In addition to other details contained in the data frame, there is also
a column titled `expression`, which contains a pre-formatted text with
statistical details. These expressions (Figure 1) attempt to follow the
gold standard in statistical reporting for both Bayesian ([Doorn et al.,
2020](#ref-van2020jasp)) and Frequentist ([American Psychological
Association and others, 2019](#ref-american2019publication)) frameworks.

![The templates used in \`{statsExpressions}\` to display statistical
details in a plot.](stats_reporting_format.png)

The templates used in
[statsExpressions](https://www.indrapatil.com/statsExpressions/) to
display statistical details in a plot.

This expression can be easily displayed in a plot (Figure 2). Displaying
statistical results in the context of a visualization is indeed a
philosophy adopted by the
[ggstatsplot](https://www.indrapatil.com/ggstatsplot/) package ([Patil,
2021](#ref-Patil2021)), and
[statsExpressions](https://www.indrapatil.com/statsExpressions/)
functions as its statistical processing backend.

``` r

# needed libraries
library(ggplot2)

# Example 1: Performing a t-test on raw tidy data and extracting the formatted expression
# for direct use in a plot annotation
res_ttest <- two_sample_test(mtcars, am, wt, type = "parametric")

ggplot(mtcars, aes(as.factor(am), wt)) +
  geom_boxplot() +
  labs(
    x = "Transmission (0 = automatic, 1 = manual)",
    y = "Weight (1000 lbs)",
    title = "Vehicle Weight by Transmission Type",
    subtitle = res_ttest$expression[[1]] # Extract formatted p-value and effect size
  )
```

![Example illustrating how \`{statsExpressions}\` functions can be used
to display results from a statistical test in a
plot.](statsExpressions_files/figure-html/anova_example-1.png)

Example illustrating how
[statsExpressions](https://www.indrapatil.com/statsExpressions/)
functions can be used to display results from a statistical test in a
plot.

``` r


# Example 2: One-way ANOVA
res_anova <- oneway_anova(iris, Species, Sepal.Length, type = "nonparametric")

ggplot(iris, aes(x = Sepal.Length, y = Species)) +
  geom_boxplot() + # use 'expression' column to display results in the subtitle
  labs(
    x = "Penguin Species",
    y = "Body mass (in grams)",
    title = "Kruskal-Wallis Rank Sum Test",
    subtitle = res_anova$expression[[1]]
  )
```

![Example illustrating how \`{statsExpressions}\` functions can be used
to display results from a statistical test in a
plot.](statsExpressions_files/figure-html/anova_example-2.png)

Example illustrating how
[statsExpressions](https://www.indrapatil.com/statsExpressions/)
functions can be used to display results from a statistical test in a
plot.

## Licensing and Availability

[statsExpressions](https://www.indrapatil.com/statsExpressions/) is
licensed under the GNU General Public License (v3.0), with all source
code stored at
[GitHub](https://github.com/IndrajeetPatil/statsExpressions/). In the
spirit of honest and open science, requests and suggestions for fixes,
feature updates, as well as general questions and concerns are
encouraged via direct interaction with contributors and developers by
filing an
[issue](https://github.com/IndrajeetPatil/statsExpressions/issues).

## Acknowledgements

I would like to acknowledge the support of Mina Cikara, Fiery Cushman,
and Iyad Rahwan during the development of this project.
[statsExpressions](https://www.indrapatil.com/statsExpressions/) relies
heavily on the [`easystats`](https://github.com/easystats/easystats)
ecosystem, a collaborative project created to facilitate the usage of
`R` for statistical analyses. Thus, I would like to thank the [members
of easystats](https://github.com/orgs/easystats/people) as well as the
users.

## References

American Psychological Association and others. (2019). *Publication
Manual of the American Psychological Association* (7th Edition.).
American Psychological Association.

Beath, K. J. (2016). metaplus: An R package for the analysis of robust
meta-analysis and meta-regression. *R Journal*, *8*(1), 5–16.
doi:[10.32614/RJ-2016-001](https://doi.org/10.32614/RJ-2016-001)

Ben-Shachar, M. S., Lüdecke, D., & Makowski, D. (2020). effectsize:
Estimation of effect size indices and standardized parameters. *Journal
of Open Source Software*, *5*(56), 2815.
doi:[10.21105/joss.02815](https://doi.org/10.21105/joss.02815)

Doorn, J. van, Bergh, D. van den, Böhm, U., Dablander, F., Derks, K.,
Draws, T., Etz, A., et al. (2020). The JASP guidelines for conducting
and reporting a bayesian analysis. *Psychonomic Bulletin & Review*,
1–14.
doi:[10.3758/s13423-020-01798-5](https://doi.org/10.3758/s13423-020-01798-5)

Heck, W., D., Gronau, F., Q., Wagenmakers, &, & E.-J. (2019). *metaBMA:
Bayesian model averaging for random and fixed effects meta-analysis*.
Retrieved from <https://CRAN.R-project.org/package=metaBMA>

Lüdecke, D., Ben-Shachar, M. S., Patil, I., & Makowski, D. (2020).
parameters: Extracting, computing and exploring the parameters of
statistical models using R. *Journal of Open Source Software*, *5*(53),
2445. doi:[10.21105/joss.02445](https://doi.org/10.21105/joss.02445)

Lüdecke, D., Ben-Shachar, M. S., Patil, I., Waggoner, P., & Makowski, D.
(2021). performance: An R package for assessment, comparison and testing
of statistical models. *Journal of Open Source Software*, *6*(60), 3139.
doi:[10.21105/joss.03139](https://doi.org/10.21105/joss.03139)

Lüdecke, D., Waggoner, P., & Makowski, D. (2019). insight: A unified
interface to access information from model objects in R. *Journal of
Open Source Software*, *4*(38), 1412.
doi:[10.21105/joss.01412](https://doi.org/10.21105/joss.01412)

Mair, P., & Wilcox, R. (2020). Robust Statistical Methods in R Using the
WRS2 Package. *Behavior Research Methods*, *52*, 464–488.
doi:[10.3758/s13428-019-01246-w](https://doi.org/10.3758/s13428-019-01246-w)

Makowski, D., Ben-Shachar, M. S., & Lüdecke, D. (2019). bayestestR:
Describing effects and their uncertainty, existence and significance
within the bayesian framework. *Journal of Open Source Software*,
*4*(40), 1541.
doi:[10.21105/joss.01541](https://doi.org/10.21105/joss.01541)

Makowski, D., Ben-Shachar, M. S., Patil, I., & Lüdecke, D. (2020).
Methods and algorithms for correlation analysis in r. *Journal of Open
Source Software*, *5*(51), 2306.
doi:[10.21105/joss.02306](https://doi.org/10.21105/joss.02306)

Morey, R. D., & Rouder, J. N. (2020). *BayesFactor: Computation of bayes
factors for common designs*. Retrieved from
<https://richarddmorey.github.io/BayesFactor/>

Patil, I. (2021). Visualizations with statistical details: The
’ggstatsplot’ approach. *PsyArxiv*.
doi:[10.31234/osf.io/p7mku](https://doi.org/10.31234/osf.io/p7mku)

R Core Team. (2021). *R: A language and environment for statistical
computing*. Vienna, Austria: R Foundation for Statistical Computing.
Retrieved from <https://www.R-project.org/>

Robinson, D., Hayes, A., & Couch, S. (2021). *Broom: Convert statistical
objects into tidy tibbles*. Retrieved from
<https://CRAN.R-project.org/package=broom>

Viechtbauer, W. (2010). Conducting meta-analyses in R with the metafor
package. *Journal of Statistical Software*, *36*(3), 1–48. Retrieved
from <https://www.jstatsoft.org/v36/i03/>

Wickham, H., Averick, M., Bryan, J., Chang, W., McGowan, L. D.,
François, R., Grolemund, G., et al. (2019). Welcome to the tidyverse.
*Journal of Open Source Software*, *4*(43), 1686.
doi:[10.21105/joss.01686](https://doi.org/10.21105/joss.01686)
