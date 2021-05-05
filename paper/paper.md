---
title: 'statsExpressions: R Package for Tidy Dataframes and Expressions with Statistical Details'
tags:
  - R
  - parametric statistics
  - nonparametric statistics
  - robust statistics
  - Bayesian statistics
  - tidy
authors:
  - name: Indrajeet Patil
    orcid: 0000-0003-1995-6531
    affiliation: 1
affiliations:
  - name: Center for Humans and Machines, Max Planck Institute for Human Development, Berlin, Germany
    index: 1
date: "2021-05-05"
bibliography: paper.bib
---



# Summary

The `statsExpressions` package has two key aims: to provide a consistent syntax
to do statistical analysis with tidy data, and to provide statistical
expressions (i.e., a pre-formatted in-text statistical result) for plotting
functions. Currently, it supports common types of statistical approaches and
tests: parametric, nonparametric, robust, and Bayesian *t*-test, one-way ANOVA,
correlation analyses, contingency table analyses, and meta-analyses. The
functions are pipe-friendly and compatible with tidy data.

# Statement of need

Statistical packages exhibit substantial diversity in terms of their syntax and
expected input type. This can make it difficult to switch from one statistical
approach to another. For example, some functions expect vectors as inputs, while
others expect dataframes. Depending on whether it is a repeated measures design
or not, different functions might expect data to be in wide or long format. Some
functions can internally omit missing values, while other functions error in
their presence. Furthermore, if someone wishes to utilize the objects returned
by these packages downstream in their workflow, this is not straightforward
either because even functions from the same package can return a list, a matrix,
an array, a dataframe, etc., depending on the function.

This is where `statsExpressions` comes in: It can be thought of as a unified
portal through which most of the functionality in these underlying packages can
be accessed, with a simpler interface and no requirement to change data format.

# Comparison to Other Packages

Unlike `broom` [@Robinson2021] or `parameters` [@Lüdecke2020parameters], the
goal of `statsExpressions` is not to convert model objects into tidy dataframes,
but to provide a consistent and easy syntax to carry out statistical tests.
Additionally, none of these packages return statistical expressions.

# Tidy Dataframes from Statistical Analysis

The package offers six primary functions that let users choose a statistical
approach without changing the syntax (i.e., by only specifying a single
argument). The users are always expected to provide a dataframe in tidy format
[@Wickham2019], and all functions work with missing data. Moreover, they always
return a dataframe that can be further utilized downstream in the workflow (for
a visualization, e.g.).

Function | Parametric | Non-parametric | Robust | Bayesian
------------------ | ---- | ----- | ----| ----- 
`one_sample_test` | \checkmark | \checkmark | \checkmark | \checkmark
`two_sample_test` | \checkmark | \checkmark | \checkmark | \checkmark
`oneway_anova` | \checkmark | \checkmark | \checkmark | \checkmark
`corr_test` | \checkmark | \checkmark | \checkmark | \checkmark
`contingency_table` | \checkmark | \checkmark | - | \checkmark
`meta_analysis` | \checkmark | - | \checkmark | \checkmark

: A summary table listing the primary functions in the package and the
statistical approaches they support. For a more detailed description of the
tests and outputs from these functions, the readers are encouraged to read
vignettes on the package website: <https://indrajeetpatil.github.io/statsExpressions/articles/>.

`statsExpressions` internally relies on `stats` package for parametric and
non-parametric [@base2021], `WRS2` package for robust [@Mair2020], and
`BayesFactor` package for Bayesian statistics [@Morey2020]. The random-effects
meta-analysis is carried out using `metafor` (parametric) [@Viechtbauer2010],
`metaplus` (robust) [@Beath2016], and `metaBMA` (Bayesian) [@Heck2019] packages.
Additionally, it relies on `easystats` packages [@Ben-Shachar2020;
@Lüdecke2020parameters;
@Lüdecke2020performance; @Lüdecke2019; @Makowski2019; @Makowski2020] to compute
appropriate effect size/posterior estimates and their confidence/credible
intervals.

To illustrate the simplicity of this syntax, let's say we want to run a
two-sample *t*-test. If we first run a parametric *t*-test and then decide to
run a robust *t*-test instead, the syntax remains the same and the statistical
approach can be modified by changing a single argument:


```r
mtcars %>% two_sample_test(am, wt, type = "parametric") # Welch's t-test
#> # A tibble: 1 x 14
#>   term  group mean.group1 mean.group2 statistic df.error    p.value
#>   <chr> <chr>       <dbl>       <dbl>     <dbl>    <dbl>      <dbl>
#> 1 wt    am           3.77        2.41      5.49     29.2 0.00000627
#>   method                  estimate conf.level conf.low conf.high effectsize
#>   <chr>                      <dbl>      <dbl>    <dbl>     <dbl> <chr>     
#> 1 Welch Two Sample t-test     1.84       0.95     1.00      2.66 Hedges' g 
#>   expression
#>   <list>    
#> 1 <language>

mtcars %>% two_sample_test(am, wt, type = "robust") # Yuen's t-test
#> # A tibble: 1 x 10
#>   statistic df.error   p.value
#>       <dbl>    <dbl>     <dbl>
#> 1      5.84     13.6 0.0000485
#>   method                                               estimate conf.low
#>   <chr>                                                   <dbl>    <dbl>
#> 1 Yuen's test on trimmed means for independent samples    0.915    0.754
#>   conf.high conf.level effectsize                         expression
#>       <dbl>      <dbl> <chr>                              <list>    
#> 1     0.977       0.95 Explanatory measure of effect size <language>
```

These functions are also compatible with other popular data manipulation
packages (see Appendix for an example).

# Expressions for Plots

In addition to other details contained in the dataframe, there is also a column
titled `expression`, which contains a pre-formatted text with statistical
details. These expressions (Figure 1) attempt to follow the gold standard in
statistical reporting for both Bayesian [@van2020jasp] and Frequentist
[@american2019publication] frameworks.

\begin{figure}
\includegraphics[width=1\linewidth]{stats_reporting_format} \caption{The templates used in `statsExpressions` to display statistical details in a plot.}\label{fig:expr_template}
\end{figure}

This expression be easily displayed in a plot (Figure 2). Displaying statistical
results in the context of a visualization is indeed a philosophy adopted by the
`ggstatsplot` package [@Patil2021], and `statsExpressions` functions as its
statistical processing backend.

\begin{figure}
\includegraphics[width=1\linewidth]{paper_files/figure-latex/anova_example-1} \caption{Example illustrating how `statsExpressions` functions can be used to display results from a statistical test in a plot. Code to create this figure is reported in Appendix.}\label{fig:anova_example}
\end{figure}

# Licensing and Availability

`statsExpressions` is licensed under the GNU General Public License (v3.0), with all
source code stored at [GitHub](https://github.com/IndrajeetPatil/statsExpressions/).
In the spirit of honest and open science, requests and suggestions for fixes,
feature updates, as well as general questions and concerns are encouraged via
direct interaction with contributors and developers by filing an
[issue](https://github.com/IndrajeetPatil/statsExpressions/issues) while respecting
[*Contribution Guidelines*](https://indrajeetpatil.github.io/statsExpressions/CONTRIBUTING.html).

# Acknowledgements

I would like to acknowledge the support of Mina Cikara, Fiery Cushman, and Iyad
Rahwan during the development of this project. `statsExpressions` relies heavily
on the [`easystats`](https://github.com/easystats/easystats) ecosystem, a
collaborative project created to facilitate the usage of `R` for statistical
analyses. Thus, I would like to thank the [members of easystats](https://github.com/orgs/easystats/people) as well as the users.

# References

<div id="refs"></div>

# Appendix

## Example with `dplyr`

We can use combination of `dplyr` and `statsExpressions` to repeat the same
statistical analysis across grouping variables.


```r
# running one-sample proportion test for all levels of `cyl`
mtcars %>%
  group_by(cyl) %>%
  group_modify(~ contingency_table(.x, am), .keep = TRUE) %>%
  ungroup()
#> # A tibble: 3 x 11
#>     cyl statistic    df p.value method                                  
#>   <dbl>     <dbl> <dbl>   <dbl> <chr>                                   
#> 1     4     2.27      1 0.132   Chi-squared test for given probabilities
#> 2     6     0.143     1 0.705   Chi-squared test for given probabilities
#> 3     8     7.14      1 0.00753 Chi-squared test for given probabilities
#>   estimate conf.level conf.low conf.high effectsize        expression
#>      <dbl>      <dbl>    <dbl>     <dbl> <chr>             <list>    
#> 1    0.344       0.95    0         0.917 Cramer's V (adj.) <language>
#> 2    0           0.95    0         0     Cramer's V (adj.) <language>
#> 3    0.685       0.95    0.127     1.18  Cramer's V (adj.) <language>
```

## Code to reproduce for Figure 2


```r
# needed libraries
library(statsExpressions)
library(ggplot2)
library(ggridges)
library(palmerpenguins) # `penguins` dataset is from this package

# creating a dataframe
res <- oneway_anova(penguins, species, body_mass_g, type = "nonparametric")

# create a ridgeplot using `ggridges` package
ggplot(penguins, aes(x = body_mass_g, y = species)) +
  geom_density_ridges(
    jittered_points = TRUE,
    quantile_lines = TRUE,
    scale = 0.9,
    vline_size = 1,
    vline_color = "red",
    position = position_raincloud(adjust_vlines = TRUE)
  ) + # use 'expression' column to display results in the subtitle
  labs(
    x = "Penguin species",
    y = "Body mass (in grams)",
    title = "Kruskal-Wallis Rank Sum Test",
    subtitle = res$expression[[1]]
  )
```
