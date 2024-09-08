---
title: "Test and effect size details"
output:
  rmarkdown::html_vignette:
    toc: true
    toc_depth: 4
    keep_md: true
vignette: >
  %\VignetteIndexEntry{Test and effect size details}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---



This vignette can be cited as:


```
To cite package 'statsExpressions' in publications use:

  Patil, I., (2021). statsExpressions: R Package for Tidy Dataframes and Expressions with Statistical Details.
  Journal of Open Source Software, 6(61), 3236, https://doi.org/10.21105/joss.03236

A BibTeX entry for LaTeX users is

  @Article{,
    doi = {10.21105/joss.03236},
    url = {https://doi.org/10.21105/joss.03236},
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

## Introduction

Here a go-to summary about statistical test carried out and the returned effect
size for each function is provided. This should be useful if one needs to find
out more information about how an argument is resolved in the underlying package
or if one wishes to browse the source code. So, for example, if you want to know
more about how one-way (between-subjects) ANOVA, you can run
`?stats::oneway.test` in your R console.

Abbreviations used: CI = Confidence Interval

## Summary of functionality


**Summary of available analyses**

| Test                       | Function                 |
| :------------------------- | :----------------------- |
| one-sample *t*-test        | `one_sample_test()`      |
| two-sample *t*-test        | `two_sample_test()`      |
| one-way ANOVA              | `oneway_anova()`         |
| correlation analysis       | `corr_test()`            |
| contingency table analysis | `contingency_table()`    |
| meta-analysis              | `meta_analysis()`        |
| pairwise comparisons       | `pairwise_comparisons()` |

**Summary of details available for analyses**

| Analysis                        | Hypothesis testing | Effect size estimation |
| :------------------------------ | :----------------- | :--------------------- |
| (one/two-sample) *t*-test       | ✅                 | ✅                     |
| one-way ANOVA                   | ✅                 | ✅                     |
| correlation                     | ✅                 | ✅                     |
| (one/two-way) contingency table | ✅                 | ✅                     |
| random-effects meta-analysis    | ✅                 | ✅                     |

**Summary of supported statistical approaches**

| Description                                       | Parametric | Non-parametric | Robust | Bayesian |
| :------------------------------------------------ | :--------- | :------------- | :----- | :------- |
| Between group/condition comparisons               | ✅         | ✅             | ✅     | ✅       |
| Within group/condition comparisons                | ✅         | ✅             | ✅     | ✅       |
| Distribution of a numeric variable                | ✅         | ✅             | ✅     | ✅       |
| Correlation between two variables                 | ✅         | ✅             | ✅     | ✅       |
| Association between categorical variables         | ✅         | ✅             | ❌     | ✅       |
| Equal proportions for categorical variable levels | ✅         | ✅             | ❌     | ✅       |
| Random-effects meta-analysis                      | ✅         | ❌             | ✅     | ✅       |

## Summary of tests and effect sizes

Here a go-to summary about statistical test carried out and the returned effect
size for each function is provided. This should be useful if one needs to find
out more information about how an argument is resolved in the underlying package
or if one wishes to browse the source code. So, for example, if you want to know
more about how one-way (between-subjects) ANOVA, you can run
`?stats::oneway.test` in your R console.

### `centrality_description()`


| Type           | Measure      | Function used                         |
| :------------- | :----------- | :------------------------------------ |
| Parametric     | mean         | `datawizard::describe_distribution()` |
| Non-parametric | median       | `datawizard::describe_distribution()` |
| Robust         | trimmed mean | `datawizard::describe_distribution()` |
| Bayesian       | MAP          | `datawizard::describe_distribution()` |

### `oneway_anova()`


#### between-subjects

**Hypothesis testing**

| Type           | No. of groups | Test                                            | Function used            |
| :------------- | :------------ | :---------------------------------------------- | :----------------------- |
| Parametric     | > 2           | Fisher's or Welch's one-way ANOVA               | `stats::oneway.test()`   |
| Non-parametric | > 2           | Kruskal-Wallis one-way ANOVA                    | `stats::kruskal.test()`  |
| Robust         | > 2           | Heteroscedastic one-way ANOVA for trimmed means | `WRS2::t1way()`          |
| Bayes Factor   | > 2           | Fisher's ANOVA                                  | `BayesFactor::anovaBF()` |

**Effect size estimation**

| Type           | No. of groups | Effect size                                | CI available? | Function used                                              |
| :------------- | :------------ | :----------------------------------------- | :------------ | :--------------------------------------------------------- |
| Parametric     | > 2           | partial eta-squared, partial omega-squared | Yes           | `effectsize::omega_squared()`, `effectsize::eta_squared()` |
| Non-parametric | > 2           | rank epsilon squared                       | Yes           | `effectsize::rank_epsilon_squared()`                       |
| Robust         | > 2           | Explanatory measure of effect size         | Yes           | `WRS2::t1way()`                                            |
| Bayes Factor   | > 2           | Bayesian R-squared                         | Yes           | `performance::r2_bayes()`                                  |

#### within-subjects

**Hypothesis testing**

| Type           | No. of groups | Test                                                              | Function used            |
| :------------- | :------------ | :---------------------------------------------------------------- | :----------------------- |
| Parametric     | > 2           | One-way repeated measures ANOVA                                   | `afex::aov_ez()`         |
| Non-parametric | > 2           | Friedman rank sum test                                            | `stats::friedman.test()` |
| Robust         | > 2           | Heteroscedastic one-way repeated measures ANOVA for trimmed means | `WRS2::rmanova()`        |
| Bayes Factor   | > 2           | One-way repeated measures ANOVA                                   | `BayesFactor::anovaBF()` |

**Effect size estimation**

| Type           | No. of groups | Effect size                                                     | CI available? | Function used                                              |
| :------------- | :------------ | :-------------------------------------------------------------- | :------------ | :--------------------------------------------------------- |
| Parametric     | > 2           | partial eta-squared, partial omega-squared                      | Yes           | `effectsize::omega_squared()`, `effectsize::eta_squared()` |
| Non-parametric | > 2           | Kendall's coefficient of concordance                            | Yes           | `effectsize::kendalls_w()`                                 |
| Robust         | > 2           | Algina-Keselman-Penfield robust standardized difference average | Yes           | `WRS2::wmcpAKP()`                                          |
| Bayes Factor   | > 2           | Bayesian R-squared                                              | Yes           | `performance::r2_bayes()`                                  |

### `two_sample_test()` 


#### between-subjects

**Hypothesis testing**

| Type           | No. of groups | Test                          | Function used            |
| :------------- | :------------ | :---------------------------- | :----------------------- |
| Parametric     | 2             | Student's or Welch's *t*-test | `stats::t.test()`        |
| Non-parametric | 2             | Mann-Whitney *U* test         | `stats::wilcox.test()`   |
| Robust         | 2             | Yuen's test for trimmed means | `WRS2::yuen()`           |
| Bayesian       | 2             | Student's *t*-test            | `BayesFactor::ttestBF()` |

**Effect size estimation**

| Type           | No. of groups | Effect size                                             | CI available? | Function used                                      |
| :------------- | :------------ | :------------------------------------------------------ | :------------ | :------------------------------------------------- |
| Parametric     | 2             | Cohen's *d*, Hedge's *g*                                | Yes           | `effectsize::cohens_d()`, `effectsize::hedges_g()` |
| Non-parametric | 2             | *r* (rank-biserial correlation)                         | Yes           | `effectsize::rank_biserial()`                      |
| Robust         | 2             | Algina-Keselman-Penfield robust standardized difference | Yes           | `WRS2::akp.effect()`                               |
| Bayesian       | 2             | difference                                              | Yes           | `bayestestR::describe_posterior()`                 |

#### within-subjects

**Hypothesis testing**

| Type           | No. of groups | Test                                               | Function used            |
| :------------- | :------------ | :------------------------------------------------- | :----------------------- |
| Parametric     | 2             | Student's *t*-test                                 | `stats::t.test()`        |
| Non-parametric | 2             | Wilcoxon signed-rank test                          | `stats::wilcox.test()`   |
| Robust         | 2             | Yuen's test on trimmed means for dependent samples | `WRS2::yuend()`          |
| Bayesian       | 2             | Student's *t*-test                                 | `BayesFactor::ttestBF()` |

**Effect size estimation**

| Type           | No. of groups | Effect size                                             | CI available? | Function used                                      |
| :------------- | :------------ | :------------------------------------------------------ | :------------ | :------------------------------------------------- |
| Parametric     | 2             | Cohen's *d*, Hedge's *g*                                | Yes           | `effectsize::cohens_d()`, `effectsize::hedges_g()` |
| Non-parametric | 2             | *r* (rank-biserial correlation)                         | Yes           | `effectsize::rank_biserial()`                      |
| Robust         | 2             | Algina-Keselman-Penfield robust standardized difference | Yes           | `WRS2::wmcpAKP()`                                  |
| Bayesian       | 2             | difference                                              | Yes           | `bayestestR::describe_posterior()`                 |

### `one_sample_test()`



**Hypothesis testing**

| Type           | Test                                     | Function used            |
| :------------- | :--------------------------------------- | :----------------------- |
| Parametric     | One-sample Student's *t*-test            | `stats::t.test()`        |
| Non-parametric | One-sample Wilcoxon test                 | `stats::wilcox.test()`   |
| Robust         | Bootstrap-*t* method for one-sample test | `WRS2::trimcibt()`       |
| Bayesian       | One-sample Student's *t*-test            | `BayesFactor::ttestBF()` |

**Effect size estimation**

| Type           | Effect size                     | CI available? | Function used                                      |
| :------------- | :------------------------------ | :------------ | :------------------------------------------------- |
| Parametric     | Cohen's *d*, Hedge's *g*        | Yes           | `effectsize::cohens_d()`, `effectsize::hedges_g()` |
| Non-parametric | *r* (rank-biserial correlation) | Yes           | `effectsize::rank_biserial()`                      |
| Robust         | trimmed mean                    | Yes           | `WRS2::trimcibt()`                                 |
| Bayes Factor   | difference                      | Yes           | `bayestestR::describe_posterior()`                 |

### `corr_test()`


**Hypothesis testing** and **Effect size estimation**

| Type           | Test                                         | CI available? | Function used                |
| :------------- | :------------------------------------------- | :------------ | :--------------------------- |
| Parametric     | Pearson's correlation coefficient            | Yes           | `correlation::correlation()` |
| Non-parametric | Spearman's rank correlation coefficient      | Yes           | `correlation::correlation()` |
| Robust         | Winsorized Pearson's correlation coefficient | Yes           | `correlation::correlation()` |
| Bayesian       | Bayesian Pearson's correlation coefficient   | Yes           | `correlation::correlation()` |

### `contingency_table()`


#### two-way table

**Hypothesis testing**

| Type                      | Design   | Test                                | Function used                       |
| :------------------------ | :------- | :---------------------------------- | :---------------------------------- |
| Parametric/Non-parametric | Unpaired | Pearson's chi-squared test          | `stats::chisq.test()`               |
| Bayesian                  | Unpaired | Bayesian Pearson's chi-squared test | `BayesFactor::contingencyTableBF()` |
| Parametric/Non-parametric | Paired   | McNemar's chi-squared test          | `stats::mcnemar.test()`             |
| Bayesian                  | Paired   | No                                  | No                                  |

**Effect size estimation**

| Type                      | Design   | Effect size  | CI available? | Function used             |
| :------------------------ | :------- | :----------- | :------------ | :------------------------ |
| Parametric/Non-parametric | Unpaired | Cramer's *V* | Yes           | `effectsize::cramers_v()` |
| Bayesian                  | Unpaired | Cramer's *V* | Yes           | `effectsize::cramers_v()` |
| Parametric/Non-parametric | Paired   | Cohen's *g*  | Yes           | `effectsize::cohens_g()`  |
| Bayesian                  | Paired   | No           | No            | No                        |

#### one-way table

**Hypothesis testing**

| Type                      | Test                                      | Function used         |
| :------------------------ | :---------------------------------------- | :-------------------- |
| Parametric/Non-parametric | Goodness of fit chi-squared test          | `stats::chisq.test()` |
| Bayesian                  | Bayesian Goodness of fit chi-squared test | (custom)              |

**Effect size estimation**

| Type                      | Effect size   | CI available? | Function used              |
| :------------------------ | :------------ | :------------ | :------------------------- |
| Parametric/Non-parametric | Pearson's *C* | Yes           | `effectsize::pearsons_c()` |
| Bayesian                  | No            | No            | No                         |

### `meta_analysis()`


**Hypothesis testing** and **Effect size estimation**

| Type       | Test                                             | Effect size | CI available? | Function used            |
| :--------- | :----------------------------------------------- | :---------- | :------------ | :----------------------- |
| Parametric | Meta-analysis via random-effects models          | *beta*      | Yes           | `metafor::metafor()`     |
| Robust     | Meta-analysis via robust random-effects models   | *beta*      | Yes           | `metaplus::metaplus()`   |
| Bayes      | Meta-analysis via Bayesian random-effects models | *beta*      | Yes           | `metaBMA::meta_random()` |

## Effect size interpretation

See `{effectsize}`'s interpretation functions to check different rules/conventions
to interpret effect sizes:

<https://easystats.github.io/effectsize/reference/index.html#section-interpretation>

## References

  - For parametric and non-parametric effect sizes:
    <https://easystats.github.io/effectsize/articles/>

  - For robust effect sizes:
    <https://CRAN.R-project.org/package=WRS2/vignettes/WRS2.pdf>

  - For Bayesian posterior estimates:
    <https://easystats.github.io/bayestestR/articles/bayes_factors.html>

## Suggestions

If you find any bugs or have any suggestions/remarks, please file an issue on GitHub: 
<https://github.com/IndrajeetPatil/statsExpressions/issues>
