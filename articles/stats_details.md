# Test and effect size details

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

Here a go-to summary about statistical test carried out and the returned
effect size for each function is provided. This should be useful if one
needs to find out more information about how an argument is resolved in
the underlying package or if one wishes to browse the source code. So,
for example, if you want to know more about how one-way
(between-subjects) ANOVA, you can run
[`?stats::oneway.test`](https://rdrr.io/r/stats/oneway.test.html) in
your R console.

Abbreviations used: CI = Confidence Interval

## Summary of functionality

**Summary of available analyses**

| Test | Function |
|:---|:---|
| one-sample *t*-test | [`one_sample_test()`](https://indrajeetpatil.github.io/statsExpressions/reference/one_sample_test.md) |
| two-sample *t*-test | [`two_sample_test()`](https://indrajeetpatil.github.io/statsExpressions/reference/two_sample_test.md) |
| one-way ANOVA | [`oneway_anova()`](https://indrajeetpatil.github.io/statsExpressions/reference/oneway_anova.md) |
| correlation analysis | [`corr_test()`](https://indrajeetpatil.github.io/statsExpressions/reference/corr_test.md) |
| contingency table analysis | [`contingency_table()`](https://indrajeetpatil.github.io/statsExpressions/reference/contingency_table.md) |
| meta-analysis | [`meta_analysis()`](https://indrajeetpatil.github.io/statsExpressions/reference/meta_analysis.md) |
| pairwise comparisons | [`pairwise_comparisons()`](https://indrajeetpatil.github.io/statsExpressions/reference/pairwise_comparisons.md) |

**Summary of details available for analyses**

| Analysis                        | Hypothesis testing | Effect size estimation |
|:--------------------------------|:-------------------|:-----------------------|
| (one/two-sample) *t*-test       | ✅                 | ✅                     |
| one-way ANOVA                   | ✅                 | ✅                     |
| correlation                     | ✅                 | ✅                     |
| (one/two-way) contingency table | ✅                 | ✅                     |
| random-effects meta-analysis    | ✅                 | ✅                     |

**Summary of supported statistical approaches**

| Description | Parametric | Non-parametric | Robust | Bayesian |
|:---|:---|:---|:---|:---|
| Between group/condition comparisons | ✅ | ✅ | ✅ | ✅ |
| Within group/condition comparisons | ✅ | ✅ | ✅ | ✅ |
| Distribution of a numeric variable | ✅ | ✅ | ✅ | ✅ |
| Correlation between two variables | ✅ | ✅ | ✅ | ✅ |
| Association between categorical variables | ✅ | ✅ | ❌ | ✅ |
| Equal proportions for categorical variable levels | ✅ | ✅ | ❌ | ✅ |
| Random-effects meta-analysis | ✅ | ❌ | ✅ | ✅ |

## Summary of tests and effect sizes

Here a go-to summary about statistical test carried out and the returned
effect size for each function is provided. This should be useful if one
needs to find out more information about how an argument is resolved in
the underlying package or if one wishes to browse the source code. So,
for example, if you want to know more about how one-way
(between-subjects) ANOVA, you can run
[`?stats::oneway.test`](https://rdrr.io/r/stats/oneway.test.html) in
your R console.

### `centrality_description()`

| Type | Measure | Function used |
|:---|:---|:---|
| Parametric | mean | [`datawizard::describe_distribution()`](https://easystats.github.io/datawizard/reference/describe_distribution.html) |
| Non-parametric | median | [`datawizard::describe_distribution()`](https://easystats.github.io/datawizard/reference/describe_distribution.html) |
| Robust | trimmed mean | [`datawizard::describe_distribution()`](https://easystats.github.io/datawizard/reference/describe_distribution.html) |
| Bayesian | MAP | [`datawizard::describe_distribution()`](https://easystats.github.io/datawizard/reference/describe_distribution.html) |

### `oneway_anova()`

#### between-subjects

**Hypothesis testing**

| Type | No. of groups | Test | Function used |
|:---|:---|:---|:---|
| Parametric | \> 2 | Fisher’s or Welch’s one-way ANOVA | [`stats::oneway.test()`](https://rdrr.io/r/stats/oneway.test.html) |
| Non-parametric | \> 2 | Kruskal-Wallis one-way ANOVA | [`stats::kruskal.test()`](https://rdrr.io/r/stats/kruskal.test.html) |
| Robust | \> 2 | Heteroscedastic one-way ANOVA for trimmed means | [`WRS2::t1way()`](https://rdrr.io/pkg/WRS2/man/t1way.html) |
| Bayes Factor | \> 2 | Fisher’s ANOVA | [`BayesFactor::anovaBF()`](https://rdrr.io/pkg/BayesFactor/man/anovaBF.html) |

**Effect size estimation**

| Type | No. of groups | Effect size | CI available? | Function used |
|:---|:---|:---|:---|:---|
| Parametric | \> 2 | partial eta-squared, partial omega-squared | Yes | [`effectsize::omega_squared()`](https://easystats.github.io/effectsize/reference/eta_squared.html), [`effectsize::eta_squared()`](https://easystats.github.io/effectsize/reference/eta_squared.html) |
| Non-parametric | \> 2 | rank epsilon squared | Yes | [`effectsize::rank_epsilon_squared()`](https://easystats.github.io/effectsize/reference/rank_epsilon_squared.html) |
| Robust | \> 2 | Explanatory measure of effect size | Yes | [`WRS2::t1way()`](https://rdrr.io/pkg/WRS2/man/t1way.html) |
| Bayes Factor | \> 2 | Bayesian R-squared | Yes | [`performance::r2_bayes()`](https://easystats.github.io/performance/reference/r2_bayes.html) |

#### within-subjects

**Hypothesis testing**

| Type | No. of groups | Test | Function used |
|:---|:---|:---|:---|
| Parametric | \> 2 | One-way repeated measures ANOVA | [`afex::aov_ez()`](https://rdrr.io/pkg/afex/man/aov_car.html) |
| Non-parametric | \> 2 | Friedman rank sum test | [`stats::friedman.test()`](https://rdrr.io/r/stats/friedman.test.html) |
| Robust | \> 2 | Heteroscedastic one-way repeated measures ANOVA for trimmed means | [`WRS2::rmanova()`](https://rdrr.io/pkg/WRS2/man/rmanova.html) |
| Bayes Factor | \> 2 | One-way repeated measures ANOVA | [`BayesFactor::anovaBF()`](https://rdrr.io/pkg/BayesFactor/man/anovaBF.html) |

**Effect size estimation**

| Type | No. of groups | Effect size | CI available? | Function used |
|:---|:---|:---|:---|:---|
| Parametric | \> 2 | partial eta-squared, partial omega-squared | Yes | [`effectsize::omega_squared()`](https://easystats.github.io/effectsize/reference/eta_squared.html), [`effectsize::eta_squared()`](https://easystats.github.io/effectsize/reference/eta_squared.html) |
| Non-parametric | \> 2 | Kendall’s coefficient of concordance | Yes | [`effectsize::kendalls_w()`](https://easystats.github.io/effectsize/reference/rank_epsilon_squared.html) |
| Robust | \> 2 | Algina-Keselman-Penfield robust standardized difference average | Yes | [`WRS2::wmcpAKP()`](https://rdrr.io/pkg/WRS2/man/wmcpAKP.html) |
| Bayes Factor | \> 2 | Bayesian R-squared | Yes | [`performance::r2_bayes()`](https://easystats.github.io/performance/reference/r2_bayes.html) |

### `two_sample_test()`

#### between-subjects

**Hypothesis testing**

| Type | No. of groups | Test | Function used |
|:---|:---|:---|:---|
| Parametric | 2 | Student’s or Welch’s *t*-test | [`stats::t.test()`](https://rdrr.io/r/stats/t.test.html) |
| Non-parametric | 2 | Mann-Whitney *U* test | [`stats::wilcox.test()`](https://rdrr.io/r/stats/wilcox.test.html) |
| Robust | 2 | Yuen’s test for trimmed means | [`WRS2::yuen()`](https://rdrr.io/pkg/WRS2/man/yuen.html) |
| Bayesian | 2 | Student’s *t*-test | [`BayesFactor::ttestBF()`](https://rdrr.io/pkg/BayesFactor/man/ttestBF.html) |

**Effect size estimation**

| Type | No. of groups | Effect size | CI available? | Function used |
|:---|:---|:---|:---|:---|
| Parametric | 2 | Cohen’s *d*, Hedge’s *g* | Yes | [`effectsize::cohens_d()`](https://easystats.github.io/effectsize/reference/cohens_d.html), [`effectsize::hedges_g()`](https://easystats.github.io/effectsize/reference/cohens_d.html) |
| Non-parametric | 2 | *r* (rank-biserial correlation) | Yes | [`effectsize::rank_biserial()`](https://easystats.github.io/effectsize/reference/rank_biserial.html) |
| Robust | 2 | Algina-Keselman-Penfield robust standardized difference | Yes | [`WRS2::akp.effect()`](https://rdrr.io/pkg/WRS2/man/yuen.html) |
| Bayesian | 2 | difference | Yes | [`bayestestR::describe_posterior()`](https://easystats.github.io/bayestestR/reference/describe_posterior.html) |

#### within-subjects

**Hypothesis testing**

| Type | No. of groups | Test | Function used |
|:---|:---|:---|:---|
| Parametric | 2 | Student’s *t*-test | [`stats::t.test()`](https://rdrr.io/r/stats/t.test.html) |
| Non-parametric | 2 | Wilcoxon signed-rank test | [`stats::wilcox.test()`](https://rdrr.io/r/stats/wilcox.test.html) |
| Robust | 2 | Yuen’s test on trimmed means for dependent samples | [`WRS2::yuend()`](https://rdrr.io/pkg/WRS2/man/yuend.html) |
| Bayesian | 2 | Student’s *t*-test | [`BayesFactor::ttestBF()`](https://rdrr.io/pkg/BayesFactor/man/ttestBF.html) |

**Effect size estimation**

| Type | No. of groups | Effect size | CI available? | Function used |
|:---|:---|:---|:---|:---|
| Parametric | 2 | Cohen’s *d*, Hedge’s *g* | Yes | [`effectsize::cohens_d()`](https://easystats.github.io/effectsize/reference/cohens_d.html), [`effectsize::hedges_g()`](https://easystats.github.io/effectsize/reference/cohens_d.html) |
| Non-parametric | 2 | *r* (rank-biserial correlation) | Yes | [`effectsize::rank_biserial()`](https://easystats.github.io/effectsize/reference/rank_biserial.html) |
| Robust | 2 | Algina-Keselman-Penfield robust standardized difference | Yes | [`WRS2::wmcpAKP()`](https://rdrr.io/pkg/WRS2/man/wmcpAKP.html) |
| Bayesian | 2 | difference | Yes | [`bayestestR::describe_posterior()`](https://easystats.github.io/bayestestR/reference/describe_posterior.html) |

### `one_sample_test()`

**Hypothesis testing**

| Type | Test | Function used |
|:---|:---|:---|
| Parametric | One-sample Student’s *t*-test | [`stats::t.test()`](https://rdrr.io/r/stats/t.test.html) |
| Non-parametric | One-sample Wilcoxon test | [`stats::wilcox.test()`](https://rdrr.io/r/stats/wilcox.test.html) |
| Robust | Bootstrap-*t* method for one-sample test | [`WRS2::trimcibt()`](https://rdrr.io/pkg/WRS2/man/trimcibt.html) |
| Bayesian | One-sample Student’s *t*-test | [`BayesFactor::ttestBF()`](https://rdrr.io/pkg/BayesFactor/man/ttestBF.html) |

**Effect size estimation**

| Type | Effect size | CI available? | Function used |
|:---|:---|:---|:---|
| Parametric | Cohen’s *d*, Hedge’s *g* | Yes | [`effectsize::cohens_d()`](https://easystats.github.io/effectsize/reference/cohens_d.html), [`effectsize::hedges_g()`](https://easystats.github.io/effectsize/reference/cohens_d.html) |
| Non-parametric | *r* (rank-biserial correlation) | Yes | [`effectsize::rank_biserial()`](https://easystats.github.io/effectsize/reference/rank_biserial.html) |
| Robust | trimmed mean | Yes | [`WRS2::trimcibt()`](https://rdrr.io/pkg/WRS2/man/trimcibt.html) |
| Bayes Factor | difference | Yes | [`bayestestR::describe_posterior()`](https://easystats.github.io/bayestestR/reference/describe_posterior.html) |

### `corr_test()`

**Hypothesis testing** and **Effect size estimation**

| Type | Test | CI available? | Function used |
|:---|:---|:---|:---|
| Parametric | Pearson’s correlation coefficient | Yes | [`correlation::correlation()`](https://easystats.github.io/correlation/reference/correlation.html) |
| Non-parametric | Spearman’s rank correlation coefficient | Yes | [`correlation::correlation()`](https://easystats.github.io/correlation/reference/correlation.html) |
| Robust | Winsorized Pearson’s correlation coefficient | Yes | [`correlation::correlation()`](https://easystats.github.io/correlation/reference/correlation.html) |
| Bayesian | Bayesian Pearson’s correlation coefficient | Yes | [`correlation::correlation()`](https://easystats.github.io/correlation/reference/correlation.html) |

### `contingency_table()`

#### two-way table

**Hypothesis testing**

| Type | Design | Test | Function used |
|:---|:---|:---|:---|
| Parametric/Non-parametric | Unpaired | Pearson’s chi-squared test | [`stats::chisq.test()`](https://rdrr.io/r/stats/chisq.test.html) |
| Bayesian | Unpaired | Bayesian Pearson’s chi-squared test | [`BayesFactor::contingencyTableBF()`](https://rdrr.io/pkg/BayesFactor/man/contingencyTableBF.html) |
| Parametric/Non-parametric | Paired | McNemar’s chi-squared test | [`stats::mcnemar.test()`](https://rdrr.io/r/stats/mcnemar.test.html) |
| Bayesian | Paired | No | No |

**Effect size estimation**

| Type | Design | Effect size | CI available? | Function used |
|:---|:---|:---|:---|:---|
| Parametric/Non-parametric | Unpaired | Cramer’s *V* | Yes | [`effectsize::cramers_v()`](https://easystats.github.io/effectsize/reference/phi.html) |
| Bayesian | Unpaired | Cramer’s *V* | Yes | [`effectsize::cramers_v()`](https://easystats.github.io/effectsize/reference/phi.html) |
| Parametric/Non-parametric | Paired | Cohen’s *g* | Yes | [`effectsize::cohens_g()`](https://easystats.github.io/effectsize/reference/cohens_g.html) |
| Bayesian | Paired | No | No | No |

#### one-way table

**Hypothesis testing**

| Type | Test | Function used |
|:---|:---|:---|
| Parametric/Non-parametric | Goodness of fit chi-squared test | [`stats::chisq.test()`](https://rdrr.io/r/stats/chisq.test.html) |
| Bayesian | Bayesian Goodness of fit chi-squared test | (custom) |

**Effect size estimation**

| Type | Effect size | CI available? | Function used |
|:---|:---|:---|:---|
| Parametric/Non-parametric | Pearson’s *C* | Yes | [`effectsize::pearsons_c()`](https://easystats.github.io/effectsize/reference/phi.html) |
| Bayesian | No | No | No |

### `meta_analysis()`

**Hypothesis testing** and **Effect size estimation**

| Type | Test | Effect size | CI available? | Function used |
|:---|:---|:---|:---|:---|
| Parametric | Meta-analysis via random-effects models | *beta* | Yes | [`metafor::metafor()`](https://wviechtb.github.io/metafor/reference/metafor-package.html) |
| Robust | Meta-analysis via robust random-effects models | *beta* | Yes | [`metaplus::metaplus()`](https://rdrr.io/pkg/metaplus/man/metaplus.html) |
| Bayes | Meta-analysis via Bayesian random-effects models | *beta* | Yes | [`metaBMA::meta_random()`](https://danheck.github.io/metaBMA/reference/meta_random.html) |

## Effect size interpretation

See [effectsize](https://easystats.github.io/effectsize/)’s
interpretation functions to check different rules/conventions to
interpret effect sizes:

<https://easystats.github.io/effectsize/reference/index.html#section-interpretation>

## References

- For parametric and non-parametric effect sizes:
  <https://easystats.github.io/effectsize/articles/>

- For robust effect sizes:
  <https://CRAN.R-project.org/package=WRS2/vignettes/WRS2.pdf>

- For Bayesian posterior estimates:
  <https://easystats.github.io/bayestestR/articles/bayes_factors.html>

## Suggestions

If you find any bugs or have any suggestions/remarks, please file an
issue on GitHub:
<https://github.com/IndrajeetPatil/statsExpressions/issues>
