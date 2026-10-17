# Test and effect size details

## Introduction

This article describes the data that
[statsExpressions](https://www.indrapatil.com/statsExpressions/)
functions expect, and summarizes, for each function, the statistical
tests carried out, the effect sizes returned, and the underlying
functions used to compute them.

Abbreviations used: CI = Confidence Interval

## Data requirements

The one-sample, group-comparison, correlation, and centrality functions
expect **long (tidy) data** — one row per observation. Contingency-table
functions also accept a `counts` column for aggregated frequencies;
[`meta_analysis()`](https://www.indrapatil.com/statsExpressions/reference/meta_analysis.md)
instead requires `estimate` and `std.error` columns, with one row per
study. A few additional requirements are worth noting:

- **Within-subjects (repeated measures) designs**: The data must contain
  exactly *one* observation per subject per condition (a complete,
  balanced block design). If you have multiple trials per
  subject-condition cell, aggregate them first (e.g., by taking the
  mean) before passing the data. You can verify this with
  `table(data$subject, data$condition)` — every cell should equal `1`.

- **`subject.id` argument**: For within-subjects designs, always specify
  `subject.id` explicitly. If omitted, the function pairs observations
  by row order within each condition, so any data that is not already
  sorted identically within every condition level can produce silently
  incorrect paired tests — even with exactly two conditions and no
  missing values.

- **Missing data**: Missing values are removed internally. For
  within-subjects designs, any subject who has `NA` in *any* condition,
  or lacks a row in a condition, is removed entirely by
  [`complete_subjects()`](https://www.indrapatil.com/statsExpressions/reference/complete_subjects.md),
  ensuring a balanced design is maintained. For between-subjects
  designs, only the rows with `NA` are removed, unless `subject.id` is
  also supplied, in which case all rows of a subject with an `NA` are
  removed.

## Summary of functionality

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

## Summary of tests and effect sizes

To learn how an argument is resolved, or to browse the source code,
consult the underlying function listed in these tables. For example, for
the between-subjects one-way ANOVA, run
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

#### Between-subjects

**Hypothesis testing**

| Type | No. of groups | Test | Function used |
|:---|:---|:---|:---|
| Parametric | \> 2 | Fisher’s or Welch’s one-way ANOVA | [`stats::oneway.test()`](https://rdrr.io/r/stats/oneway.test.html) |
| Non-parametric | \> 2 | Kruskal-Wallis one-way ANOVA | [`stats::kruskal.test()`](https://rdrr.io/r/stats/kruskal.test.html) |
| Robust | \> 2 | Heteroscedastic one-way ANOVA for trimmed means | [`WRS2::t1way()`](https://rdrr.io/pkg/WRS2/man/t1way.html) |
| Bayesian | \> 2 | Fisher’s ANOVA | [`BayesFactor::anovaBF()`](https://rdrr.io/pkg/BayesFactor/man/anovaBF.html) |

**Effect size estimation**

| Type | No. of groups | Effect size | CI available? | Function used |
|:---|:---|:---|:---|:---|
| Parametric | \> 2 | partial eta-squared, partial omega-squared | Yes | [`effectsize::omega_squared()`](https://easystats.github.io/effectsize/reference/eta_squared.html), [`effectsize::eta_squared()`](https://easystats.github.io/effectsize/reference/eta_squared.html) |
| Non-parametric | \> 2 | rank epsilon squared | Yes | [`effectsize::rank_epsilon_squared()`](https://easystats.github.io/effectsize/reference/rank_epsilon_squared.html) |
| Robust | \> 2 | Explanatory measure of effect size | Yes | [`WRS2::t1way()`](https://rdrr.io/pkg/WRS2/man/t1way.html) |
| Bayesian | \> 2 | Bayesian R-squared | Yes | [`performance::r2_bayes()`](https://easystats.github.io/performance/reference/r2_bayes.html) |

#### Within-subjects

**Paired data**: See
[`complete_subjects()`](https://www.indrapatil.com/statsExpressions/reference/complete_subjects.md)
for subject matching, duplicate observations, and removal of incomplete
subjects.

**Hypothesis testing**

| Type | No. of groups | Test | Function used |
|:---|:---|:---|:---|
| Parametric | \> 2 | One-way repeated measures ANOVA | [`afex::aov_ez()`](https://rdrr.io/pkg/afex/man/aov_car.html) |
| Non-parametric | \> 2 | Friedman rank sum test | [`stats::friedman.test()`](https://rdrr.io/r/stats/friedman.test.html) |
| Robust | \> 2 | Heteroscedastic one-way repeated measures ANOVA for trimmed means | [`WRS2::rmanova()`](https://rdrr.io/pkg/WRS2/man/rmanova.html) |
| Bayesian | \> 2 | One-way repeated measures ANOVA | [`BayesFactor::anovaBF()`](https://rdrr.io/pkg/BayesFactor/man/anovaBF.html) |

**Effect size estimation**

| Type | No. of groups | Effect size | CI available? | Function used |
|:---|:---|:---|:---|:---|
| Parametric | \> 2 | partial eta-squared, partial omega-squared | Yes | [`effectsize::omega_squared()`](https://easystats.github.io/effectsize/reference/eta_squared.html), [`effectsize::eta_squared()`](https://easystats.github.io/effectsize/reference/eta_squared.html) |
| Non-parametric | \> 2 | Kendall’s coefficient of concordance | Yes | [`effectsize::kendalls_w()`](https://easystats.github.io/effectsize/reference/rank_epsilon_squared.html) |
| Robust | \> 2 | Algina-Keselman-Penfield robust standardized difference average | Yes | [`WRS2::wmcpAKP()`](https://rdrr.io/pkg/WRS2/man/wmcpAKP.html) |
| Bayesian | \> 2 | Bayesian R-squared | Yes | [`performance::r2_bayes()`](https://easystats.github.io/performance/reference/r2_bayes.html) |

### `two_sample_test()`

#### Between-subjects

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
| Parametric | 2 | Cohen’s *d*, Hedges’ *g* | Yes | [`effectsize::cohens_d()`](https://easystats.github.io/effectsize/reference/cohens_d.html), [`effectsize::hedges_g()`](https://easystats.github.io/effectsize/reference/cohens_d.html) |
| Non-parametric | 2 | *r* (rank-biserial correlation) | Yes | [`effectsize::rank_biserial()`](https://easystats.github.io/effectsize/reference/rank_biserial.html) |
| Robust | 2 | Algina-Keselman-Penfield robust standardized difference | Yes | [`WRS2::akp.effect()`](https://rdrr.io/pkg/WRS2/man/yuen.html) |
| Bayesian | 2 | difference | Yes | [`bayestestR::describe_posterior()`](https://easystats.github.io/bayestestR/reference/describe_posterior.html) |

#### Within-subjects

**Paired data**: See
[`complete_subjects()`](https://www.indrapatil.com/statsExpressions/reference/complete_subjects.md)
for subject matching, duplicate observations, and removal of incomplete
subjects.

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
| Parametric | 2 | Cohen’s *d*, Hedges’ *g* | Yes | [`effectsize::cohens_d()`](https://easystats.github.io/effectsize/reference/cohens_d.html), [`effectsize::hedges_g()`](https://easystats.github.io/effectsize/reference/cohens_d.html) |
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
| Parametric | Cohen’s *d*, Hedges’ *g* | Yes | [`effectsize::cohens_d()`](https://easystats.github.io/effectsize/reference/cohens_d.html), [`effectsize::hedges_g()`](https://easystats.github.io/effectsize/reference/cohens_d.html) |
| Non-parametric | *r* (rank-biserial correlation) | Yes | [`effectsize::rank_biserial()`](https://easystats.github.io/effectsize/reference/rank_biserial.html) |
| Robust | trimmed mean | Yes | [`WRS2::trimcibt()`](https://rdrr.io/pkg/WRS2/man/trimcibt.html) |
| Bayesian | difference | Yes | [`bayestestR::describe_posterior()`](https://easystats.github.io/bayestestR/reference/describe_posterior.html) |

### `corr_test()`

**Hypothesis testing and effect size estimation**

| Type | Test | CI available? | Function used |
|:---|:---|:---|:---|
| Parametric | Pearson’s correlation coefficient | Yes | [`correlation::correlation()`](https://easystats.github.io/correlation/reference/correlation.html) |
| Non-parametric | Spearman’s rank correlation coefficient | Yes | [`correlation::correlation()`](https://easystats.github.io/correlation/reference/correlation.html) |
| Robust | Winsorized Pearson’s correlation coefficient | Yes | [`correlation::correlation()`](https://easystats.github.io/correlation/reference/correlation.html) |
| Bayesian | Bayesian Pearson’s correlation coefficient | Yes | [`correlation::correlation()`](https://easystats.github.io/correlation/reference/correlation.html) |

### `contingency_table()`

There are no dedicated non-parametric or robust contingency table
analyses. `type = "nonparametric"` and `type = "robust"` are accepted,
but run the same frequentist tests as `type = "parametric"` (the
“Parametric/Non-parametric” rows below).

#### Two-way table

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

Paired Bayesian analysis is not supported: for a two-way table with
`type = "bayes"`, the `paired` argument is ignored and the unpaired
Bayesian test is run instead.

#### One-way table

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

### `pairwise_comparisons()`

#### Between-subjects

**Hypothesis testing**

| Type | Equal variance? | Test | *p*-value adjustment? | Function used |
|:---|:---|:---|:---|:---|
| Parametric | No | Games-Howell test | Yes | [`PMCMRplus::gamesHowellTest()`](https://rdrr.io/pkg/PMCMRplus/man/gamesHowellTest.html) |
| Parametric | Yes | Student’s *t*-test | Yes | [`stats::pairwise.t.test()`](https://rdrr.io/r/stats/pairwise.t.test.html) |
| Non-parametric | No | Dunn test | Yes | [`PMCMRplus::kwAllPairsDunnTest()`](https://rdrr.io/pkg/PMCMRplus/man/kwAllPairsDunnTest.html) |
| Robust | No | Yuen’s trimmed means test | Yes | [`WRS2::lincon()`](https://rdrr.io/pkg/WRS2/man/t1way.html) |
| Bayesian | `NA` | Student’s *t*-test | `NA` | [`BayesFactor::ttestBF()`](https://rdrr.io/pkg/BayesFactor/man/ttestBF.html) |

**Effect size estimation**

Not supported.

#### Within-subjects

**Paired data**: See
[`complete_subjects()`](https://www.indrapatil.com/statsExpressions/reference/complete_subjects.md)
for subject matching, duplicate observations, and removal of incomplete
subjects.

**Hypothesis testing**

| Type | Test | *p*-value adjustment? | Function used |
|:---|:---|:---|:---|
| Parametric | Student’s *t*-test | Yes | [`stats::pairwise.t.test()`](https://rdrr.io/r/stats/pairwise.t.test.html) |
| Non-parametric | Durbin-Conover test | Yes | [`PMCMRplus::durbinAllPairsTest()`](https://rdrr.io/pkg/PMCMRplus/man/durbinAllPairsTest.html) |
| Robust | Yuen’s trimmed means test | Yes | [`WRS2::rmmcp()`](https://rdrr.io/pkg/WRS2/man/rmanova.html) |
| Bayesian | Student’s *t*-test | `NA` | [`BayesFactor::ttestBF()`](https://rdrr.io/pkg/BayesFactor/man/ttestBF.html) |

**Effect size estimation**

Not supported.

### `pairwise_contingency_table()`

**Hypothesis testing**

| Test | *p*-value adjustment? | Function used |
|:---|:---|:---|
| Fisher’s exact test | Yes | [`stats::fisher.test()`](https://rdrr.io/r/stats/fisher.test.html) |

**Effect size estimation**

| Effect size | CI available? | Function used |
|:---|:---|:---|
| Cramer’s *V* | Yes | [`effectsize::cramers_v()`](https://easystats.github.io/effectsize/reference/phi.html) |

### `meta_analysis()`

**Hypothesis testing and effect size estimation**

| Type | Test | Effect size | CI available? | Function used |
|:---|:---|:---|:---|:---|
| Parametric | Meta-analysis via random-effects models | *beta* | Yes | [`metafor::rma()`](https://wviechtb.github.io/metafor/reference/rma.uni.html) |
| Robust | Meta-analysis via robust random-effects models | *beta* | Yes | [`metaplus::metaplus()`](https://rdrr.io/pkg/metaplus/man/metaplus.html) |
| Bayesian | Meta-analysis via Bayesian random-effects models | *beta* | Yes | [`metaBMA::meta_random()`](https://danheck.github.io/metaBMA/reference/meta_random.html) |

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
