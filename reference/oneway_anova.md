# One-way analysis of variance (ANOVA)

Parametric, non-parametric, robust, and Bayesian one-way ANOVA.

## Usage

``` r
oneway_anova(
  data,
  x,
  y,
  subject.id = NULL,
  type = "parametric",
  paired = FALSE,
  digits = 2L,
  conf.level = 0.95,
  effsize.type = "omega",
  var.equal = FALSE,
  bf.prior = 0.707,
  tr = 0.2,
  nboot = 100L,
  ...
)
```

## Arguments

- data:

  A data frame (or a tibble) from which variables specified are to be
  taken. Other data types (e.g., matrix, table, array, etc.) will
  **not** be accepted. Additionally, grouped data frames from `{dplyr}`
  should be ungrouped before they are entered as `data`.

- x:

  The grouping (or independent) variable from `data`. For repeated
  measures designs, see the note on ordering in `subject.id`.

- y:

  The response (or outcome or dependent) variable from `data`.

- subject.id:

  Relevant in case of a repeated measures or within-subjects design
  (i.e., `paired = TRUE`), it specifies the subject or repeated measures
  identifier. **Important**: If this argument is `NULL` (which is the
  default), observations are paired by their row order within each level
  of `x` (i.e., the data is assumed to be sorted in a subject-1,
  subject-2, ... pattern within every level). If the data is **not**
  sorted this way, the paired results will be silently incorrect, so it
  is safest to always specify `subject.id`. For paired designs, each
  subject must have at most one row per level of `x` (an error is thrown
  otherwise), and rows with a missing `subject.id` are dropped because
  they cannot be paired (see
  [`complete_subjects()`](https://www.indrapatil.com/statsExpressions/reference/complete_subjects.md)).

- type:

  A character specifying the type of statistical approach:

  - `"parametric"`

  - `"nonparametric"`

  - `"robust"`

  - `"bayes"`

  You can specify just the initial letter (e.g. `"np"` or `"bf"`).
  Matching is on the initial lowercase letter only, so any other value
  (including capitalized values such as `"Bayes"`) falls back to
  `"parametric"` without a warning.

- paired:

  Logical that decides whether the experimental design is repeated
  measures/within-subjects or between-subjects. The default is `FALSE`.

- digits:

  Number of digits for rounding or significant figures. May also be
  `"signif"` to return significant figures or `"scientific"` to return
  scientific notation. Control the number of digits by adding the value
  as suffix, e.g. `digits = "scientific4"` to have scientific notation
  with 4 decimal places, or `digits = "signif5"` for 5 significant
  figures (see also [`signif()`](https://rdrr.io/r/base/Round.html)).

- conf.level:

  Scalar between `0` and `1` (default: `95%` confidence/credible
  intervals, `0.95`).

- effsize.type:

  Type of effect size needed for *parametric* tests. The argument can be
  `"eta"` (partial eta-squared) or `"omega"` (partial omega-squared).

- var.equal:

  a logical variable indicating whether to treat the two variances as
  being equal. If `TRUE` then the pooled variance is used to estimate
  the variance otherwise the Welch (or Satterthwaite) approximation to
  the degrees of freedom is used.

- bf.prior:

  A number between `0.5` and `2` (default `0.707`), the prior width to
  use in calculating Bayes factors and posterior estimates. In addition
  to numeric arguments, several named values are also recognized:
  `"medium"`, `"wide"`, and `"ultrawide"`, corresponding to *r* scale
  values of `1/2`, `sqrt(2)/2`, and `1`, respectively. In case of an
  ANOVA, this value corresponds to scale for fixed effects.

- tr:

  Trim level for the mean when carrying out `robust` tests. In case of
  an error, try reducing the value of `tr`, which is by default set to
  `0.2`. Lowering the value might help.

- nboot:

  Number of bootstrap samples for computing confidence interval for the
  effect size (Default: `100L`).

- ...:

  Additional arguments (currently ignored).

## Value

The returned object is a tibble data frame with the additional class
`"statsExpressions"`. The exact set of columns depends on the test and,
for functions that accept a `type` argument, on the chosen analysis
(parametric, non-parametric, robust, or Bayesian). Any given call
therefore returns *some* (not all) of the columns below.

**Hypothesis testing**

- `statistic`: the numeric value of a statistic

- `df` and/or `df.error`: degrees of freedom for the test. A *t*-test
  uses `df.error`, a chi-squared test uses `df`, and a parametric ANOVA
  uses both (numerator `df` and denominator `df.error`)

- `p.value`: the *p*-value associated with the observed statistic
  (two-sided unless a one-sided `alternative` is requested, where
  supported)

- `method`: the name of the inferential statistical test

**Effect size estimation**

- `effectsize`: the name of the effect size

- `estimate`: estimated value of the effect size

- `conf.level`: the coverage level of the confidence/credible interval
  (e.g. `0.95`); the interval itself spans `conf.low` to `conf.high`

- `conf.low`: lower bound for the effect size estimate

- `conf.high`: upper bound for the effect size estimate

- `conf.method`: method used to compute the confidence/credible interval

- `conf.distribution`: statistical distribution for the effect

**Bayesian analysis** (only when `type = "bayes"`)

- `bf10`: Bayes factor for the alternative hypothesis relative to the
  null

- `log_e_bf10`: natural logarithm of the Bayes factor (present for most,
  but not all, Bayesian analyses)

- `prior.distribution`, `prior.scale`, `prior.location`: prior
  specification used to compute the Bayes factor and posterior estimates

**Pairwise comparisons** (for
[`pairwise_comparisons()`](https://www.indrapatil.com/statsExpressions/reference/pairwise_comparisons.md)
and
[`pairwise_contingency_table()`](https://www.indrapatil.com/statsExpressions/reference/pairwise_contingency_table.md))

- `group1`, `group2`: the two levels being compared

- `p.adjust.method`: the adjustment method used for multiple comparisons

- `p.value.adj`: the adjusted *p*-value; returned by
  [`pairwise_contingency_table()`](https://www.indrapatil.com/statsExpressions/reference/pairwise_contingency_table.md).
  Note that
  [`pairwise_comparisons()`](https://www.indrapatil.com/statsExpressions/reference/pairwise_comparisons.md)
  instead folds the adjusted value into `p.value` (and does not return a
  separate `p.value.adj` column)

**Common columns**

- `n.obs`: sample size used for the analysis: observations for unpaired
  tests, complete subjects/pairs for paired two-sample tests and ANOVAs,
  or studies for meta-analysis

- `expression`: a list-column of pre-formatted
  [plotmath](https://rdrr.io/r/grDevices/plotmath.html) expressions;
  each element is a `language` object (not a character string)
  containing the statistical details, ready to be used in `{ggplot2}`
  (e.g. in [`labs()`](https://ggplot2.tidyverse.org/reference/labs.html)
  or
  [`annotate()`](https://ggplot2.tidyverse.org/reference/annotate.html))

For a per-function, column-by-column breakdown of the output (and an
explanation of the internal
[`add_expression_col()`](https://www.indrapatil.com/statsExpressions/reference/add_expression_col.md)
engine that builds the `expression` column), see the [Return value
schema](https://www.indrapatil.com/statsExpressions/articles/web_only/return_value_schema.html)
article. For more examples, see the [data frame output
vignette](https://www.indrapatil.com/statsExpressions/articles/web_only/dataframe_outputs.html).

## One-way ANOVA

The table below provides summary about:

- statistical test carried out for inferential statistics

- type of effect size estimate and a measure of uncertainty for this
  estimate

- functions used internally to compute these details

### Between-subjects

**Hypothesis testing**

|  |  |  |  |
|----|----|----|----|
| Type | No. of groups | Test | Function used |
| Parametric | \> 2 | Fisher's or Welch's one-way ANOVA | [`stats::oneway.test()`](https://rdrr.io/r/stats/oneway.test.html) |
| Non-parametric | \> 2 | Kruskal-Wallis one-way ANOVA | [`stats::kruskal.test()`](https://rdrr.io/r/stats/kruskal.test.html) |
| Robust | \> 2 | Heteroscedastic one-way ANOVA for trimmed means | [`WRS2::t1way()`](https://rdrr.io/pkg/WRS2/man/t1way.html) |
| Bayesian | \> 2 | Fisher's ANOVA | [`BayesFactor::anovaBF()`](https://rdrr.io/pkg/BayesFactor/man/anovaBF.html) |

**Effect size estimation**

|  |  |  |  |  |
|----|----|----|----|----|
| Type | No. of groups | Effect size | CI available? | Function used |
| Parametric | \> 2 | partial eta-squared, partial omega-squared | Yes | [`effectsize::omega_squared()`](https://easystats.github.io/effectsize/reference/eta_squared.html), [`effectsize::eta_squared()`](https://easystats.github.io/effectsize/reference/eta_squared.html) |
| Non-parametric | \> 2 | rank epsilon squared | Yes | [`effectsize::rank_epsilon_squared()`](https://easystats.github.io/effectsize/reference/rank_epsilon_squared.html) |
| Robust | \> 2 | Explanatory measure of effect size | Yes | [`WRS2::t1way()`](https://rdrr.io/pkg/WRS2/man/t1way.html) |
| Bayesian | \> 2 | Bayesian R-squared | Yes | [`performance::r2_bayes()`](https://easystats.github.io/performance/reference/r2_bayes.html) |

### Within-subjects

**Paired data**: See
[`complete_subjects()`](https://www.indrapatil.com/statsExpressions/reference/complete_subjects.md)
for subject matching, duplicate observations, and removal of incomplete
subjects.

**Hypothesis testing**

|  |  |  |  |
|----|----|----|----|
| Type | No. of groups | Test | Function used |
| Parametric | \> 2 | One-way repeated measures ANOVA | [`afex::aov_ez()`](https://rdrr.io/pkg/afex/man/aov_car.html) |
| Non-parametric | \> 2 | Friedman rank sum test | [`stats::friedman.test()`](https://rdrr.io/r/stats/friedman.test.html) |
| Robust | \> 2 | Heteroscedastic one-way repeated measures ANOVA for trimmed means | [`WRS2::rmanova()`](https://rdrr.io/pkg/WRS2/man/rmanova.html) |
| Bayesian | \> 2 | One-way repeated measures ANOVA | [`BayesFactor::anovaBF()`](https://rdrr.io/pkg/BayesFactor/man/anovaBF.html) |

**Effect size estimation**

|  |  |  |  |  |
|----|----|----|----|----|
| Type | No. of groups | Effect size | CI available? | Function used |
| Parametric | \> 2 | partial eta-squared, partial omega-squared | Yes | [`effectsize::omega_squared()`](https://easystats.github.io/effectsize/reference/eta_squared.html), [`effectsize::eta_squared()`](https://easystats.github.io/effectsize/reference/eta_squared.html) |
| Non-parametric | \> 2 | Kendall's coefficient of concordance | Yes | [`effectsize::kendalls_w()`](https://easystats.github.io/effectsize/reference/rank_epsilon_squared.html) |
| Robust | \> 2 | Algina-Keselman-Penfield robust standardized difference average | Yes | [`WRS2::wmcpAKP()`](https://rdrr.io/pkg/WRS2/man/wmcpAKP.html) |
| Bayesian | \> 2 | Bayesian R-squared | Yes | [`performance::r2_bayes()`](https://easystats.github.io/performance/reference/r2_bayes.html) |

## Citation

Patil, I., (2021). statsExpressions: R Package for Tidy Dataframes and
Expressions with Statistical Details. Journal of Open Source Software,
6(61), 3236, https://doi.org/10.21105/joss.03236

## Examples

``` r
# for reproducibility
set.seed(123)
library(statsExpressions)

# ----------------------- parametric -------------------------------------

# between-subjects
oneway_anova(
  data = mtcars,
  x = cyl,
  y = wt
)
#> # A tibble: 1 × 14
#>   statistic    df df.error   p.value
#>       <dbl> <dbl>    <dbl>     <dbl>
#> 1      20.2     2     19.0 0.0000196
#>   method                                                   effectsize estimate
#>   <chr>                                                    <chr>         <dbl>
#> 1 One-way analysis of means (not assuming equal variances) Omega2        0.637
#>   conf.level conf.low conf.high conf.method conf.distribution n.obs expression
#>        <dbl>    <dbl>     <dbl> <chr>       <chr>             <int> <list>    
#> 1       0.95    0.370         1 ncp         F                    32 <language>

# biased (partial eta-squared) effect size
oneway_anova(
  data = mtcars,
  x = cyl,
  y = wt,
  effsize.type = "eta"
)
#> # A tibble: 1 × 14
#>   statistic    df df.error   p.value
#>       <dbl> <dbl>    <dbl>     <dbl>
#> 1      20.2     2     19.0 0.0000196
#>   method                                                   effectsize estimate
#>   <chr>                                                    <chr>         <dbl>
#> 1 One-way analysis of means (not assuming equal variances) Eta2          0.681
#>   conf.level conf.low conf.high conf.method conf.distribution n.obs expression
#>        <dbl>    <dbl>     <dbl> <chr>       <chr>             <int> <list>    
#> 1       0.95    0.437         1 ncp         F                    32 <language>

# within-subjects design
oneway_anova(
  data = bugs_long,
  x = condition,
  y = desire,
  subject.id = subject,
  paired = TRUE
)
#> Registered S3 method overwritten by 'car':
#>   method           from
#>   na.action.merMod lme4
#> # A tibble: 1 × 18
#>   term      sumsq sum.squares.error    df df.error meansq statistic  p.value
#>   <chr>     <dbl>             <dbl> <dbl>    <dbl>  <dbl>     <dbl>    <dbl>
#> 1 condition  233.              984.  2.63     229.   4.30      20.6 8.27e-11
#>   method                                              effectsize       estimate
#>   <chr>                                               <chr>               <dbl>
#> 1 ANOVA estimation for factorial designs using 'afex' Omega2 (partial)   0.0783
#>   conf.level conf.low conf.high conf.method conf.distribution n.obs expression
#>        <dbl>    <dbl>     <dbl> <chr>       <chr>             <int> <list>    
#> 1       0.95   0.0280         1 ncp         F                    88 <language>

# ----------------------- non-parametric ----------------------------------

# between-subjects
oneway_anova(
  data = mtcars,
  x = cyl,
  y = wt,
  type = "np"
)
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

# within-subjects design
oneway_anova(
  data = bugs_long,
  x = condition,
  y = desire,
  subject.id = subject,
  paired = TRUE,
  type = "np"
)
#> # A tibble: 1 × 15
#>   parameter1 parameter2 statistic df.error  p.value method                
#>   <chr>      <chr>          <dbl>    <dbl>    <dbl> <chr>                 
#> 1 desire     condition       55.8        3 4.56e-12 Friedman rank sum test
#>   effectsize  estimate conf.level conf.low conf.high conf.method         
#>   <chr>          <dbl>      <dbl>    <dbl>     <dbl> <chr>               
#> 1 Kendall's W    0.211       0.95    0.154         1 percentile bootstrap
#>   conf.iterations n.obs expression
#>             <int> <int> <list>    
#> 1             100    88 <language>

# ----------------------- robust -------------------------------------

# between-subjects
oneway_anova(
  data = mtcars,
  x = cyl,
  y = wt,
  type = "r"
)
#> # A tibble: 1 × 12
#>   statistic    df df.error p.value
#>       <dbl> <dbl>    <dbl>   <dbl>
#> 1      12.7     2     12.2 0.00102
#>   method                                           
#>   <chr>                                            
#> 1 A heteroscedastic one-way ANOVA for trimmed means
#>   effectsize                         estimate conf.level conf.low conf.high
#>   <chr>                                 <dbl>      <dbl>    <dbl>     <dbl>
#> 1 Explanatory measure of effect size     1.02       0.95    0.824      1.62
#>   n.obs expression
#>   <int> <list>    
#> 1    32 <language>

# within-subjects design
oneway_anova(
  data = bugs_long,
  x = condition,
  y = desire,
  subject.id = subject,
  paired = TRUE,
  type = "r"
)
#> # A tibble: 1 × 12
#>   statistic    df df.error  p.value
#>       <dbl> <dbl>    <dbl>    <dbl>
#> 1      21.0  2.73     145. 1.15e-10
#>   method                                                             
#>   <chr>                                                              
#> 1 A heteroscedastic one-way repeated measures ANOVA for trimmed means
#>   effectsize                                                      estimate
#>   <chr>                                                              <dbl>
#> 1 Algina-Keselman-Penfield robust standardized difference average    0.325
#>   conf.level conf.low conf.high n.obs expression
#>        <dbl>    <dbl>     <dbl> <int> <list>    
#> 1       0.95    0.110     0.572    88 <language>

# ----------------------- Bayesian -------------------------------------

# between-subjects
oneway_anova(
  data = mtcars,
  x = cyl,
  y = wt,
  type = "bayes"
)
#> # A tibble: 6 × 17
#>   term     pd prior.distribution prior.location prior.scale   bf10
#>   <chr> <dbl> <chr>                       <dbl>       <dbl>  <dbl>
#> 1 mu    1     cauchy                          0       0.707 20968.
#> 2 cyl-4 1     cauchy                          0       0.707 20968.
#> 3 cyl-6 0.542 cauchy                          0       0.707 20968.
#> 4 cyl-8 1     cauchy                          0       0.707 20968.
#> 5 sig2  1     cauchy                          0       0.707 20968.
#> 6 g_cyl 1     cauchy                          0       0.707 20968.
#>   method                          log_e_bf10 effectsize         estimate std.dev
#>   <chr>                                <dbl> <chr>                 <dbl>   <dbl>
#> 1 Bayes factors for linear models       9.95 Bayesian R-squared    0.581  0.0815
#> 2 Bayes factors for linear models       9.95 Bayesian R-squared    0.581  0.0815
#> 3 Bayes factors for linear models       9.95 Bayesian R-squared    0.581  0.0815
#> 4 Bayes factors for linear models       9.95 Bayesian R-squared    0.581  0.0815
#> 5 Bayes factors for linear models       9.95 Bayesian R-squared    0.581  0.0815
#> 6 Bayes factors for linear models       9.95 Bayesian R-squared    0.581  0.0815
#>   conf.level conf.low conf.high conf.method n.obs expression
#>        <dbl>    <dbl>     <dbl> <chr>       <int> <list>    
#> 1       0.95    0.373     0.702 HDI            32 <language>
#> 2       0.95    0.373     0.702 HDI            32 <language>
#> 3       0.95    0.373     0.702 HDI            32 <language>
#> 4       0.95    0.373     0.702 HDI            32 <language>
#> 5       0.95    0.373     0.702 HDI            32 <language>
#> 6       0.95    0.373     0.702 HDI            32 <language>

# within-subjects design
oneway_anova(
  data = bugs_long,
  x = condition,
  y = desire,
  subject.id = subject,
  paired = TRUE,
  type = "bayes"
)
#> Multiple `BFBayesFactor` models detected - posteriors are extracted from
#>   the first numerator model.
#>   See help("get_parameters", package = "insight").
#> # A tibble: 8 × 19
#>   term              pd prior.distribution prior.location prior.scale effect
#>   <chr>          <dbl> <chr>                       <dbl>       <dbl> <chr> 
#> 1 mu             1     cauchy                          0       0.707 fixed 
#> 2 condition-HDHF 1     cauchy                          0       0.707 fixed 
#> 3 condition-HDLF 0.868 cauchy                          0       0.707 fixed 
#> 4 condition-LDHF 0.994 cauchy                          0       0.707 fixed 
#> 5 condition-LDLF 1     cauchy                          0       0.707 fixed 
#> 6 sig2           1     cauchy                          0       1     fixed 
#> 7 g_condition    1     cauchy                          0       1     fixed 
#> 8 g_.rowid       1     cauchy                          0       1     fixed 
#>          bf10 method                          log_e_bf10 effectsize        
#>         <dbl> <chr>                                <dbl> <chr>             
#> 1 1360982795. Bayes factors for linear models       21.0 Bayesian R-squared
#> 2 1360982795. Bayes factors for linear models       21.0 Bayesian R-squared
#> 3 1360982795. Bayes factors for linear models       21.0 Bayesian R-squared
#> 4 1360982795. Bayes factors for linear models       21.0 Bayesian R-squared
#> 5 1360982795. Bayes factors for linear models       21.0 Bayesian R-squared
#> 6 1360982795. Bayes factors for linear models       21.0 Bayesian R-squared
#> 7 1360982795. Bayes factors for linear models       21.0 Bayesian R-squared
#> 8 1360982795. Bayes factors for linear models       21.0 Bayesian R-squared
#>   estimate std.dev conf.level conf.low conf.high conf.method component   n.obs
#>      <dbl>   <dbl>      <dbl>    <dbl>     <dbl> <chr>       <chr>       <int>
#> 1    0.531  0.0323       0.95    0.461     0.587 HDI         conditional    88
#> 2    0.531  0.0323       0.95    0.461     0.587 HDI         conditional    88
#> 3    0.531  0.0323       0.95    0.461     0.587 HDI         conditional    88
#> 4    0.531  0.0323       0.95    0.461     0.587 HDI         conditional    88
#> 5    0.531  0.0323       0.95    0.461     0.587 HDI         conditional    88
#> 6    0.531  0.0323       0.95    0.461     0.587 HDI         conditional    88
#> 7    0.531  0.0323       0.95    0.461     0.587 HDI         conditional    88
#> 8    0.531  0.0323       0.95    0.461     0.587 HDI         conditional    88
#>   expression
#>   <list>    
#> 1 <language>
#> 2 <language>
#> 3 <language>
#> 4 <language>
#> 5 <language>
#> 6 <language>
#> 7 <language>
#> 8 <language>
```
