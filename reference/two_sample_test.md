# Two-sample tests

Parametric, non-parametric, robust, and Bayesian two-sample tests.

## Usage

``` r
two_sample_test(
  data,
  x,
  y,
  subject.id = NULL,
  type = "parametric",
  paired = FALSE,
  alternative = "two.sided",
  digits = 2L,
  conf.level = 0.95,
  effsize.type = "g",
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
  taken. Other data types (e.g., matrix,table, array, etc.) will **not**
  be accepted. Additionally, grouped data frames from `{dplyr}` should
  be ungrouped before they are entered as `data`.

- x:

  The grouping (or independent) variable from `data`. In case of a
  repeated measures or within-subjects design, if `subject.id` argument
  is not available or not explicitly specified, the function assumes
  that the data has already been sorted by such an id by the user and
  creates an internal identifier. So if your data is **not** sorted, the
  results *can* be inaccurate when there are more than two levels in `x`
  and there are `NA`s present. The data is expected to be sorted by user
  in subject-1, subject-2, ..., pattern.

- y:

  The response (or outcome or dependent) variable from `data`.

- subject.id:

  Relevant in case of a repeated measures or within-subjects design
  (`paired = TRUE`, i.e.), it specifies the subject or repeated measures
  identifier. **Important**: Note that if this argument is `NULL` (which
  is the default), the function assumes that the data has already been
  sorted by such an id by the user and creates an internal identifier.
  So if your data is **not** sorted and you leave this argument
  unspecified, the results *can* be inaccurate when there are more than
  two levels in `x` and there are `NA`s present.

- type:

  A character specifying the type of statistical approach:

  - `"parametric"`

  - `"nonparametric"`

  - `"robust"`

  - `"bayes"`

  You can specify just the initial letter.

- paired:

  Logical that decides whether the experimental design is repeated
  measures/within-subjects or between-subjects. The default is `FALSE`.

- alternative:

  a character string specifying the alternative hypothesis, must be one
  of `"two.sided"` (default), `"greater"` or `"less"`. You can specify
  just the initial letter.

- digits:

  Number of digits for rounding or significant figures. May also be
  `"signif"` to return significant figures or `"scientific"` to return
  scientific notation. Control the number of digits by adding the value
  as suffix, e.g. `digits = "scientific4"` to have scientific notation
  with 4 decimal places, or `digits = "signif5"` for 5 significant
  figures (see also [`signif()`](https://rdrr.io/r/base/Round.html)).

- conf.level:

  Scalar between `0` and `1` (default: `95%` confidence/credible
  intervals, `0.95`). If `NULL`, no confidence intervals will be
  computed.

- effsize.type:

  Type of effect size needed for *parametric* tests. The argument can be
  `"d"` (for Cohen's *d*) or `"g"` (for Hedge's *g*).

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

  Currently ignored.

## Value

The returned tibble data frame can contain some or all of the following
columns (the exact columns will depend on the statistical test):

- `statistic`: the numeric value of a statistic

- `df`: the numeric value of a parameter being modeled (often degrees of
  freedom for the test)

- `df.error` and `df`: relevant only if the statistic in question has
  two degrees of freedom (e.g. anova)

- `p.value`: the two-sided *p*-value associated with the observed
  statistic

- `method`: the name of the inferential statistical test

- `estimate`: estimated value of the effect size

- `conf.low`: lower bound for the effect size estimate

- `conf.high`: upper bound for the effect size estimate

- `conf.level`: width of the confidence interval

- `conf.method`: method used to compute confidence interval

- `conf.distribution`: statistical distribution for the effect

- `effectsize`: the name of the effect size

- `n.obs`: number of observations

- `expression`: pre-formatted expression containing statistical details

For examples, see [data frame output
vignette](https://indrajeetpatil.github.io/statsExpressions/articles/web_only/dataframe_outputs.html).

## Two-sample tests

The table below provides summary about:

- statistical test carried out for inferential statistics

- type of effect size estimate and a measure of uncertainty for this
  estimate

- functions used internally to compute these details

### between-subjects

**Hypothesis testing**

|  |  |  |  |
|----|----|----|----|
| Type | No. of groups | Test | Function used |
| Parametric | 2 | Student's or Welch's *t*-test | [`stats::t.test()`](https://rdrr.io/r/stats/t.test.html) |
| Non-parametric | 2 | Mann-Whitney *U* test | [`stats::wilcox.test()`](https://rdrr.io/r/stats/wilcox.test.html) |
| Robust | 2 | Yuen's test for trimmed means | [`WRS2::yuen()`](https://rdrr.io/pkg/WRS2/man/yuen.html) |
| Bayesian | 2 | Student's *t*-test | [`BayesFactor::ttestBF()`](https://rdrr.io/pkg/BayesFactor/man/ttestBF.html) |

**Effect size estimation**

|  |  |  |  |  |
|----|----|----|----|----|
| Type | No. of groups | Effect size | CI available? | Function used |
| Parametric | 2 | Cohen's *d*, Hedge's *g* | Yes | [`effectsize::cohens_d()`](https://easystats.github.io/effectsize/reference/cohens_d.html), [`effectsize::hedges_g()`](https://easystats.github.io/effectsize/reference/cohens_d.html) |
| Non-parametric | 2 | *r* (rank-biserial correlation) | Yes | [`effectsize::rank_biserial()`](https://easystats.github.io/effectsize/reference/rank_biserial.html) |
| Robust | 2 | Algina-Keselman-Penfield robust standardized difference | Yes | [`WRS2::akp.effect()`](https://rdrr.io/pkg/WRS2/man/yuen.html) |
| Bayesian | 2 | difference | Yes | [`bayestestR::describe_posterior()`](https://easystats.github.io/bayestestR/reference/describe_posterior.html) |

### within-subjects

**Hypothesis testing**

|  |  |  |  |
|----|----|----|----|
| Type | No. of groups | Test | Function used |
| Parametric | 2 | Student's *t*-test | [`stats::t.test()`](https://rdrr.io/r/stats/t.test.html) |
| Non-parametric | 2 | Wilcoxon signed-rank test | [`stats::wilcox.test()`](https://rdrr.io/r/stats/wilcox.test.html) |
| Robust | 2 | Yuen's test on trimmed means for dependent samples | [`WRS2::yuend()`](https://rdrr.io/pkg/WRS2/man/yuend.html) |
| Bayesian | 2 | Student's *t*-test | [`BayesFactor::ttestBF()`](https://rdrr.io/pkg/BayesFactor/man/ttestBF.html) |

**Effect size estimation**

|  |  |  |  |  |
|----|----|----|----|----|
| Type | No. of groups | Effect size | CI available? | Function used |
| Parametric | 2 | Cohen's *d*, Hedge's *g* | Yes | [`effectsize::cohens_d()`](https://easystats.github.io/effectsize/reference/cohens_d.html), [`effectsize::hedges_g()`](https://easystats.github.io/effectsize/reference/cohens_d.html) |
| Non-parametric | 2 | *r* (rank-biserial correlation) | Yes | [`effectsize::rank_biserial()`](https://easystats.github.io/effectsize/reference/rank_biserial.html) |
| Robust | 2 | Algina-Keselman-Penfield robust standardized difference | Yes | [`WRS2::wmcpAKP()`](https://rdrr.io/pkg/WRS2/man/wmcpAKP.html) |
| Bayesian | 2 | difference | Yes | [`bayestestR::describe_posterior()`](https://easystats.github.io/bayestestR/reference/describe_posterior.html) |

## Citation

Patil, I., (2021). statsExpressions: R Package for Tidy Dataframes and
Expressions with Statistical Details. Journal of Open Source Software,
6(61), 3236, https://doi.org/10.21105/joss.03236

## Examples

``` r
# ----------------------- within-subjects -------------------------------------

# data
df <- dplyr::filter(bugs_long, condition %in% c("LDLF", "LDHF"))

# for reproducibility
set.seed(123)

# ----------------------- parametric ---------------------------------------

two_sample_test(df, condition, desire, subject.id = subject, paired = TRUE, type = "parametric")
#> # A tibble: 1 × 16
#>   term   group     statistic df.error       p.value method        alternative
#>   <chr>  <chr>         <dbl>    <dbl>         <dbl> <chr>         <chr>      
#> 1 desire condition      6.65       90 0.00000000222 Paired t-test two.sided  
#>   effectsize estimate conf.level conf.low conf.high conf.method
#>   <chr>         <dbl>      <dbl>    <dbl>     <dbl> <chr>      
#> 1 Hedges' g     0.691       0.95    0.462     0.917 ncp        
#>   conf.distribution n.obs expression
#>   <chr>             <int> <list>    
#> 1 t                    91 <language>

# ----------------------- non-parametric -----------------------------------

two_sample_test(df, condition, desire, subject.id = subject, paired = TRUE, type = "nonparametric")
#> # A tibble: 1 × 14
#>   parameter1 parameter2 statistic      p.value method                   
#>   <chr>      <chr>          <dbl>        <dbl> <chr>                    
#> 1 desire     condition      2250. 0.0000000241 Wilcoxon signed rank test
#>   alternative effectsize        estimate conf.level conf.low conf.high
#>   <chr>       <chr>                <dbl>      <dbl>    <dbl>     <dbl>
#> 1 two.sided   r (rank biserial)    0.761       0.95    0.642     0.844
#>   conf.method n.obs expression
#>   <chr>       <int> <list>    
#> 1 normal         91 <language>

# ----------------------- robust --------------------------------------------

two_sample_test(df, condition, desire, subject.id = subject, paired = TRUE, type = "robust")
#> # A tibble: 1 × 15
#>   statistic df.error      p.value
#>       <dbl>    <dbl>        <dbl>
#> 1      6.46       54 0.0000000313
#>   method                                            
#>   <chr>                                             
#> 1 Yuen's test on trimmed means for dependent samples
#>   effectsize                                              estimate conf.level
#>   <chr>                                                      <dbl>      <dbl>
#> 1 Algina-Keselman-Penfield robust standardized difference    0.533       0.95
#>   conf.low conf.high    mu small medium large n.obs expression
#>      <dbl>     <dbl> <dbl> <dbl>  <dbl> <dbl> <int> <list>    
#> 1    0.369     0.707     0   0.1    0.3   0.5    91 <language>

# ----------------------- Bayesian ---------------------------------------

two_sample_test(df, condition, desire, subject.id = subject, paired = TRUE, type = "bayes")
#> # A tibble: 1 × 16
#>   term       effectsize      estimate conf.level conf.low conf.high    pd
#>   <chr>      <chr>              <dbl>      <dbl>    <dbl>     <dbl> <dbl>
#> 1 Difference Bayesian t-test     1.63       0.95     1.13      2.11     1
#>   prior.distribution prior.location prior.scale     bf10 method         
#>   <chr>                       <dbl>       <dbl>    <dbl> <chr>          
#> 1 cauchy                          0       0.707 4762370. Bayesian t-test
#>   conf.method log_e_bf10 n.obs expression
#>   <chr>            <dbl> <int> <list>    
#> 1 ETI               15.4    91 <language>
# ----------------------- between-subjects -------------------------------------

# for reproducibility
set.seed(123)

# ----------------------- parametric ---------------------------------------

# unequal variance
two_sample_test(ToothGrowth, supp, len, type = "parametric")
#> # A tibble: 1 × 18
#>   parameter1 parameter2 mean.parameter1 mean.parameter2 statistic df.error
#>   <chr>      <chr>                <dbl>           <dbl>     <dbl>    <dbl>
#> 1 len        supp                  20.7            17.0      1.92     55.3
#>   p.value method                  alternative effectsize estimate conf.level
#>     <dbl> <chr>                   <chr>       <chr>         <dbl>      <dbl>
#> 1  0.0606 Welch Two Sample t-test two.sided   Hedges' g     0.488       0.95
#>   conf.low conf.high conf.method conf.distribution n.obs expression
#>      <dbl>     <dbl> <chr>       <chr>             <int> <list>    
#> 1  -0.0217     0.993 ncp         t                    60 <language>

# equal variance
two_sample_test(ToothGrowth, supp, len, type = "parametric", var.equal = TRUE)
#> # A tibble: 1 × 18
#>   parameter1 parameter2 mean.parameter1 mean.parameter2 statistic df.error
#>   <chr>      <chr>                <dbl>           <dbl>     <dbl>    <dbl>
#> 1 len        supp                  20.7            17.0      1.92       58
#>   p.value method            alternative effectsize estimate conf.level conf.low
#>     <dbl> <chr>             <chr>       <chr>         <dbl>      <dbl>    <dbl>
#> 1  0.0604 Two Sample t-test two.sided   Hedges' g     0.488       0.95  -0.0217
#>   conf.high conf.method conf.distribution n.obs expression
#>       <dbl> <chr>       <chr>             <int> <list>    
#> 1     0.993 ncp         t                    60 <language>

# ----------------------- non-parametric -----------------------------------

two_sample_test(ToothGrowth, supp, len, type = "nonparametric")
#> # A tibble: 1 × 14
#>   parameter1 parameter2 statistic p.value method                 alternative
#>   <chr>      <chr>          <dbl>   <dbl> <chr>                  <chr>      
#> 1 len        supp            576.  0.0645 Wilcoxon rank sum test two.sided  
#>   effectsize        estimate conf.level conf.low conf.high conf.method n.obs
#>   <chr>                <dbl>      <dbl>    <dbl>     <dbl> <chr>       <int>
#> 1 r (rank biserial)    0.279       0.95 -0.00812     0.523 normal         60
#>   expression
#>   <list>    
#> 1 <language>

# ----------------------- robust --------------------------------------------

two_sample_test(ToothGrowth, supp, len, type = "robust")
#> # A tibble: 1 × 11
#>   statistic df.error p.value
#>       <dbl>    <dbl>   <dbl>
#> 1      2.29     33.5  0.0286
#>   method                                              
#>   <chr>                                               
#> 1 Yuen's test on trimmed means for independent samples
#>   effectsize                                              estimate conf.level
#>   <chr>                                                      <dbl>      <dbl>
#> 1 Algina-Keselman-Penfield robust standardized difference    0.683       0.95
#>   conf.low conf.high n.obs expression
#>      <dbl>     <dbl> <int> <list>    
#> 1 -0.00736      2.36    60 <language>

# ----------------------- Bayesian ---------------------------------------

two_sample_test(ToothGrowth, supp, len, type = "bayes")
#> # A tibble: 1 × 16
#>   term       effectsize      estimate conf.level conf.low conf.high    pd
#>   <chr>      <chr>              <dbl>      <dbl>    <dbl>     <dbl> <dbl>
#> 1 Difference Bayesian t-test     3.16       0.95   -0.338      6.78 0.961
#>   prior.distribution prior.location prior.scale  bf10 method         
#>   <chr>                       <dbl>       <dbl> <dbl> <chr>          
#> 1 cauchy                          0       0.707  1.20 Bayesian t-test
#>   conf.method log_e_bf10 n.obs expression
#>   <chr>            <dbl> <int> <list>    
#> 1 ETI              0.181    60 <language>
```
