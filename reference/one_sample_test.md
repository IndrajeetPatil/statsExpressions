# One-sample tests

Parametric, non-parametric, robust, and Bayesian one-sample tests.

## Usage

``` r
one_sample_test(
  data,
  x,
  type = "parametric",
  test.value = 0,
  alternative = "two.sided",
  digits = 2L,
  conf.level = 0.95,
  tr = 0.2,
  bf.prior = 0.707,
  effsize.type = "g",
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

  A numeric variable from the data frame `data`.

- type:

  A character specifying the type of statistical approach:

  - `"parametric"`

  - `"nonparametric"`

  - `"robust"`

  - `"bayes"`

  You can specify just the initial letter.

- test.value:

  A number indicating the true value of the mean (Default: `0`).

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

- tr:

  Trim level for the mean when carrying out `robust` tests. In case of
  an error, try reducing the value of `tr`, which is by default set to
  `0.2`. Lowering the value might help.

- bf.prior:

  A number between `0.5` and `2` (default `0.707`), the prior width to
  use in calculating Bayes factors and posterior estimates. In addition
  to numeric arguments, several named values are also recognized:
  `"medium"`, `"wide"`, and `"ultrawide"`, corresponding to *r* scale
  values of `1/2`, `sqrt(2)/2`, and `1`, respectively. In case of an
  ANOVA, this value corresponds to scale for fixed effects.

- effsize.type:

  Type of effect size needed for *parametric* tests. The argument can be
  `"d"` (for Cohen's *d*) or `"g"` (for Hedge's *g*).

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

## One-sample tests

The table below provides summary about:

- statistical test carried out for inferential statistics

- type of effect size estimate and a measure of uncertainty for this
  estimate

- functions used internally to compute these details

**Hypothesis testing**

|  |  |  |
|----|----|----|
| Type | Test | Function used |
| Parametric | One-sample Student's *t*-test | [`stats::t.test()`](https://rdrr.io/r/stats/t.test.html) |
| Non-parametric | One-sample Wilcoxon test | [`stats::wilcox.test()`](https://rdrr.io/r/stats/wilcox.test.html) |
| Robust | Bootstrap-*t* method for one-sample test | [`WRS2::trimcibt()`](https://rdrr.io/pkg/WRS2/man/trimcibt.html) |
| Bayesian | One-sample Student's *t*-test | [`BayesFactor::ttestBF()`](https://rdrr.io/pkg/BayesFactor/man/ttestBF.html) |

**Effect size estimation**

|  |  |  |  |
|----|----|----|----|
| Type | Effect size | CI available? | Function used |
| Parametric | Cohen's *d*, Hedge's *g* | Yes | [`effectsize::cohens_d()`](https://easystats.github.io/effectsize/reference/cohens_d.html), [`effectsize::hedges_g()`](https://easystats.github.io/effectsize/reference/cohens_d.html) |
| Non-parametric | *r* (rank-biserial correlation) | Yes | [`effectsize::rank_biserial()`](https://easystats.github.io/effectsize/reference/rank_biserial.html) |
| Robust | trimmed mean | Yes | [`WRS2::trimcibt()`](https://rdrr.io/pkg/WRS2/man/trimcibt.html) |
| Bayes Factor | difference | Yes | [`bayestestR::describe_posterior()`](https://easystats.github.io/bayestestR/reference/describe_posterior.html) |

## Citation

Patil, I., (2021). statsExpressions: R Package for Tidy Dataframes and
Expressions with Statistical Details. Journal of Open Source Software,
6(61), 3236, https://doi.org/10.21105/joss.03236

## Examples

``` r
# for reproducibility
set.seed(123)

# ----------------------- parametric -----------------------

one_sample_test(mtcars, wt, test.value = 3)
#> # A tibble: 1 × 15
#>      mu statistic df.error p.value method            alternative effectsize
#>   <dbl>     <dbl>    <dbl>   <dbl> <chr>             <chr>       <chr>     
#> 1     3      1.26       31   0.218 One Sample t-test two.sided   Hedges' g 
#>   estimate conf.level conf.low conf.high conf.method conf.distribution n.obs
#>      <dbl>      <dbl>    <dbl>     <dbl> <chr>       <chr>             <int>
#> 1    0.217       0.95   -0.127     0.557 ncp         t                    32
#>   expression
#>   <list>    
#> 1 <language>

# ----------------------- non-parametric -------------------

one_sample_test(mtcars, wt, test.value = 3, type = "nonparametric")
#> # A tibble: 1 × 12
#>   statistic p.value method                    alternative effectsize       
#>       <dbl>   <dbl> <chr>                     <chr>       <chr>            
#> 1       319   0.308 Wilcoxon signed rank test two.sided   r (rank biserial)
#>   estimate conf.level conf.low conf.high conf.method n.obs expression
#>      <dbl>      <dbl>    <dbl>     <dbl> <chr>       <int> <list>    
#> 1    0.208       0.95   -0.184     0.543 normal         32 <language>

# ----------------------- robust ---------------------------

one_sample_test(mtcars, wt, test.value = 3, type = "robust")
#> # A tibble: 1 × 10
#>   statistic p.value n.obs method                                 effectsize  
#>       <dbl>   <dbl> <int> <chr>                                  <chr>       
#> 1      1.18   0.275    32 Bootstrap-t method for one-sample test Trimmed mean
#>   estimate conf.level conf.low conf.high expression
#>      <dbl>      <dbl>    <dbl>     <dbl> <list>    
#> 1     3.20       0.95     2.85      3.54 <language>

# ----------------------- Bayesian -------------------------

one_sample_test(mtcars, wt, test.value = 3, type = "bayes")
#> # A tibble: 1 × 16
#>   term       effectsize      estimate conf.level conf.low conf.high    pd
#>   <chr>      <chr>              <dbl>      <dbl>    <dbl>     <dbl> <dbl>
#> 1 Difference Bayesian t-test    0.195       0.95   -0.165     0.555  0.86
#>   prior.distribution prior.location prior.scale  bf10 method         
#>   <chr>                       <dbl>       <dbl> <dbl> <chr>          
#> 1 cauchy                          0       0.707 0.387 Bayesian t-test
#>   conf.method log_e_bf10 n.obs expression
#>   <chr>            <dbl> <int> <list>    
#> 1 ETI             -0.950    32 <language>
```
