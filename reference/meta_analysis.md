# Random-effects meta-analysis

Parametric, non-parametric, robust, and Bayesian random-effects
meta-analysis.

## Usage

``` r
meta_analysis(
  data,
  type = "parametric",
  random = "mixture",
  digits = 2L,
  conf.level = 0.95,
  ...
)
```

## Arguments

- data:

  A data frame. It **must** contain columns named `estimate` (effect
  sizes or outcomes) and `std.error` (corresponding standard errors).
  These two columns will be used:

  - as `yi` and `sei` arguments in
    [`metafor::rma()`](https://wviechtb.github.io/metafor/reference/rma.uni.html)
    (for **parametric** test)

  - as `yi` and `sei` arguments in
    [`metaplus::metaplus()`](https://rdrr.io/pkg/metaplus/man/metaplus.html)
    (for **robust** test)

  - as `y` and `SE` arguments in
    [`metaBMA::meta_random()`](https://danheck.github.io/metaBMA/reference/meta_random.html)
    (for **Bayesian** test)

- type:

  A character specifying the type of statistical approach:

  - `"parametric"`

  - `"nonparametric"`

  - `"robust"`

  - `"bayes"`

  You can specify just the initial letter.

- random:

  The type of random effects distribution. One of "normal", "t-dist",
  "mixture", for standard normal, \\t\\-distribution or mixture of
  normals respectively.

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

- ...:

  Additional arguments passed to the respective meta-analysis function.

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

## Note

**Important**: The function assumes that you have already downloaded the
needed package (`{metafor}`, `{metaplus}`, or `{metaBMA}`) for
meta-analysis. If they are not available, you will be asked to install
them.

## Random-effects meta-analysis

The table below provides summary about:

- statistical test carried out for inferential statistics

- type of effect size estimate and a measure of uncertainty for this
  estimate

- functions used internally to compute these details

**Hypothesis testing** and **Effect size estimation**

|  |  |  |  |
|----|----|----|----|
| Type | Test | CI available? | Function used |
| Parametric | Pearson's correlation coefficient | Yes | [`correlation::correlation()`](https://easystats.github.io/correlation/reference/correlation.html) |
| Non-parametric | Spearman's rank correlation coefficient | Yes | [`correlation::correlation()`](https://easystats.github.io/correlation/reference/correlation.html) |
| Robust | Winsorized Pearson's correlation coefficient | Yes | [`correlation::correlation()`](https://easystats.github.io/correlation/reference/correlation.html) |
| Bayesian | Bayesian Pearson's correlation coefficient | Yes | [`correlation::correlation()`](https://easystats.github.io/correlation/reference/correlation.html) |

## Citation

Patil, I., (2021). statsExpressions: R Package for Tidy Dataframes and
Expressions with Statistical Details. Journal of Open Source Software,
6(61), 3236, https://doi.org/10.21105/joss.03236

## Examples

``` r
set.seed(123)
library(statsExpressions)

# let's use `mag` dataset from `{metaplus}`
data(mag, package = "metaplus")
dat <- dplyr::rename(mag, estimate = yi, std.error = sei)

# ----------------------- parametric ----------------------------------------

meta_analysis(dat)
#> # A tibble: 1 × 14
#>   term    effectsize                     estimate std.error conf.level conf.low
#>   <chr>   <chr>                             <dbl>     <dbl>      <dbl>    <dbl>
#> 1 Overall meta-analytic summary estimate   -0.767     0.212       0.95    -1.18
#>   conf.high statistic  p.value weight method                        conf.method
#>       <dbl>     <dbl>    <dbl>  <dbl> <chr>                         <chr>      
#> 1    -0.351     -3.62 0.000295     NA Meta-analysis using 'metafor' Wald       
#>   n.obs expression
#>   <int> <list>    
#> 1    16 <language>

# ----------------------- robust --------------------------------------------

meta_analysis(dat, type = "random", random = "normal")
#> # A tibble: 1 × 14
#>   term    effectsize                     estimate std.error conf.low conf.high
#>   <chr>   <chr>                             <dbl>     <dbl>    <dbl>     <dbl>
#> 1 Overall meta-analytic summary estimate   -0.746     0.234    -1.26    -0.343
#>   statistic  p.value weight conf.level method                               
#>       <dbl>    <dbl>  <dbl>      <dbl> <chr>                                
#> 1     -3.20 0.000501     NA       0.95 Robust meta-analysis using 'metaplus'
#>   conf.method n.obs expression
#>   <chr>       <int> <list>    
#> 1 Wald           16 <language>

# ----------------------- Bayesian ------------------------------------------

meta_analysis(dat, type = "bayes")
#> # A tibble: 2 × 20
#>   term    effectsize                       estimate std.error conf.level
#>   <chr>   <chr>                               <dbl>     <dbl>      <dbl>
#> 1 Overall meta-analytic posterior estimate   -0.643     0.220       0.95
#> 2 tau     meta-analytic posterior estimate    0.484     0.182       0.95
#>   conf.low conf.high weight  bf10  rhat   ess component prior.distribution
#>      <dbl>     <dbl>  <dbl> <dbl> <dbl> <dbl> <chr>     <chr>             
#> 1   -1.11     -0.242     NA  53.0     1 3507  meta      Student's t       
#> 2    0.205     0.909     NA  53.0     1 3460. meta      Inverse gamma     
#>   prior.location prior.scale method                                 conf.method
#>            <dbl>       <dbl> <chr>                                  <chr>      
#> 1              0       0.707 Bayesian meta-analysis using 'metaBMA' ETI        
#> 2              1       0.15  Bayesian meta-analysis using 'metaBMA' ETI        
#>   log_e_bf10 n.obs expression
#>        <dbl> <int> <list>    
#> 1       3.97    16 <language>
#> 2       3.97    16 <language>
```
