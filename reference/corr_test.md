# Correlation analyses

Parametric, non-parametric, robust, and Bayesian correlation test.

## Usage

``` r
corr_test(
  data,
  x,
  y,
  type = "parametric",
  digits = 2L,
  conf.level = 0.95,
  tr = 0.2,
  bf.prior = 0.707,
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

  The column in `data` containing the explanatory variable to be plotted
  on the `x`-axis.

- y:

  The column in `data` containing the response (outcome) variable to be
  plotted on the `y`-axis.

- type:

  A character specifying the type of statistical approach:

  - `"parametric"`

  - `"nonparametric"`

  - `"robust"`

  - `"bayes"`

  You can specify just the initial letter.

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

- ...:

  Additional arguments (currently ignored).

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

## Correlation analyses

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
# for reproducibility
set.seed(123)

# ----------------------- parametric -----------------------

corr_test(mtcars, wt, mpg, type = "parametric")
#> # A tibble: 1 × 14
#>   parameter1 parameter2 effectsize          estimate conf.level conf.low
#>   <chr>      <chr>      <chr>                  <dbl>      <dbl>    <dbl>
#> 1 wt         mpg        Pearson correlation   -0.868       0.95   -0.934
#>   conf.high statistic df.error  p.value method              n.obs conf.method
#>       <dbl>     <dbl>    <int>    <dbl> <chr>               <int> <chr>      
#> 1    -0.744     -9.56       30 1.29e-10 Pearson correlation    32 normal     
#>   expression
#>   <list>    
#> 1 <language>

# ----------------------- non-parametric -------------------

corr_test(mtcars, wt, mpg, type = "nonparametric")
#> # A tibble: 1 × 13
#>   parameter1 parameter2 effectsize           estimate conf.level conf.low
#>   <chr>      <chr>      <chr>                   <dbl>      <dbl>    <dbl>
#> 1 wt         mpg        Spearman correlation   -0.886       0.95   -0.945
#>   conf.high statistic  p.value method               n.obs conf.method expression
#>       <dbl>     <dbl>    <dbl> <chr>                <int> <chr>       <list>    
#> 1    -0.774    10292. 1.49e-11 Spearman correlation    32 normal      <language>

# ----------------------- robust ---------------------------

corr_test(mtcars, wt, mpg, type = "robust")
#> # A tibble: 1 × 14
#>   parameter1 parameter2 effectsize                     estimate conf.level
#>   <chr>      <chr>      <chr>                             <dbl>      <dbl>
#> 1 wt         mpg        Winsorized Pearson correlation   -0.864       0.95
#>   conf.low conf.high statistic df.error  p.value method                        
#>      <dbl>     <dbl>     <dbl>    <int>    <dbl> <chr>                         
#> 1   -0.932    -0.738     -9.41       30 1.84e-10 Winsorized Pearson correlation
#>   n.obs conf.method expression
#>   <int> <chr>       <list>    
#> 1    32 normal      <language>

# ----------------------- Bayesian -------------------------

corr_test(mtcars, wt, mpg, type = "bayes")
#> # A tibble: 1 × 17
#>   parameter1 parameter2 effectsize                   estimate conf.level
#>   <chr>      <chr>      <chr>                           <dbl>      <dbl>
#> 1 wt         mpg        Bayesian Pearson correlation   -0.843       0.95
#>   conf.low conf.high    pd rope.percentage prior.distribution prior.location
#>      <dbl>     <dbl> <dbl>           <dbl> <chr>                       <dbl>
#> 1   -0.934    -0.734     1               0 beta                         1.41
#>   prior.scale      bf10 method                       n.obs conf.method
#>         <dbl>     <dbl> <chr>                        <int> <chr>      
#> 1        1.41 56223033. Bayesian Pearson correlation    32 HDI        
#>   expression
#>   <list>    
#> 1 <language>
```
