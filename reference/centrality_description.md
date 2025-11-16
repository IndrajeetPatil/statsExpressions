# Data frame and expression for distribution properties

Parametric, non-parametric, robust, and Bayesian measures of centrality.

## Usage

``` r
centrality_description(
  data,
  x,
  y,
  type = "parametric",
  conf.level = 0.95,
  tr = 0.2,
  digits = 2L,
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

  The grouping (or independent) variable in `data`.

- y:

  The response (or outcome or dependent) variable from `data`.

- type:

  A character specifying the type of statistical approach:

  - `"parametric"`

  - `"nonparametric"`

  - `"robust"`

  - `"bayes"`

  You can specify just the initial letter.

- conf.level:

  Scalar between `0` and `1` (default: `95%` confidence/credible
  intervals, `0.95`). If `NULL`, no confidence intervals will be
  computed.

- tr:

  Trim level for the mean when carrying out `robust` tests. In case of
  an error, try reducing the value of `tr`, which is by default set to
  `0.2`. Lowering the value might help.

- digits:

  Number of digits for rounding or significant figures. May also be
  `"signif"` to return significant figures or `"scientific"` to return
  scientific notation. Control the number of digits by adding the value
  as suffix, e.g. `digits = "scientific4"` to have scientific notation
  with 4 decimal places, or `digits = "signif5"` for 5 significant
  figures (see also [`signif()`](https://rdrr.io/r/base/Round.html)).

- ...:

  Currently ignored.

## Details

This function describes a distribution for `y` variable for each level
of the grouping variable in `x` by a set of indices (e.g., measures of
centrality, dispersion, range, skewness, kurtosis, etc.). It
additionally returns an expression containing a specified centrality
measure. The function internally relies on
[`datawizard::describe_distribution()`](https://easystats.github.io/datawizard/reference/describe_distribution.html)
function.

## Centrality measures

The table below provides summary about:

- statistical test carried out for inferential statistics

- type of effect size estimate and a measure of uncertainty for this
  estimate

- functions used internally to compute these details

|  |  |  |
|----|----|----|
| Type | Measure | Function used |
| Parametric | mean | [`datawizard::describe_distribution()`](https://easystats.github.io/datawizard/reference/describe_distribution.html) |
| Non-parametric | median | [`datawizard::describe_distribution()`](https://easystats.github.io/datawizard/reference/describe_distribution.html) |
| Robust | trimmed mean | [`datawizard::describe_distribution()`](https://easystats.github.io/datawizard/reference/describe_distribution.html) |
| Bayesian | MAP | [`datawizard::describe_distribution()`](https://easystats.github.io/datawizard/reference/describe_distribution.html) |

## Citation

Patil, I., (2021). statsExpressions: R Package for Tidy Dataframes and
Expressions with Statistical Details. Journal of Open Source Software,
6(61), 3236, https://doi.org/10.21105/joss.03236

## Examples

``` r
# for reproducibility
set.seed(123)

# ----------------------- parametric -----------------------

centrality_description(iris, Species, Sepal.Length, type = "parametric")
#> # A tibble: 3 × 14
#>   Species    Sepal.Length std.dev   iqr conf.low conf.high   min   max skewness
#>   <fct>             <dbl>   <dbl> <dbl>    <dbl>     <dbl> <dbl> <dbl>    <dbl>
#> 1 setosa             5.01   0.352 0.400     4.90      5.10   4.3   5.8    0.120
#> 2 versicolor         5.94   0.516 0.7       5.80      6.07   4.9   7      0.105
#> 3 virginica          6.59   0.636 0.750     6.39      6.79   4.9   7.9    0.118
#>   kurtosis n.obs missing.obs expression n.expression          
#>      <dbl> <int>       <int> <list>     <chr>                 
#> 1  -0.253     50           0 <language> "setosa\n(n = 50)"    
#> 2  -0.533     50           0 <language> "versicolor\n(n = 50)"
#> 3   0.0329    50           0 <language> "virginica\n(n = 50)" 

# ----------------------- non-parametric -------------------

centrality_description(mtcars, am, wt, type = "nonparametric")
#> # A tibble: 2 × 14
#>      am    wt   mad   iqr conf.low conf.high   min   max skewness kurtosis n.obs
#>   <dbl> <dbl> <dbl> <dbl>    <dbl>     <dbl> <dbl> <dbl>    <dbl>    <dbl> <int>
#> 1     0  3.52 0.452 0.41      3.44      3.84  2.46  5.42    1.15     1.06     19
#> 2     1  2.32 0.682 0.942     1.94      2.78  1.51  3.57    0.269   -0.654    13
#>   missing.obs expression n.expression 
#>         <int> <list>     <chr>        
#> 1           0 <language> "0\n(n = 19)"
#> 2           0 <language> "1\n(n = 13)"

# ----------------------- robust ---------------------------

centrality_description(ToothGrowth, supp, len, type = "robust")
#> # A tibble: 2 × 14
#>   supp    len std.dev   iqr conf.low conf.high   min   max skewness kurtosis
#>   <fct> <dbl>   <dbl> <dbl>    <dbl>     <dbl> <dbl> <dbl>    <dbl>    <dbl>
#> 1 OJ     21.7    6.61  10.9     17.5      23.5   8.2  30.9   -0.580   -0.831
#> 2 VC     16.6    8.27  12.5     13.6      19.6   4.2  33.9    0.306   -0.700
#>   n.obs missing.obs expression n.expression  
#>   <int>       <int> <list>     <chr>         
#> 1    30           0 <language> "OJ\n(n = 30)"
#> 2    30           0 <language> "VC\n(n = 30)"

# ----------------------- Bayesian -------------------------

centrality_description(sleep, group, extra, type = "bayes")
#> # A tibble: 2 × 13
#>   group  extra   iqr conf.low conf.high   min   max skewness kurtosis n.obs
#>   <fct>  <dbl> <dbl>    <dbl>     <dbl> <dbl> <dbl>    <dbl>    <dbl> <int>
#> 1 1     0.0579  2.8   -1.39        3.66  -1.6   3.7    0.581   -0.630    10
#> 2 2     0.973   3.82   0.0728      4.88  -0.1   5.5    0.386   -1.42     10
#>   missing.obs expression n.expression 
#>         <int> <list>     <chr>        
#> 1           0 <language> "1\n(n = 10)"
#> 2           0 <language> "2\n(n = 10)"
```
