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

## One-way ANOVA

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
| Parametric | \> 2 | Fisher's or Welch's one-way ANOVA | [`stats::oneway.test()`](https://rdrr.io/r/stats/oneway.test.html) |
| Non-parametric | \> 2 | Kruskal-Wallis one-way ANOVA | [`stats::kruskal.test()`](https://rdrr.io/r/stats/kruskal.test.html) |
| Robust | \> 2 | Heteroscedastic one-way ANOVA for trimmed means | [`WRS2::t1way()`](https://rdrr.io/pkg/WRS2/man/t1way.html) |
| Bayes Factor | \> 2 | Fisher's ANOVA | [`BayesFactor::anovaBF()`](https://rdrr.io/pkg/BayesFactor/man/anovaBF.html) |

**Effect size estimation**

|  |  |  |  |  |
|----|----|----|----|----|
| Type | No. of groups | Effect size | CI available? | Function used |
| Parametric | \> 2 | partial eta-squared, partial omega-squared | Yes | [`effectsize::omega_squared()`](https://easystats.github.io/effectsize/reference/eta_squared.html), [`effectsize::eta_squared()`](https://easystats.github.io/effectsize/reference/eta_squared.html) |
| Non-parametric | \> 2 | rank epsilon squared | Yes | [`effectsize::rank_epsilon_squared()`](https://easystats.github.io/effectsize/reference/rank_epsilon_squared.html) |
| Robust | \> 2 | Explanatory measure of effect size | Yes | [`WRS2::t1way()`](https://rdrr.io/pkg/WRS2/man/t1way.html) |
| Bayes Factor | \> 2 | Bayesian R-squared | Yes | [`performance::r2_bayes()`](https://easystats.github.io/performance/reference/r2_bayes.html) |

### within-subjects

**Hypothesis testing**

|  |  |  |  |
|----|----|----|----|
| Type | No. of groups | Test | Function used |
| Parametric | \> 2 | One-way repeated measures ANOVA | [`afex::aov_ez()`](https://rdrr.io/pkg/afex/man/aov_car.html) |
| Non-parametric | \> 2 | Friedman rank sum test | [`stats::friedman.test()`](https://rdrr.io/r/stats/friedman.test.html) |
| Robust | \> 2 | Heteroscedastic one-way repeated measures ANOVA for trimmed means | [`WRS2::rmanova()`](https://rdrr.io/pkg/WRS2/man/rmanova.html) |
| Bayes Factor | \> 2 | One-way repeated measures ANOVA | [`BayesFactor::anovaBF()`](https://rdrr.io/pkg/BayesFactor/man/anovaBF.html) |

**Effect size estimation**

|  |  |  |  |  |
|----|----|----|----|----|
| Type | No. of groups | Effect size | CI available? | Function used |
| Parametric | \> 2 | partial eta-squared, partial omega-squared | Yes | [`effectsize::omega_squared()`](https://easystats.github.io/effectsize/reference/eta_squared.html), [`effectsize::eta_squared()`](https://easystats.github.io/effectsize/reference/eta_squared.html) |
| Non-parametric | \> 2 | Kendall's coefficient of concordance | Yes | [`effectsize::kendalls_w()`](https://easystats.github.io/effectsize/reference/rank_epsilon_squared.html) |
| Robust | \> 2 | Algina-Keselman-Penfield robust standardized difference average | Yes | [`WRS2::wmcpAKP()`](https://rdrr.io/pkg/WRS2/man/wmcpAKP.html) |
| Bayes Factor | \> 2 | Bayesian R-squared | Yes | [`performance::r2_bayes()`](https://easystats.github.io/performance/reference/r2_bayes.html) |

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
  x    = cyl,
  y    = wt
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

# within-subjects design
oneway_anova(
  data       = iris_long,
  x          = condition,
  y          = value,
  subject.id = id,
  paired     = TRUE
)
#> # A tibble: 1 × 18
#>   term      sumsq sum.squares.error    df df.error meansq statistic  p.value
#>   <chr>     <dbl>             <dbl> <dbl>    <dbl>  <dbl>     <dbl>    <dbl>
#> 1 condition 1656.              318.  1.15     171.   1.86      776. 1.32e-69
#>   method                                              effectsize       estimate
#>   <chr>                                               <chr>               <dbl>
#> 1 ANOVA estimation for factorial designs using 'afex' Omega2 (partial)    0.707
#>   conf.level conf.low conf.high conf.method conf.distribution n.obs expression
#>        <dbl>    <dbl>     <dbl> <chr>       <chr>             <int> <list>    
#> 1       0.95    0.673         1 ncp         F                   150 <language>

# ----------------------- non-parametric ----------------------------------

# between-subjects
oneway_anova(
  data = mtcars,
  x    = cyl,
  y    = wt,
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
  data       = iris_long,
  x          = condition,
  y          = value,
  subject.id = id,
  paired     = TRUE,
  type       = "np"
)
#> # A tibble: 1 × 15
#>   parameter1 parameter2 statistic df.error  p.value method                
#>   <chr>      <chr>          <dbl>    <dbl>    <dbl> <chr>                 
#> 1 value      condition        410        3 1.51e-88 Friedman rank sum test
#>   effectsize  estimate conf.level conf.low conf.high conf.method         
#>   <chr>          <dbl>      <dbl>    <dbl>     <dbl> <chr>               
#> 1 Kendall's W    0.911       0.95    0.904         1 percentile bootstrap
#>   conf.iterations n.obs expression
#>             <int> <int> <list>    
#> 1             100   150 <language>

# ----------------------- robust -------------------------------------

# between-subjects
oneway_anova(
  data = mtcars,
  x    = cyl,
  y    = wt,
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
#> 1 Explanatory measure of effect size     1.02       0.95    0.828      1.35
#>   n.obs expression
#>   <int> <list>    
#> 1    32 <language>

# within-subjects design
oneway_anova(
  data       = iris_long,
  x          = condition,
  y          = value,
  subject.id = id,
  paired     = TRUE,
  type       = "r"
)
#> # A tibble: 1 × 12
#>   statistic    df df.error   p.value
#>       <dbl> <dbl>    <dbl>     <dbl>
#> 1      368.  1.09     97.1 2.23e-308
#>   method                                                             
#>   <chr>                                                              
#> 1 A heteroscedastic one-way repeated measures ANOVA for trimmed means
#>   effectsize                                                      estimate
#>   <chr>                                                              <dbl>
#> 1 Algina-Keselman-Penfield robust standardized difference average   -0.349
#>   conf.level conf.low conf.high n.obs expression
#>        <dbl>    <dbl>     <dbl> <int> <list>    
#> 1       0.95   -0.755     0.123   150 <language>

# ----------------------- Bayesian -------------------------------------

# between-subjects
oneway_anova(
  data = mtcars,
  x    = cyl,
  y    = wt,
  type = "bayes"
)
#> # A tibble: 6 × 17
#>   term     pd prior.distribution prior.location prior.scale   bf10
#>   <chr> <dbl> <chr>                       <dbl>       <dbl>  <dbl>
#> 1 mu    1     cauchy                          0       0.707 20968.
#> 2 cyl-4 1     cauchy                          0       0.707 20968.
#> 3 cyl-6 0.552 cauchy                          0       0.707 20968.
#> 4 cyl-8 1     cauchy                          0       0.707 20968.
#> 5 sig2  1     cauchy                          0       0.707 20968.
#> 6 g_cyl 1     cauchy                          0       0.707 20968.
#>   method                          log_e_bf10 effectsize         estimate std.dev
#>   <chr>                                <dbl> <chr>                 <dbl>   <dbl>
#> 1 Bayes factors for linear models       9.95 Bayesian R-squared    0.577  0.0869
#> 2 Bayes factors for linear models       9.95 Bayesian R-squared    0.577  0.0869
#> 3 Bayes factors for linear models       9.95 Bayesian R-squared    0.577  0.0869
#> 4 Bayes factors for linear models       9.95 Bayesian R-squared    0.577  0.0869
#> 5 Bayes factors for linear models       9.95 Bayesian R-squared    0.577  0.0869
#> 6 Bayes factors for linear models       9.95 Bayesian R-squared    0.577  0.0869
#>   conf.level conf.low conf.high conf.method n.obs expression
#>        <dbl>    <dbl>     <dbl> <chr>       <int> <list>    
#> 1       0.95    0.379     0.707 HDI            32 <language>
#> 2       0.95    0.379     0.707 HDI            32 <language>
#> 3       0.95    0.379     0.707 HDI            32 <language>
#> 4       0.95    0.379     0.707 HDI            32 <language>
#> 5       0.95    0.379     0.707 HDI            32 <language>
#> 6       0.95    0.379     0.707 HDI            32 <language>

# within-subjects design
oneway_anova(
  data       = iris_long,
  x          = condition,
  y          = value,
  subject.id = id,
  paired     = TRUE,
  type       = "bayes"
)
#> Multiple `BFBayesFactor` models detected - posteriors are extracted from
#>   the first numerator model.
#>   See help("get_parameters", package = "insight").
#> # A tibble: 8 × 19
#>   term                      pd prior.distribution prior.location prior.scale
#>   <chr>                  <dbl> <chr>                       <dbl>       <dbl>
#> 1 mu                         1 cauchy                          0       0.707
#> 2 condition-Petal.Length     1 cauchy                          0       0.707
#> 3 condition-Petal.Width      1 cauchy                          0       0.707
#> 4 condition-Sepal.Length     1 cauchy                          0       0.707
#> 5 condition-Sepal.Width      1 cauchy                          0       0.707
#> 6 sig2                       1 cauchy                          0       1    
#> 7 g_condition                1 cauchy                          0       1    
#> 8 g_.rowid                   1 cauchy                          0       1    
#>   effect     bf10 method                          log_e_bf10 effectsize        
#>   <chr>     <dbl> <chr>                                <dbl> <chr>             
#> 1 fixed  1.55e182 Bayes factors for linear models       420. Bayesian R-squared
#> 2 fixed  1.55e182 Bayes factors for linear models       420. Bayesian R-squared
#> 3 fixed  1.55e182 Bayes factors for linear models       420. Bayesian R-squared
#> 4 fixed  1.55e182 Bayes factors for linear models       420. Bayesian R-squared
#> 5 fixed  1.55e182 Bayes factors for linear models       420. Bayesian R-squared
#> 6 fixed  1.55e182 Bayes factors for linear models       420. Bayesian R-squared
#> 7 fixed  1.55e182 Bayes factors for linear models       420. Bayesian R-squared
#> 8 fixed  1.55e182 Bayes factors for linear models       420. Bayesian R-squared
#>   estimate std.dev conf.level conf.low conf.high conf.method component   n.obs
#>      <dbl>   <dbl>      <dbl>    <dbl>     <dbl> <chr>       <chr>       <int>
#> 1    0.818 0.00841       0.95    0.800     0.834 HDI         conditional   150
#> 2    0.818 0.00841       0.95    0.800     0.834 HDI         conditional   150
#> 3    0.818 0.00841       0.95    0.800     0.834 HDI         conditional   150
#> 4    0.818 0.00841       0.95    0.800     0.834 HDI         conditional   150
#> 5    0.818 0.00841       0.95    0.800     0.834 HDI         conditional   150
#> 6    0.818 0.00841       0.95    0.800     0.834 HDI         conditional   150
#> 7    0.818 0.00841       0.95    0.800     0.834 HDI         conditional   150
#> 8    0.818 0.00841       0.95    0.800     0.834 HDI         conditional   150
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
