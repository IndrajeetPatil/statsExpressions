# Multiple pairwise comparison for one-way design

Calculate parametric, non-parametric, robust, and Bayes Factor pairwise
comparisons between group levels with corrections for multiple testing.

## Usage

``` r
pairwise_comparisons(
  data,
  x,
  y,
  subject.id = NULL,
  type = "parametric",
  paired = FALSE,
  var.equal = FALSE,
  tr = 0.2,
  bf.prior = 0.707,
  p.adjust.method = "holm",
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

- var.equal:

  a logical variable indicating whether to treat the two variances as
  being equal. If `TRUE` then the pooled variance is used to estimate
  the variance otherwise the Welch (or Satterthwaite) approximation to
  the degrees of freedom is used.

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

- p.adjust.method:

  Adjustment method for *p*-values for multiple comparisons. Possible
  methods are: `"holm"` (default), `"hochberg"`, `"hommel"`,
  `"bonferroni"`, `"BH"`, `"BY"`, `"fdr"`, `"none"`.

- digits:

  Number of digits for rounding or significant figures. May also be
  `"signif"` to return significant figures or `"scientific"` to return
  scientific notation. Control the number of digits by adding the value
  as suffix, e.g. `digits = "scientific4"` to have scientific notation
  with 4 decimal places, or `digits = "signif5"` for 5 significant
  figures (see also [`signif()`](https://rdrr.io/r/base/Round.html)).

- ...:

  Additional arguments passed to other methods.

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

## Pairwise comparison tests

The table below provides summary about:

- statistical test carried out for inferential statistics

- type of effect size estimate and a measure of uncertainty for this
  estimate

- functions used internally to compute these details

### between-subjects

**Hypothesis testing**

|  |  |  |  |  |
|----|----|----|----|----|
| Type | Equal variance? | Test | *p*-value adjustment? | Function used |
| Parametric | No | Games-Howell test | Yes | [`PMCMRplus::gamesHowellTest()`](https://rdrr.io/pkg/PMCMRplus/man/gamesHowellTest.html) |
| Parametric | Yes | Student's *t*-test | Yes | [`stats::pairwise.t.test()`](https://rdrr.io/r/stats/pairwise.t.test.html) |
| Non-parametric | No | Dunn test | Yes | [`PMCMRplus::kwAllPairsDunnTest()`](https://rdrr.io/pkg/PMCMRplus/man/kwAllPairsDunnTest.html) |
| Robust | No | Yuen's trimmed means test | Yes | [`WRS2::lincon()`](https://rdrr.io/pkg/WRS2/man/t1way.html) |
| Bayesian | `NA` | Student's *t*-test | `NA` | [`BayesFactor::ttestBF()`](https://rdrr.io/pkg/BayesFactor/man/ttestBF.html) |

**Effect size estimation**

Not supported.

### within-subjects

**Hypothesis testing**

|  |  |  |  |
|----|----|----|----|
| Type | Test | *p*-value adjustment? | Function used |
| Parametric | Student's *t*-test | Yes | [`stats::pairwise.t.test()`](https://rdrr.io/r/stats/pairwise.t.test.html) |
| Non-parametric | Durbin-Conover test | Yes | [`PMCMRplus::durbinAllPairsTest()`](https://rdrr.io/pkg/PMCMRplus/man/durbinAllPairsTest.html) |
| Robust | Yuen's trimmed means test | Yes | [`WRS2::rmmcp()`](https://rdrr.io/pkg/WRS2/man/rmanova.html) |
| Bayesian | Student's *t*-test | `NA` | [`BayesFactor::ttestBF()`](https://rdrr.io/pkg/BayesFactor/man/ttestBF.html) |

**Effect size estimation**

Not supported.

## Citation

Patil, I., (2021). statsExpressions: R Package for Tidy Dataframes and
Expressions with Statistical Details. Journal of Open Source Software,
6(61), 3236, https://doi.org/10.21105/joss.03236

## References

For more, see:
<https://indrajeetpatil.github.io/ggstatsplot/articles/web_only/pairwise.html>

## Examples

``` r
# for reproducibility
set.seed(123)
library(statsExpressions)

#------------------- between-subjects design ----------------------------

# parametric
# if `var.equal = TRUE`, then Student's t-test will be run
pairwise_comparisons(
  data            = mtcars,
  x               = cyl,
  y               = wt,
  type            = "parametric",
  var.equal       = TRUE,
  paired          = FALSE,
  p.adjust.method = "none"
)
#> # A tibble: 3 × 6
#>   group1 group2     p.value p.adjust.method test        expression
#>   <chr>  <chr>        <dbl> <chr>           <chr>       <list>    
#> 1 4      6      0.0106      None            Student's t <language>
#> 2 4      8      0.000000207 None            Student's t <language>
#> 3 6      8      0.00516     None            Student's t <language>

# if `var.equal = FALSE`, then Games-Howell test will be run
pairwise_comparisons(
  data            = mtcars,
  x               = cyl,
  y               = wt,
  type            = "parametric",
  var.equal       = FALSE,
  paired          = FALSE,
  p.adjust.method = "bonferroni"
)
#> # A tibble: 3 × 9
#>   group1 group2 statistic   p.value alternative distribution p.adjust.method
#>   <chr>  <chr>      <dbl>     <dbl> <chr>       <chr>        <chr>          
#> 1 4      6           5.39 0.0125    two.sided   q            Bonferroni     
#> 2 4      8           9.11 0.0000124 two.sided   q            Bonferroni     
#> 3 6      8           5.12 0.0148    two.sided   q            Bonferroni     
#>   test         expression
#>   <chr>        <list>    
#> 1 Games-Howell <language>
#> 2 Games-Howell <language>
#> 3 Games-Howell <language>

# non-parametric (Dunn test)
pairwise_comparisons(
  data            = mtcars,
  x               = cyl,
  y               = wt,
  type            = "nonparametric",
  paired          = FALSE,
  p.adjust.method = "none"
)
#> # A tibble: 3 × 9
#>   group1 group2 statistic    p.value alternative distribution p.adjust.method
#>   <chr>  <chr>      <dbl>      <dbl> <chr>       <chr>        <chr>          
#> 1 4      6           1.84 0.0663     two.sided   z            None           
#> 2 4      8           4.76 0.00000198 two.sided   z            None           
#> 3 6      8           2.22 0.0263     two.sided   z            None           
#>   test  expression
#>   <chr> <list>    
#> 1 Dunn  <language>
#> 2 Dunn  <language>
#> 3 Dunn  <language>

# robust (Yuen's trimmed means *t*-test)
pairwise_comparisons(
  data            = mtcars,
  x               = cyl,
  y               = wt,
  type            = "robust",
  paired          = FALSE,
  p.adjust.method = "fdr"
)
#> # A tibble: 3 × 10
#>   group1 group2 estimate conf.level conf.low conf.high  p.value p.adjust.method
#>   <chr>  <chr>     <dbl>      <dbl>    <dbl>     <dbl>    <dbl> <chr>          
#> 1 4      6        -0.909       0.95    -1.64    -0.173 0.00872  FDR            
#> 2 4      8        -1.62        0.95    -2.50    -0.746 0.000549 FDR            
#> 3 6      8        -0.713       0.95    -1.58     0.155 0.0438   FDR            
#>   test                 expression
#>   <chr>                <list>    
#> 1 Yuen's trimmed means <language>
#> 2 Yuen's trimmed means <language>
#> 3 Yuen's trimmed means <language>

# Bayes Factor (Student's *t*-test)
pairwise_comparisons(
  data   = mtcars,
  x      = cyl,
  y      = wt,
  type   = "bayes",
  paired = FALSE
)
#> # A tibble: 3 × 18
#>   group1 group2 term       effectsize      estimate conf.level conf.low
#>   <chr>  <chr>  <chr>      <chr>              <dbl>      <dbl>    <dbl>
#> 1 4      6      Difference Bayesian t-test   -0.686       0.95    -1.22
#> 2 4      8      Difference Bayesian t-test   -1.63        0.95    -2.21
#> 3 6      8      Difference Bayesian t-test   -0.715       0.95    -1.36
#>   conf.high    pd prior.distribution prior.location prior.scale    bf10
#>       <dbl> <dbl> <chr>                       <dbl>       <dbl>   <dbl>
#> 1   -0.157  0.992 cauchy                          0       0.707   11.4 
#> 2   -1.01   1     cauchy                          0       0.707 5222.  
#> 3   -0.0910 0.987 cauchy                          0       0.707    5.36
#>   conf.method log_e_bf10 n.obs expression test       
#>   <chr>            <dbl> <int> <list>     <chr>      
#> 1 ETI               2.44    18 <language> Student's t
#> 2 ETI               8.56    25 <language> Student's t
#> 3 ETI               1.68    21 <language> Student's t

#------------------- within-subjects design ----------------------------

# parametric (Student's *t*-test)
pairwise_comparisons(
  data            = bugs_long,
  x               = condition,
  y               = desire,
  subject.id      = subject,
  type            = "parametric",
  paired          = TRUE,
  p.adjust.method = "BH"
)
#> # A tibble: 6 × 6
#>   group1 group2  p.value p.adjust.method test        expression
#>   <chr>  <chr>     <dbl> <chr>           <chr>       <list>    
#> 1 HDHF   HDLF   1.06e- 3 FDR             Student's t <language>
#> 2 HDHF   LDHF   7.02e- 2 FDR             Student's t <language>
#> 3 HDHF   LDLF   3.95e-12 FDR             Student's t <language>
#> 4 HDLF   LDHF   6.74e- 2 FDR             Student's t <language>
#> 5 HDLF   LDLF   1.99e- 3 FDR             Student's t <language>
#> 6 LDHF   LDLF   6.66e- 9 FDR             Student's t <language>

# non-parametric (Durbin-Conover test)
pairwise_comparisons(
  data            = bugs_long,
  x               = condition,
  y               = desire,
  subject.id      = subject,
  type            = "nonparametric",
  paired          = TRUE,
  p.adjust.method = "BY"
)
#> # A tibble: 6 × 9
#>   group1 group2 statistic  p.value alternative distribution p.adjust.method
#>   <chr>  <chr>      <dbl>    <dbl> <chr>       <chr>        <chr>          
#> 1 HDHF   HDLF        4.78 1.44e- 5 two.sided   t            BY             
#> 2 HDHF   LDHF        2.44 4.47e- 2 two.sided   t            BY             
#> 3 HDHF   LDLF        8.01 5.45e-13 two.sided   t            BY             
#> 4 HDLF   LDHF        2.34 4.96e- 2 two.sided   t            BY             
#> 5 HDLF   LDLF        3.23 5.05e- 3 two.sided   t            BY             
#> 6 LDHF   LDLF        5.57 4.64e- 7 two.sided   t            BY             
#>   test           expression
#>   <chr>          <list>    
#> 1 Durbin-Conover <language>
#> 2 Durbin-Conover <language>
#> 3 Durbin-Conover <language>
#> 4 Durbin-Conover <language>
#> 5 Durbin-Conover <language>
#> 6 Durbin-Conover <language>

# robust (Yuen's trimmed means t-test)
pairwise_comparisons(
  data            = bugs_long,
  x               = condition,
  y               = desire,
  subject.id      = subject,
  type            = "robust",
  paired          = TRUE,
  p.adjust.method = "hommel"
)
#> # A tibble: 6 × 11
#>   group1 group2 estimate conf.level conf.low conf.high     p.value  p.crit
#>   <chr>  <chr>     <dbl>      <dbl>    <dbl>     <dbl>       <dbl>   <dbl>
#> 1 HDHF   HDLF      1.03        0.95   0.140      1.92  0.00999     0.0127 
#> 2 HDHF   LDHF      0.454       0.95  -0.104      1.01  0.0520      0.025  
#> 3 HDHF   LDLF      1.95        0.95   1.09       2.82  0.000000564 0.00851
#> 4 HDLF   LDHF     -0.676       0.95  -1.61       0.256 0.0520      0.05   
#> 5 HDLF   LDLF      0.889       0.95   0.0244     1.75  0.0203      0.0169 
#> 6 LDHF   LDLF      1.35        0.95   0.560      2.14  0.000102    0.0102 
#>   p.adjust.method test                 expression
#>   <chr>           <chr>                <list>    
#> 1 Hommel          Yuen's trimmed means <language>
#> 2 Hommel          Yuen's trimmed means <language>
#> 3 Hommel          Yuen's trimmed means <language>
#> 4 Hommel          Yuen's trimmed means <language>
#> 5 Hommel          Yuen's trimmed means <language>
#> 6 Hommel          Yuen's trimmed means <language>

# Bayes Factor (Student's *t*-test)
pairwise_comparisons(
  data       = bugs_long,
  x          = condition,
  y          = desire,
  subject.id = subject,
  type       = "bayes",
  paired     = TRUE
)
#> # A tibble: 6 × 18
#>   group1 group2 term       effectsize      estimate conf.level conf.low
#>   <chr>  <chr>  <chr>      <chr>              <dbl>      <dbl>    <dbl>
#> 1 HDHF   HDLF   Difference Bayesian t-test    1.10        0.95   0.491 
#> 2 HDHF   LDHF   Difference Bayesian t-test    0.455       0.95  -0.0483
#> 3 HDHF   LDLF   Difference Bayesian t-test    2.13        0.95   1.62  
#> 4 HDLF   LDHF   Difference Bayesian t-test   -0.661       0.95  -1.32  
#> 5 HDLF   LDLF   Difference Bayesian t-test    0.991       0.95   0.369 
#> 6 LDHF   LDLF   Difference Bayesian t-test    1.65        0.95   1.14  
#>   conf.high    pd prior.distribution prior.location prior.scale     bf10
#>       <dbl> <dbl> <chr>                       <dbl>       <dbl>    <dbl>
#> 1    1.76   1.000 cauchy                          0       0.707 4.16e+ 1
#> 2    0.955  0.962 cauchy                          0       0.707 5.83e- 1
#> 3    2.63   1     cauchy                          0       0.707 1.20e+10
#> 4    0.0315 0.97  cauchy                          0       0.707 6.98e- 1
#> 5    1.57   0.999 cauchy                          0       0.707 1.81e+ 1
#> 6    2.15   1     cauchy                          0       0.707 4.81e+ 6
#>   conf.method log_e_bf10 n.obs expression test       
#>   <chr>            <dbl> <int> <list>     <chr>      
#> 1 ETI              3.73     88 <language> Student's t
#> 2 ETI             -0.539    88 <language> Student's t
#> 3 ETI             23.2      88 <language> Student's t
#> 4 ETI             -0.359    88 <language> Student's t
#> 5 ETI              2.90     88 <language> Student's t
#> 6 ETI             15.4      88 <language> Student's t
```
