# Contingency table analyses

Parametric and Bayesian one-way and two-way contingency table analyses.

## Usage

``` r
contingency_table(
  data,
  x,
  y = NULL,
  paired = FALSE,
  type = "parametric",
  counts = NULL,
  ratio = NULL,
  alternative = "two.sided",
  digits = 2L,
  conf.level = 0.95,
  sampling.plan = "indepMulti",
  fixed.margin = "rows",
  prior.concentration = 1,
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

  The variable to use as the **rows** in the contingency table.

- y:

  The variable to use as the **columns** in the contingency table.
  Default is `NULL`. If `NULL`, one-sample proportion test (a goodness
  of fit test) will be run for the `x` variable.

- paired:

  Logical indicating whether data came from a within-subjects or
  repeated measures design study (Default: `FALSE`).

- type:

  A character specifying the type of statistical approach:

  - `"parametric"`

  - `"nonparametric"`

  - `"robust"`

  - `"bayes"`

  You can specify just the initial letter.

- counts:

  The variable in data containing counts, or `NULL` if each row
  represents a single observation.

- ratio:

  A vector of proportions: the expected proportions for the proportion
  test (should sum to `1`). Default is `NULL`, which means the null is
  equal theoretical proportions across the levels of the nominal
  variable. E.g., `ratio = c(0.5, 0.5)` for two levels,
  `ratio = c(0.25, 0.25, 0.25, 0.25)` for four levels, etc.

- alternative:

  A character string specifying the alternative hypothesis; Controls the
  type of CI returned: `"two.sided"` (default, two-sided CI),
  `"greater"` or `"less"` (one-sided CI). Partial matching is allowed
  (e.g., `"g"`, `"l"`, `"two"`...). See section *One-Sided CIs* in the
  [effectsize_CIs vignette](https://easystats.github.io/effectsize/).

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

- sampling.plan:

  Character describing the sampling plan. Possible options:

  - `"indepMulti"` (independent multinomial; default)

  - `"poisson"`

  - `"jointMulti"` (joint multinomial)

  - `"hypergeom"` (hypergeometric). For more, see
    [`BayesFactor::contingencyTableBF()`](https://rdrr.io/pkg/BayesFactor/man/contingencyTableBF.html).

- fixed.margin:

  For the independent multinomial sampling plan, which margin is fixed
  (`"rows"` or `"cols"`). Defaults to `"rows"`.

- prior.concentration:

  Specifies the prior concentration parameter, set to `1` by default. It
  indexes the expected deviation from the null hypothesis under the
  alternative, and corresponds to Gunel and Dickey's (1974) `"a"`
  parameter.

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

- `df`: the numeric value of a parameter being modeled (often degrees of
  freedom for the test)

- `df.error` and `df`: relevant only if the statistic in question has
  two degrees of freedom (e.g. anova)

- `p.value`: the two-sided *p*-value associated with the observed
  statistic

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

- `n.obs`: number of observations

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

## Contingency table analyses

The table below provides summary about:

- statistical test carried out for inferential statistics

- type of effect size estimate and a measure of uncertainty for this
  estimate

- functions used internally to compute these details

### two-way table

**Hypothesis testing**

|  |  |  |  |
|----|----|----|----|
| Type | Design | Test | Function used |
| Parametric/Non-parametric | Unpaired | Pearson's chi-squared test | [`stats::chisq.test()`](https://rdrr.io/r/stats/chisq.test.html) |
| Bayesian | Unpaired | Bayesian Pearson's chi-squared test | [`BayesFactor::contingencyTableBF()`](https://rdrr.io/pkg/BayesFactor/man/contingencyTableBF.html) |
| Parametric/Non-parametric | Paired | McNemar's chi-squared test | [`stats::mcnemar.test()`](https://rdrr.io/r/stats/mcnemar.test.html) |
| Bayesian | Paired | No | No |

**Effect size estimation**

|  |  |  |  |  |
|----|----|----|----|----|
| Type | Design | Effect size | CI available? | Function used |
| Parametric/Non-parametric | Unpaired | Cramer's *V* | Yes | [`effectsize::cramers_v()`](https://easystats.github.io/effectsize/reference/phi.html) |
| Bayesian | Unpaired | Cramer's *V* | Yes | [`effectsize::cramers_v()`](https://easystats.github.io/effectsize/reference/phi.html) |
| Parametric/Non-parametric | Paired | Cohen's *g* | Yes | [`effectsize::cohens_g()`](https://easystats.github.io/effectsize/reference/cohens_g.html) |
| Bayesian | Paired | No | No | No |

### one-way table

**Hypothesis testing**

|  |  |  |
|----|----|----|
| Type | Test | Function used |
| Parametric/Non-parametric | Goodness of fit chi-squared test | [`stats::chisq.test()`](https://rdrr.io/r/stats/chisq.test.html) |
| Bayesian | Bayesian Goodness of fit chi-squared test | (custom) |

**Effect size estimation**

|  |  |  |  |
|----|----|----|----|
| Type | Effect size | CI available? | Function used |
| Parametric/Non-parametric | Pearson's *C* | Yes | [`effectsize::pearsons_c()`](https://easystats.github.io/effectsize/reference/phi.html) |
| Bayesian | No | No | No |

## Examples

``` r
#### -------------------- association test ------------------------ ####

# ------------------------ frequentist ---------------------------------

# unpaired

set.seed(123)
contingency_table(
  data = mtcars,
  x = am,
  y = vs,
  paired = FALSE
)
#> # A tibble: 1 × 13
#>   statistic    df p.value method                     effectsize        estimate
#>       <dbl> <int>   <dbl> <chr>                      <chr>                <dbl>
#> 1     0.907     1   0.341 Pearson's Chi-squared test Cramer's V (adj.)        0
#>   conf.level conf.low conf.high conf.method conf.distribution n.obs expression
#>        <dbl>    <dbl>     <dbl> <chr>       <chr>             <int> <list>    
#> 1       0.95        0     0.490 ncp         chisq                32 <language>

# paired

paired_data <- dplyr::tibble(
  response_before = structure(
    c(1L, 2L, 1L, 2L),
    levels = c("no", "yes"),
    class = "factor"
  ),
  response_after = structure(
    c(1L, 1L, 2L, 2L),
    levels = c("no", "yes"),
    class = "factor"
  ),
  Freq = c(65L, 25L, 5L, 5L)
)

set.seed(123)
contingency_table(
  data = paired_data,
  x = response_before,
  y = response_after,
  paired = TRUE,
  counts = Freq
)
#> # A tibble: 1 × 12
#>   statistic    df  p.value method                     effectsize estimate
#>       <dbl> <dbl>    <dbl> <chr>                      <chr>         <dbl>
#> 1      13.3     1 0.000261 McNemar's Chi-squared test Cohen's g     0.333
#>   conf.level conf.low conf.high conf.method n.obs expression
#>        <dbl>    <dbl>     <dbl> <chr>       <int> <list>    
#> 1       0.95    0.164     0.427 binomial      100 <language>

# ------------------------ Bayesian -------------------------------------

# unpaired

set.seed(123)
contingency_table(
  data = mtcars,
  x = am,
  y = vs,
  paired = FALSE,
  type = "bayes"
)
#> # A tibble: 1 × 15
#>   term  conf.level effectsize estimate conf.low conf.high
#>   <chr>      <dbl> <chr>         <dbl>    <dbl>     <dbl>
#> 1 Ratio       0.95 Cramers_v         0        0     0.421
#>   prior.distribution      prior.location prior.scale  bf10
#>   <chr>                            <dbl>       <dbl> <dbl>
#> 1 independent multinomial              0           1 0.643
#>   method                              conf.method log_e_bf10 n.obs expression
#>   <chr>                               <chr>            <dbl> <int> <list>    
#> 1 Bayesian contingency table analysis ETI             -0.442    32 <language>

# paired

set.seed(123)
contingency_table(
  data = paired_data,
  x = response_before,
  y = response_after,
  paired = TRUE,
  counts = Freq,
  type = "bayes"
)
#> # A tibble: 1 × 15
#>   term  conf.level effectsize estimate conf.low conf.high
#>   <chr>      <dbl> <chr>         <dbl>    <dbl>     <dbl>
#> 1 Ratio       0.95 Cramers_v     0.111        0     0.340
#>   prior.distribution      prior.location prior.scale  bf10
#>   <chr>                            <dbl>       <dbl> <dbl>
#> 1 independent multinomial              0           1 0.461
#>   method                              conf.method log_e_bf10 n.obs expression
#>   <chr>                               <chr>            <dbl> <int> <list>    
#> 1 Bayesian contingency table analysis ETI             -0.775   100 <language>

#### -------------------- goodness-of-fit test -------------------- ####

# ------------------------ frequentist ---------------------------------

set.seed(123)
contingency_table(
  data = as.data.frame(HairEyeColor),
  x = Eye,
  counts = Freq
)
#> # A tibble: 1 × 13
#>   statistic    df  p.value method                                   effectsize 
#>       <dbl> <dbl>    <dbl> <chr>                                    <chr>      
#> 1      133.     3 9.65e-29 Chi-squared test for given probabilities Pearson's C
#>   estimate conf.level conf.low conf.high conf.method conf.distribution n.obs
#>      <dbl>      <dbl>    <dbl>     <dbl> <chr>       <chr>             <int>
#> 1    0.429       0.95    0.364     0.483 ncp         chisq               592
#>   expression
#>   <list>    
#> 1 <language>

# ------------------------ Bayesian -------------------------------------

set.seed(123)
contingency_table(
  data = as.data.frame(HairEyeColor),
  x = Eye,
  counts = Freq,
  ratio = c(0.2, 0.2, 0.3, 0.3),
  type = "bayes"
)
#> # A tibble: 1 × 4
#>      bf10 prior.scale method                                      expression
#>     <dbl>       <dbl> <chr>                                       <list>    
#> 1 4.17e55           1 Bayesian one-way contingency table analysis <language>
```
