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
if (identical(Sys.getenv("NOT_CRAN"), "true")) {
  #### -------------------- association test ------------------------ ####

  # ------------------------ frequentist ---------------------------------

  # unpaired

  set.seed(123)
  contingency_table(
    data   = mtcars,
    x      = am,
    y      = vs,
    paired = FALSE
  )

  # paired

  paired_data <- tibble(
    response_before = structure(c(1L, 2L, 1L, 2L), levels = c("no", "yes"), class = "factor"),
    response_after = structure(c(1L, 1L, 2L, 2L), levels = c("no", "yes"), class = "factor"),
    Freq = c(65L, 25L, 5L, 5L)
  )

  set.seed(123)
  contingency_table(
    data   = paired_data,
    x      = response_before,
    y      = response_after,
    paired = TRUE,
    counts = Freq
  )

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

  #### -------------------- goodness-of-fit test -------------------- ####

  # ------------------------ frequentist ---------------------------------

  set.seed(123)
  contingency_table(
    data   = as.data.frame(HairEyeColor),
    x      = Eye,
    counts = Freq
  )

  # ------------------------ Bayesian -------------------------------------

  set.seed(123)
  contingency_table(
    data   = as.data.frame(HairEyeColor),
    x      = Eye,
    counts = Freq,
    ratio  = c(0.2, 0.2, 0.3, 0.3),
    type   = "bayes"
  )
}
#> # A tibble: 1 × 4
#>      bf10 prior.scale method                                      expression
#>     <dbl>       <dbl> <chr>                                       <list>    
#> 1 4.17e55           1 Bayesian one-way contingency table analysis <language>
```
