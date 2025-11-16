# Template for expressions with statistical details

Creates an expression from a data frame containing statistical details.
Ideally, this data frame would come from having run
[`tidy_model_parameters()`](https://indrajeetpatil.github.io/statsExpressions/reference/tidy_model_parameters.md)
on your model object.

This function is currently **not** stable and should not be used outside
of this package context.

## Usage

``` r
add_expression_col(
  data,
  paired = FALSE,
  statistic.text = NULL,
  effsize.text = NULL,
  prior.type = NULL,
  n = NULL,
  n.text = ifelse(paired, list(quote(italic("n")["pairs"])),
    list(quote(italic("n")["obs"]))),
  digits = 2L,
  digits.df = 0L,
  digits.df.error = digits.df,
  ...
)
```

## Arguments

- data:

  A data frame containing details from the statistical analysis and
  should contain some or all of the the following columns:

  - *statistic*: the numeric value of a statistic.

  - *df.error*: the numeric value of a parameter being modeled (often
    degrees of freedom for the test); irrelevant. if there are no
    degrees of freedom.

  - *df*: relevant if the statistic in question has two degrees of
    freedom.

  - *p.value*: the two-sided *p*-value associated with observed
    statistic.

  - *method*: method describing the test carried out.

  - *effectsize*: name of the effect size (if not present, same as
    `method`).

  - *estimate*: estimated value of the effect size.

  - *conf.level*: width for the confidence intervals.

  - *conf.low*: lower bound for effect size estimate.

  - *conf.high*: upper bound for effect size estimate.

  - *bf10*: Bayes Factor value (if `bayesian = TRUE`).

- paired:

  Logical that decides whether the experimental design is repeated
  measures/within-subjects or between-subjects. The default is `FALSE`.

- statistic.text:

  A character that specifies the relevant test statistic. For example,
  for tests with *t*-statistic, `statistic.text = "t"`.

- effsize.text:

  A character that specifies the relevant effect size.

- prior.type:

  The type of prior.

- n:

  An integer specifying the sample size used for the test.

- n.text:

  A character that specifies the design, which will determine what the
  `n` stands for. It defaults to `quote(italic("n")["pairs"])` if
  `paired = TRUE`, and to `quote(italic("n")["obs"])` if
  `paired = FALSE`. If you wish to customize this further, you will need
  to provide object of `language` type.

- digits, digits.df, digits.df.error:

  Number of decimal places to display for the parameters (default:
  `0L`).

- ...:

  Currently ignored.

## Citation

Patil, I., (2021). statsExpressions: R Package for Tidy Dataframes and
Expressions with Statistical Details. Journal of Open Source Software,
6(61), 3236, https://doi.org/10.21105/joss.03236

## Examples

``` r
set.seed(123)

# creating a data frame with stats results
stats_df <- cbind.data.frame(
  statistic  = 5.494,
  df         = 29.234,
  p.value    = 0.00001,
  estimate   = -1.980,
  conf.level = 0.95,
  conf.low   = -2.873,
  conf.high  = -1.088,
  method     = "Student's t-test"
)

# expression for *t*-statistic with Cohen's *d* as effect size
# note that the plotmath expressions need to be quoted
add_expression_col(
  data           = stats_df,
  statistic.text = list(quote(italic("t"))),
  effsize.text   = list(quote(italic("d"))),
  n              = 32L,
  n.text         = list(quote(italic("n")["no.obs"])),
  digits         = 3L,
  digits.df      = 3L
)
#> # A tibble: 1 × 11
#>   statistic    df p.value effectsize       estimate conf.level conf.low
#>       <dbl> <dbl>   <dbl> <chr>               <dbl>      <dbl>    <dbl>
#> 1      5.49  29.2 0.00001 Student's t-test    -1.98       0.95    -2.87
#>   conf.high method           n.obs expression
#>       <dbl> <chr>            <int> <list>    
#> 1     -1.09 Student's t-test    32 <language>
```
