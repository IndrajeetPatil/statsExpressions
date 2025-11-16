# Expressions with statistics for tidy regression data frames

Expressions with statistics for tidy regression data frames

## Usage

``` r
tidy_model_expressions(
  data,
  statistic = NULL,
  digits = 2L,
  effsize.type = "omega",
  ...
)
```

## Arguments

- data:

  A tidy data frame from regression model object (see
  [`tidy_model_parameters()`](https://indrajeetpatil.github.io/statsExpressions/reference/tidy_model_parameters.md)).

- statistic:

  Which statistic is to be displayed (either `"t"` or `"f"`or `"z"` or
  `"chi"`) in the expression.

- digits:

  Number of digits for rounding or significant figures. May also be
  `"signif"` to return significant figures or `"scientific"` to return
  scientific notation. Control the number of digits by adding the value
  as suffix, e.g. `digits = "scientific4"` to have scientific notation
  with 4 decimal places, or `digits = "signif5"` for 5 significant
  figures (see also [`signif()`](https://rdrr.io/r/base/Round.html)).

- effsize.type:

  Type of effect size needed for *parametric* tests. The argument can be
  `"eta"` (partial eta-squared) or `"omega"` (partial omega-squared).

- ...:

  Currently ignored.

## Details

When any of the necessary numeric column values (`estimate`,
`statistic`, `p.value`) are missing, for these rows, a `NULL` is
returned instead of an expression with empty strings.

## Citation

Patil, I., (2021). statsExpressions: R Package for Tidy Dataframes and
Expressions with Statistical Details. Journal of Open Source Software,
6(61), 3236, https://doi.org/10.21105/joss.03236

## Examples

``` r
# setup
set.seed(123)
library(statsExpressions)

# extract a tidy data frame
df <- tidy_model_parameters(lm(wt ~ am * cyl, mtcars))

# create a column containing expression; the expression will depend on `statistic`
tidy_model_expressions(df, statistic = "t")
#> # A tibble: 4 × 11
#>   term        estimate std.error conf.level conf.low conf.high statistic
#>   <chr>          <dbl>     <dbl>      <dbl>    <dbl>     <dbl>     <dbl>
#> 1 (Intercept)   1.66      0.587        0.95    0.455     2.86      2.82 
#> 2 am           -0.956     0.793        0.95   -2.58      0.668    -1.21 
#> 3 cyl           0.304     0.0826       0.95    0.135     0.473     3.68 
#> 4 am:cyl        0.0328    0.130        0.95   -0.234     0.300     0.252
#>   df.error  p.value conf.method expression
#>      <int>    <dbl> <chr>       <list>    
#> 1       28 0.00864  Wald        <language>
#> 2       28 0.238    Wald        <language>
#> 3       28 0.000989 Wald        <language>
#> 4       28 0.803    Wald        <language>
tidy_model_expressions(df, statistic = "z")
#> # A tibble: 4 × 11
#>   term        estimate std.error conf.level conf.low conf.high statistic
#>   <chr>          <dbl>     <dbl>      <dbl>    <dbl>     <dbl>     <dbl>
#> 1 (Intercept)   1.66      0.587        0.95    0.455     2.86      2.82 
#> 2 am           -0.956     0.793        0.95   -2.58      0.668    -1.21 
#> 3 cyl           0.304     0.0826       0.95    0.135     0.473     3.68 
#> 4 am:cyl        0.0328    0.130        0.95   -0.234     0.300     0.252
#>   df.error  p.value conf.method expression
#>      <int>    <dbl> <chr>       <list>    
#> 1       28 0.00864  Wald        <language>
#> 2       28 0.238    Wald        <language>
#> 3       28 0.000989 Wald        <language>
#> 4       28 0.803    Wald        <language>
tidy_model_expressions(df, statistic = "chi")
#> # A tibble: 4 × 11
#>   term        estimate std.error conf.level conf.low conf.high statistic
#>   <chr>          <dbl>     <dbl>      <dbl>    <dbl>     <dbl>     <dbl>
#> 1 (Intercept)   1.66      0.587        0.95    0.455     2.86      2.82 
#> 2 am           -0.956     0.793        0.95   -2.58      0.668    -1.21 
#> 3 cyl           0.304     0.0826       0.95    0.135     0.473     3.68 
#> 4 am:cyl        0.0328    0.130        0.95   -0.234     0.300     0.252
#>   df.error  p.value conf.method expression
#>      <int>    <dbl> <chr>       <list>    
#> 1       28 0.00864  Wald        <language>
#> 2       28 0.238    Wald        <language>
#> 3       28 0.000989 Wald        <language>
#> 4       28 0.803    Wald        <language>
```
