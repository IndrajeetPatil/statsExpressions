# Convert `{parameters}` package output to `{tidyverse}` conventions

Convert `{parameters}` package output to `{tidyverse}` conventions

## Usage

``` r
tidy_model_parameters(model, ...)
```

## Arguments

- model:

  Statistical Model.

- ...:

  Arguments passed to or from other methods. Non-documented arguments
  are

  - `digits`, `p_digits`, `ci_digits` and `footer_digits` to set the
    number of digits for the output. `groups` can be used to group
    coefficients. These arguments will be passed to the print-method, or
    can directly be used in
    [`print()`](https://rdrr.io/r/base/print.html), see documentation in
    [`print.parameters_model()`](https://easystats.github.io/parameters/reference/print.parameters_model.html).

  - If `s_value = TRUE`, the p-value will be replaced by the S-value in
    the output (cf. *Rafi and Greenland 2020*).

  - `pd` adds an additional column with the *probability of direction*
    (see
    [`bayestestR::p_direction()`](https://easystats.github.io/bayestestR/reference/p_direction.html)
    for details). Furthermore, see 'Examples' in
    [`model_parameters.default()`](https://easystats.github.io/parameters/reference/model_parameters.default.html).

  - For developers, whose interest mainly is to get a "tidy" data frame
    of model summaries, it is recommended to set `pretty_names = FALSE`
    to speed up computation of the summary table.

## Citation

Patil, I., (2021). statsExpressions: R Package for Tidy Dataframes and
Expressions with Statistical Details. Journal of Open Source Software,
6(61), 3236, https://doi.org/10.21105/joss.03236

## Examples

``` r
model <- lm(mpg ~ wt + cyl, data = mtcars)
tidy_model_parameters(model)
#> # A tibble: 3 × 10
#>   term       estimate std.error conf.level conf.low conf.high statistic df.error
#>   <chr>         <dbl>     <dbl>      <dbl>    <dbl>     <dbl>     <dbl>    <int>
#> 1 (Intercep…    39.7      1.71        0.95    36.2     43.2       23.1        29
#> 2 wt            -3.19     0.757       0.95    -4.74    -1.64      -4.22       29
#> 3 cyl           -1.51     0.415       0.95    -2.36    -0.660     -3.64       29
#> # ℹ 2 more variables: p.value <dbl>, conf.method <chr>
```
