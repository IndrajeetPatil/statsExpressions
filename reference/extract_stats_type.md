# Switch the type of statistics.

Relevant mostly for `{ggstatsplot}` and `{statsExpressions}` packages,
where different statistical approaches are supported via this argument:
parametric, non-parametric, robust, and Bayesian. This switch function
converts strings entered by users to a common pattern for convenience.

`stats_type_switch()` is an alias of `extract_stats_type()`.

## Usage

``` r
extract_stats_type(type)

stats_type_switch(type)
```

## Arguments

- type:

  A character specifying the type of statistical approach:

  - `"parametric"`

  - `"nonparametric"`

  - `"robust"`

  - `"bayes"`

  You can specify just the initial letter (e.g. `"np"` or `"bf"`).
  Matching is on the initial lowercase letter only, so any other value
  (including capitalized values such as `"Bayes"`) falls back to
  `"parametric"` without a warning.

## Value

A character vector of the same length as `type`, with values
`"parametric"`, `"nonparametric"`, `"robust"`, or `"bayes"`.

## Examples

``` r
extract_stats_type("p")
#> [1] "parametric"
extract_stats_type("bf")
#> [1] "bayes"
extract_stats_type(c("np", "robust", "Bayes"))
#> [1] "nonparametric" "robust"        "parametric"   
```
