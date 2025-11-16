# Switch the type of statistics.

Relevant mostly for `{ggstatsplot}` and `{statsExpressions}` packages,
where different statistical approaches are supported via this argument:
parametric, non-parametric, robust, and Bayesian. This switch function
converts strings entered by users to a common pattern for convenience.

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

  You can specify just the initial letter.

## Examples

``` r
extract_stats_type("p")
#> [1] "parametric"
extract_stats_type("bf")
#> [1] "bayes"
```
