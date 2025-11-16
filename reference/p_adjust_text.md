# *p*-value adjustment method text

Preparing text to describe which *p*-value adjustment method was used

## Usage

``` r
p_adjust_text(p.adjust.method)
```

## Arguments

- p.adjust.method:

  Adjustment method for *p*-values for multiple comparisons. Possible
  methods are: `"holm"` (default), `"hochberg"`, `"hommel"`,
  `"bonferroni"`, `"BH"`, `"BY"`, `"fdr"`, `"none"`.

## Value

Standardized text description for what method was used.

## Examples

``` r
p_adjust_text("none")
#> [1] "None"
p_adjust_text("BY")
#> [1] "BY"
```
