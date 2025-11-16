# Convert long/tidy data frame to wide format

This conversion is helpful mostly for repeated measures design, where
removing `NA`s by participant can be a bit tedious.

## Usage

``` r
long_to_wide_converter(
  data,
  x,
  y,
  subject.id = NULL,
  paired = TRUE,
  spread = TRUE,
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

- paired:

  Logical that decides whether the experimental design is repeated
  measures/within-subjects or between-subjects. The default is `FALSE`.

- spread:

  Logical that decides whether the data frame needs to be converted from
  long/tidy to wide (default: `TRUE`).

- ...:

  Currently ignored.

## Value

A data frame with `NA`s removed while respecting the
between-or-within-subjects nature of the dataset.

## Citation

Patil, I., (2021). statsExpressions: R Package for Tidy Dataframes and
Expressions with Statistical Details. Journal of Open Source Software,
6(61), 3236, https://doi.org/10.21105/joss.03236

## Examples

``` r
# for reproducibility
library(statsExpressions)
set.seed(123)

# repeated measures design
long_to_wide_converter(
  bugs_long,
  condition,
  desire,
  subject.id = subject,
  paired = TRUE
)
#> # A tibble: 88 × 5
#>    .rowid  HDHF  HDLF  LDHF  LDLF
#>     <int> <dbl> <dbl> <dbl> <dbl>
#>  1      1  10     9     6     6  
#>  2      3  10    10    10     5  
#>  3      4   9     6     9     6  
#>  4      5   8.5   5.5   6.5   3  
#>  5      6   3     7.5   0.5   2  
#>  6      7  10    10    10    10  
#>  7      8  10     9    10    10  
#>  8      9  10     6     9.5   9.5
#>  9     11   0     0     2.5   0  
#> 10     12  10     8.5   7.5   9.5
#> # ℹ 78 more rows

# independent measures design
long_to_wide_converter(mtcars, cyl, wt, paired = FALSE)
#> # A tibble: 32 × 4
#>    .rowid   `4`   `6`   `8`
#>     <int> <dbl> <dbl> <dbl>
#>  1      1  2.32    NA    NA
#>  2      2  3.19    NA    NA
#>  3      3  3.15    NA    NA
#>  4      4  2.2     NA    NA
#>  5      5  1.62    NA    NA
#>  6      6  1.84    NA    NA
#>  7      7  2.46    NA    NA
#>  8      8  1.94    NA    NA
#>  9      9  2.14    NA    NA
#> 10     10  1.51    NA    NA
#> # ℹ 22 more rows
```
