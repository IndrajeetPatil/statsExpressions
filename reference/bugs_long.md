# Tidy version of the "Bugs" dataset.

Tidy version of the "Bugs" dataset.

## Usage

``` r
bugs_long
```

## Format

A data frame with 372 rows and 6 variables

- subject. Dummy identity number for each participant.

- gender. Participant's gender (Female, Male).

- region. Region of the world the participant was from.

- education. Level of education.

- condition. Condition of the experiment the participant gave rating for
  (**LDLF**: low disgustingness and low frighteningness; **LDHF**: low
  disgustingness and high frighteningness; **HDLF**: high disgustingness
  and low frighteningness; **HDHF**: high disgustingness and high
  frighteningness).

- desire. The desire to kill an arthropod was indicated on a scale from
  0 to 10.

## Details

This data set, "Bugs", provides the extent to which men and women want
to kill arthropods that vary in frighteningness (low, high) and
disgustingness (low, high). Each participant rates their attitudes
towards all arthropods. Subset of the data reported by Ryan et al.
(2013).

## References

Ryan, R. S., Wilde, M., & Crist, S. (2013). Compared to a small,
supervised lab experiment, a large, unsupervised web-based experiment on
a previously unknown effect has benefits that outweigh its potential
costs. *Computers in Human Behavior*, *29*(4), 1295-1301.

## Examples

``` r
dplyr::glimpse(bugs_long)
#> Rows: 372
#> Columns: 6
#> $ subject   <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 1…
#> $ gender    <fct> Female, Female, Female, Female, Female, Female, Female, Fema…
#> $ region    <fct> North America, North America, Europe, North America, North A…
#> $ education <fct> some, advance, college, college, some, some, some, high, hig…
#> $ condition <chr> "LDLF", "LDLF", "LDLF", "LDLF", "LDLF", "LDLF", "LDLF", "LDL…
#> $ desire    <dbl> 6.0, 10.0, 5.0, 6.0, 3.0, 2.0, 10.0, 10.0, 9.5, 8.5, 0.0, 9.…
```
