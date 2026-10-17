# Movie information and user ratings from IMDB.

Movie information and user ratings from IMDB.

## Usage

``` r
movies_long
```

## Format

A data frame with 1,579 rows and 8 variables

- title. Title of the movie.

- year. Year of release.

- budget. Total budget (if known) in US dollars.

- length. Length in minutes.

- rating. Average IMDB user rating.

- votes. Number of IMDB users who rated this movie.

- mpaa. MPAA rating.

- genre. Genre of the movie (`"Action"`, `"Action Comedy"`,
  `"Action Drama"`, `"Animated"`, `"Comedy"`, `"Comedy Drama"`,
  `"Drama"`, `"Romance Drama"`, `"RomCom"`).

## Source

<https://CRAN.R-project.org/package=ggplot2movies>

## Details

Modified dataset from `{ggplot2movies}` package.

## Examples

``` r
dplyr::glimpse(movies_long)
#> Rows: 1,579
#> Columns: 8
#> $ title  <chr> "Shawshank Redemption, The", "Lord of the Rings: The Return of …
#> $ year   <int> 1994, 2003, 2001, 2002, 1994, 1993, 1977, 1980, 1968, 2002, 196…
#> $ length <int> 142, 251, 208, 223, 168, 195, 125, 129, 158, 135, 93, 113, 108,…
#> $ budget <dbl> 25.0, 94.0, 93.0, 94.0, 8.0, 25.0, 11.0, 18.0, 5.0, 3.3, 1.8, 5…
#> $ rating <dbl> 9.1, 9.0, 8.8, 8.8, 8.8, 8.8, 8.8, 8.8, 8.7, 8.7, 8.7, 8.7, 8.6…
#> $ votes  <int> 149494, 103631, 157608, 114797, 132745, 97667, 134640, 103706, …
#> $ mpaa   <fct> R, PG-13, PG-13, PG-13, R, R, PG, PG, PG-13, R, PG, R, R, R, R,…
#> $ genre  <fct> Drama, Action, Action, Action, Drama, Drama, Action, Action, Dr…
```
