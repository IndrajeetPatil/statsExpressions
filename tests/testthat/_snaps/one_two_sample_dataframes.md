#  dataframes for parametric t-tests

    Code
      df_1
    Output
      # A tibble: 4 x 11
           mu statistic df.error p.value method estimate conf.level conf.low conf.high
        <dbl>     <dbl>    <dbl>   <dbl> <chr>     <dbl>      <dbl>    <dbl>     <dbl>
      1  0.25     0.242       55   0.810 One S~   0.0323       0.89  -0.183      0.248
      2  0.25     0.242       55   0.810 One S~   0.0319       0.99  -0.311      0.375
      3  0.25     0.242       55   0.810 One S~   0.0323       0.9   -0.189      0.254
      4  0.25     0.242       55   0.810 One S~   0.0319       0.5   -0.0577     0.122
      # ... with 2 more variables: effectsize <chr>, expression <list>

---

    Code
      df_2_between
    Output
      # A tibble: 4 x 14
        term  group mean.group1 mean.group2 statistic df.error p.value method estimate
        <chr> <chr>       <dbl>       <dbl>     <dbl>    <dbl>   <dbl> <chr>     <dbl>
      1 wt    am           3.77        2.41      5.26     30   1.13e-5 Two S~     1.89
      2 wt    am           3.77        2.41      5.49     29.2 6.27e-6 Welch~     1.84
      3 wt    am           3.77        2.41      5.26     30   1.13e-5 Two S~     1.89
      4 wt    am           3.77        2.41      5.49     29.2 6.27e-6 Welch~     1.84
      # ... with 5 more variables: conf.level <dbl>, conf.low <dbl>, conf.high <dbl>,
      #   effectsize <chr>, expression <list>

---

    Code
      df_2_within
    Output
      # A tibble: 4 x 12
        term   group  statistic df.error p.value method   estimate conf.level conf.low
        <chr>  <chr>      <dbl>    <dbl>   <dbl> <chr>       <dbl>      <dbl>    <dbl>
      1 desire condi~      3.61       89 5.00e-4 Paired ~    0.381       0.89   0.206 
      2 desire condi~      3.61       89 5.00e-4 Paired ~    0.378       0.99   0.0984
      3 desire condi~      3.61       89 5.00e-4 Paired ~    0.381       0.9    0.201 
      4 desire condi~      3.61       89 5.00e-4 Paired ~    0.378       0.5    0.305 
      # ... with 3 more variables: conf.high <dbl>, effectsize <chr>,
      #   expression <list>

