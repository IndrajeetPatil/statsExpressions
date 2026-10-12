# within-subjects effect size excludes the row id column

    Code
      select(df, -expression)
    Output
      # A tibble: 1 x 11
        statistic    df df.error p.value
            <dbl> <dbl>    <dbl>   <dbl>
      1      3.26  1.61     20.9  0.0676
        method                                                             
        <chr>                                                              
      1 A heteroscedastic one-way repeated measures ANOVA for trimmed means
        effectsize                                                      estimate
        <chr>                                                              <dbl>
      1 Algina-Keselman-Penfield robust standardized difference average    0.595
        conf.level conf.low conf.high n.obs
             <dbl>    <dbl>     <dbl> <int>
      1       0.95   0.0982      2.04    22

---

    Code
      df[["expression"]]
    Output
      [[1]]
      list(italic("F")["trimmed-means"](1.61, 20.92) == "3.26", italic(p) == 
          "0.07", widehat(delta)["R-avg"]^"AKP" == "0.60", CI["95%"] ~ 
          "[" * "0.10", "2.04" * "]", italic("n")["pairs"] == "22")
      

