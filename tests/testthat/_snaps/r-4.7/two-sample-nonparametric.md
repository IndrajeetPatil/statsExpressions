# nonparametric works - within-subjects design

    Code
      select(df, -expression)
    Output
      # A tibble: 1 x 13
        parameter1 parameter2 statistic  p.value method                    alternative
        <chr>      <chr>          <dbl>    <dbl> <chr>                     <chr>      
      1 desire     condition       2846 0.000213 Wilcoxon signed rank test two.sided  
        effectsize        estimate conf.level conf.low conf.high conf.method n.obs
        <chr>                <dbl>      <dbl>    <dbl>     <dbl> <chr>       <int>
      1 r (rank biserial)    0.487       0.99    0.215     0.690 normal         90

---

    Code
      df[["expression"]]
    Output
      [[1]]
      list(italic("V")["Wilcoxon"] == "2846.00000", italic(p) == "0.00021", 
          widehat(italic("r"))["biserial"]^"rank" == "0.48737", CI["99%"] ~ 
              "[" * "0.21481", "0.68950" * "]", italic("n")["pairs"] == 
              "90")
      
