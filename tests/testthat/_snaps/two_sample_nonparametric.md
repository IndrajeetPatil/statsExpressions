# t_nonparametric works - between-subjects design

    Code
      select(df, -expression)
    Output
      # A tibble: 1 x 13
        parameter1 parameter2 statistic   p.value method                 alternative
        <chr>      <chr>          <dbl>     <dbl> <chr>                  <chr>      
      1 wt         am              230. 0.0000435 Wilcoxon rank sum test two.sided  
        effectsize        estimate conf.level conf.low conf.high conf.method n.obs
        <chr>                <dbl>      <dbl>    <dbl>     <dbl> <chr>       <int>
      1 r (rank biserial)    0.866        0.9    0.749     0.931 normal         32

---

    Code
      df[["expression"]]
    Output
      [[1]]
      list(italic("W")["Mann-Whitney"] == "230.500", italic(p) == "4.347e-05", 
          widehat(italic("r"))["biserial"]^"rank" == "0.866", CI["90%"] ~ 
              "[" * "0.749", "0.931" * "]", italic("n")["obs"] == "32")
      

# nonparametric works - within-subjects design

    Code
      select(df, -expression)
    Output
      # A tibble: 1 x 13
        parameter1 parameter2 statistic  p.value method                    alternative
        <chr>      <chr>          <dbl>    <dbl> <chr>                     <chr>      
      1 desire     condition       1796 0.000430 Wilcoxon signed rank test two.sided  
        effectsize        estimate conf.level conf.low conf.high conf.method n.obs
        <chr>                <dbl>      <dbl>    <dbl>     <dbl> <chr>       <int>
      1 r (rank biserial)    0.487       0.99    0.215     0.690 normal         90

---

    Code
      df[["expression"]]
    Output
      [[1]]
      list(italic("V")["Wilcoxon"] == "1796.00000", italic(p) == "0.00043", 
          widehat(italic("r"))["biserial"]^"rank" == "0.48737", CI["99%"] ~ 
              "[" * "0.21481", "0.68950" * "]", italic("n")["pairs"] == 
              "90")
      

