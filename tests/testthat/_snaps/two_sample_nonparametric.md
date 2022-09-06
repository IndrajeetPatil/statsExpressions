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
      

# t_nonparametric works - within-subjects design

    Code
      select(df2, -expression)
    Output
      # A tibble: 1 x 13
        parameter1 parameter2 statistic p.value method                    alternative
        <chr>      <chr>          <dbl>   <dbl> <chr>                     <chr>      
      1 length     type              10 0.00295 Wilcoxon signed rank test two.sided  
        effectsize        estimate conf.level conf.low conf.high conf.method n.obs
        <chr>                <dbl>      <dbl>    <dbl>     <dbl> <chr>       <int>
      1 r (rank biserial)   -0.853       0.99   -0.964    -0.489 normal         16

---

    Code
      df2[["expression"]]
    Output
      [[1]]
      list(italic("V")["Wilcoxon"] == "10.00000", italic(p) == "0.00295", 
          widehat(italic("r"))["biserial"]^"rank" == "-0.85294", CI["99%"] ~ 
              "[" * "-0.96399", "-0.48865" * "]", italic("n")["pairs"] == 
              "16")
      
