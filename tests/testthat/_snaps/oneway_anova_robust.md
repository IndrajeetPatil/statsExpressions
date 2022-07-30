# expr_anova_robust works - between-subjects

    Code
      select(df1, -expression)
    Output
      # A tibble: 1 x 11
        statistic    df df.error   p.value
            <dbl> <dbl>    <dbl>     <dbl>
      1      20.2     2     19.0 0.0000196
        method                                           
        <chr>                                            
      1 A heteroscedastic one-way ANOVA for trimmed means
        effectsize                         estimate conf.level conf.low conf.high
        <chr>                                 <dbl>      <dbl>    <dbl>     <dbl>
      1 Explanatory measure of effect size    0.859       0.95    0.853     0.864
        n.obs
        <int>
      1    32

---

    Code
      df1[["expression"]]
    Output
      [[1]]
      list(italic("F")["trimmed-means"](2, 18.97383) == "20.24946", 
          italic(p) == "0.00002", widehat(xi) == "0.85858", CI["95%"] ~ 
              "[" * "0.85268", "0.86448" * "]", italic("n")["obs"] == 
              "32")
      

---

    Code
      select(df2, -expression)
    Output
      # A tibble: 1 x 11
        statistic    df df.error p.value
            <dbl> <dbl>    <dbl>   <dbl>
      1    0.0503     2     21.7   0.951
        method                                           
        <chr>                                            
      1 A heteroscedastic one-way ANOVA for trimmed means
        effectsize                         estimate conf.level conf.low conf.high
        <chr>                                 <dbl>      <dbl>    <dbl>     <dbl>
      1 Explanatory measure of effect size    0.201       0.99   0.0872     0.754
        n.obs
        <int>
      1    71

---

    Code
      df2[["expression"]]
    Output
      [[1]]
      list(italic("F")["trimmed-means"](2, 21.6869) == "0.0503", italic(p) == 
          "0.9511", widehat(xi) == "0.2013", CI["99%"] ~ "[" * "0.0872", 
          "0.7537" * "]", italic("n")["obs"] == "71")
      

# expr_anova_robust works - within-subjects

    Code
      select(df1, -expression)
    Output
      # A tibble: 1 x 11
        statistic    df df.error  p.value
            <dbl> <dbl>    <dbl>    <dbl>
      1      21.0  2.73     145. 1.15e-10
        method                                                             
        <chr>                                                              
      1 A heteroscedastic one-way repeated measures ANOVA for trimmed means
        effectsize                                                      estimate
        <chr>                                                              <dbl>
      1 Algina-Keselman-Penfield robust standardized difference average    0.664
        conf.level conf.low conf.high n.obs
             <dbl>    <dbl>     <dbl> <int>
      1       0.95    0.466     0.971    88

---

    Code
      df1[["expression"]]
    Output
      [[1]]
      list(italic("F")["trimmed-means"](2.7303, 144.7051) == "20.9752", 
          italic(p) == "1.1462e-10", widehat(delta)["R-avg"]^"AKP" == 
              "0.6635", CI["95%"] ~ "[" * "0.4660", "0.9707" * "]", 
          italic("n")["pairs"] == "88")
      

