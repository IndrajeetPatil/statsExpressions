# contingency_table works

    Code
      select(df1, -expression)
    Output
      # A tibble: 1 x 12
        statistic    df p.value method                     effectsize        estimate
            <dbl> <int>   <dbl> <chr>                      <chr>                <dbl>
      1      8.74     2  0.0126 Pearson's Chi-squared test Cramer's V (adj.)    0.464
        conf.level conf.low conf.high conf.method conf.distribution n.obs
             <dbl>    <dbl>     <dbl> <chr>       <chr>             <int>
      1       0.99        0         1 ncp         chisq                32

---

    Code
      df1[["expression"]]
    Output
      [[1]]
      list(chi["Pearson"]^2 * "(" * 2 * ")" == "8.74073", italic(p) == 
          "0.01265", widehat(italic("V"))["Cramer"] == "0.46431", CI["99%"] ~ 
          "[" * "0.00000", "1.00000" * "]", italic("n")["obs"] == "32")
      

---

    Code
      select(df2, -expression)
    Output
      # A tibble: 1 x 12
        statistic    df   p.value method                     effectsize       
            <dbl> <int>     <dbl> <chr>                      <chr>            
      1      457.     1 2.30e-101 Pearson's Chi-squared test Cramer's V (adj.)
        estimate conf.level conf.low conf.high conf.method conf.distribution n.obs
           <dbl>      <dbl>    <dbl>     <dbl> <chr>       <chr>             <int>
      1    0.455       0.95    0.420         1 ncp         chisq              2201

---

    Code
      df2[["expression"]]
    Output
      [[1]]
      list(chi["Pearson"]^2 * "(" * 1 * ")" == "456.87", italic(p) == 
          "2.30e-101", widehat(italic("V"))["Cramer"] == "0.46", CI["95%"] ~ 
          "[" * "0.42", "1.00" * "]", italic("n")["obs"] == "2,201")
      

---

    Code
      select(df3, -expression)
    Output
      # A tibble: 1 x 12
        statistic    df p.value method                     effectsize        estimate
            <dbl> <int>   <dbl> <chr>                      <chr>                <dbl>
      1      15.8    15   0.399 Pearson's Chi-squared test Cramer's V (adj.)   0.0558
        conf.level conf.low conf.high conf.method conf.distribution n.obs
             <dbl>    <dbl>     <dbl> <chr>       <chr>             <int>
      1       0.99        0         1 ncp         chisq                52

---

    Code
      df3[["expression"]]
    Output
      [[1]]
      list(chi["Pearson"]^2 * "(" * 15 * ")" == "15.75", italic(p) == 
          "0.40", widehat(italic("V"))["Cramer"] == "0.06", CI["99%"] ~ 
          "[" * "0.00", "1.00" * "]", italic("n")["obs"] == "52")
      

# paired contingency_table works 

    Code
      select(df1, -expression)
    Output
      # A tibble: 1 x 11
        statistic    df  p.value method                     effectsize estimate
            <dbl> <dbl>    <dbl> <chr>                      <chr>         <dbl>
      1      13.3     1 0.000261 McNemar's Chi-squared test Cohen's g     0.333
        conf.level conf.low conf.high conf.method n.obs
             <dbl>    <dbl>     <dbl> <chr>       <int>
      1       0.95    0.164     0.427 binomial      100

---

    Code
      df1[["expression"]]
    Output
      [[1]]
      list(chi["McNemar"]^2 * "(" * 1 * ")" == "13.33333", italic(p) == 
          "0.00026", widehat(italic("g"))["Cohen"] == "0.33333", CI["95%"] ~ 
          "[" * "0.16436", "0.42663" * "]", italic("n")["pairs"] == 
          "100")
      

---

    Code
      select(df2, -expression)
    Output
      # A tibble: 1 x 11
        statistic    df  p.value method                     effectsize estimate
            <dbl> <dbl>    <dbl> <chr>                      <chr>         <dbl>
      1      13.3     1 0.000261 McNemar's Chi-squared test Cohen's g     0.333
        conf.level conf.low conf.high conf.method n.obs
             <dbl>    <dbl>     <dbl> <chr>       <int>
      1        0.9    0.195     0.416 binomial       95

---

    Code
      df2[["expression"]]
    Output
      [[1]]
      list(chi["McNemar"]^2 * "(" * 1 * ")" == "13.333", italic(p) == 
          "2.607e-04", widehat(italic("g"))["Cohen"] == "0.333", CI["90%"] ~ 
          "[" * "0.195", "0.416" * "]", italic("n")["pairs"] == "95")
      

# Goodness of Fit contingency_table works without counts

    Code
      select(df1, -expression)
    Output
      # A tibble: 1 x 12
        statistic    df p.value method                                   effectsize 
            <dbl> <dbl>   <dbl> <chr>                                    <chr>      
      1      1.12     1   0.289 Chi-squared test for given probabilities Pearson's C
        estimate conf.level conf.low conf.high conf.method conf.distribution n.obs
           <dbl>      <dbl>    <dbl>     <dbl> <chr>       <chr>             <int>
      1    0.184       0.99        0         1 ncp         chisq                32

---

    Code
      df1[["expression"]]
    Output
      [[1]]
      list(chi["gof"]^2 * "(" * 1 * ")" == "1.12500", italic(p) == 
          "0.28884", widehat(italic("C"))["Pearson"] == "0.18429", 
          CI["99%"] ~ "[" * "0.00000", "1.00000" * "]", italic("n")["obs"] == 
              "32")
      

---

    Code
      select(df2, -expression)
    Output
      # A tibble: 1 x 12
        statistic    df   p.value method                                   effectsize 
            <dbl> <dbl>     <dbl> <chr>                                    <chr>      
      1      722.     1 3.92e-159 Chi-squared test for given probabilities Pearson's C
        estimate conf.level conf.low conf.high conf.method conf.distribution n.obs
           <dbl>      <dbl>    <dbl>     <dbl> <chr>       <chr>             <int>
      1    0.497       0.95    0.474         1 ncp         chisq              2201

---

    Code
      df2[["expression"]]
    Output
      [[1]]
      list(chi["gof"]^2 * "(" * 1 * ")" == "722.45", italic(p) == "3.92e-159", 
          widehat(italic("C"))["Pearson"] == "0.50", CI["95%"] ~ "[" * 
              "0.47", "1.00" * "]", italic("n")["obs"] == "2,201")
      

---

    Code
      select(df3, -expression)
    Output
      # A tibble: 1 x 12
        statistic    df     p.value method                                  
            <dbl> <dbl>       <dbl> <chr>                                   
      1      33.8     3 0.000000223 Chi-squared test for given probabilities
        effectsize  estimate conf.level conf.low conf.high conf.method
        <chr>          <dbl>      <dbl>    <dbl>     <dbl> <chr>      
      1 Pearson's C    0.555       0.95    0.413         1 ncp        
        conf.distribution n.obs
        <chr>             <int>
      1 chisq                76

---

    Code
      df3[["expression"]]
    Output
      [[1]]
      list(chi["gof"]^2 * "(" * 3 * ")" == "33.76", italic(p) == "2.23e-07", 
          widehat(italic("C"))["Pearson"] == "0.55", CI["95%"] ~ "[" * 
              "0.41", "1.00" * "]", italic("n")["obs"] == "76")
      

# bayesian (proportion test)

    Code
      select(df1, -expression)
    Output
      # A tibble: 1 x 3
         bf10 prior.scale method                                     
        <dbl>       <dbl> <chr>                                      
      1 0.247           1 Bayesian one-way contingency table analysis

---

    Code
      df1[["expression"]]
    Output
      [[1]]
      list(log[e] * (BF["01"]) == "1.40", italic("a")["Gunel-Dickey"] == 
          "1.00")
      

---

    Code
      select(df2, -expression)
    Output
      # A tibble: 1 x 3
         bf10 prior.scale method                                     
        <dbl>       <dbl> <chr>                                      
      1 0.579          10 Bayesian one-way contingency table analysis

---

    Code
      df2[["expression"]]
    Output
      [[1]]
      list(log[e] * (BF["01"]) == "0.55", italic("a")["Gunel-Dickey"] == 
          "10.00")
      

# bayesian (contingency tab)

    Code
      list(expr_text1[["expression"]], expr_text2[["expression"]], expr_text3[[
        "expression"]])
    Output
      [[1]]
      [[1]][[1]]
      list(log[e] * (BF["01"]) == "-3.335", widehat(italic("V"))["Cramer"]^"posterior" == 
          "0.479", CI["89%"]^ETI ~ "[" * "0.253", "0.668" * "]", italic("a")["Gunel-Dickey"] == 
          "1.000")
      
      
      [[2]]
      [[2]][[1]]
      list(log[e] * (BF["01"]) == "-214.255", widehat(italic("V"))["Cramer"]^"posterior" == 
          "0.455", CI["99%"]^ETI ~ "[" * "0.401", "0.507" * "]", italic("a")["Gunel-Dickey"] == 
          "1.000")
      
      
      [[3]]
      [[3]][[1]]
      list(log[e] * (BF["01"]) == "-213.873", widehat(italic("V"))["Cramer"]^"posterior" == 
          "0.454", CI["95%"]^ETI ~ "[" * "0.415", "0.493" * "]", italic("a")["Gunel-Dickey"] == 
          "1.500")
      
      

