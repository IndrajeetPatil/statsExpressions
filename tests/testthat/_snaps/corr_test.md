# corr_test works - parametric

    Code
      select(df1, -expression)
    Output
      # A tibble: 1 x 13
        parameter1 parameter2 effectsize          estimate conf.level conf.low
        <chr>      <chr>      <chr>                  <dbl>      <dbl>    <dbl>
      1 brainwt    sleep_rem  Pearson correlation   -0.221        0.9   -0.438
        conf.high statistic df.error p.value method              n.obs conf.method
            <dbl>     <dbl>    <int>   <dbl> <chr>               <int> <chr>      
      1    0.0201     -1.54       46   0.131 Pearson correlation    48 normal     

---

    Code
      df1[["expression"]]
    Output
      [[1]]
      list(italic("t")["Student"] * "(" * 46 * ")" == "-1.539", italic(p) == 
          "0.131", widehat(italic("r"))["Pearson"] == "-0.221", CI["90%"] ~ 
          "[" * "-0.438", "0.020" * "]", italic("n")["pairs"] == "48")
      

---

    Code
      select(df2, -expression)
    Output
      # A tibble: 1 x 13
        parameter1 parameter2 effectsize          estimate conf.level conf.low
        <chr>      <chr>      <chr>                  <dbl>      <dbl>    <dbl>
      1 wt         mpg        Pearson correlation   -0.868       0.95   -0.934
        conf.high statistic df.error  p.value method              n.obs conf.method
            <dbl>     <dbl>    <int>    <dbl> <chr>               <int> <chr>      
      1    -0.744     -9.56       30 1.29e-10 Pearson correlation    32 normal     

---

    Code
      df2[["expression"]]
    Output
      [[1]]
      list(italic("t")["Student"] * "(" * 30 * ")" == "-9.559", italic(p) == 
          "1.294e-10", widehat(italic("r"))["Pearson"] == "-0.868", 
          CI["95%"] ~ "[" * "-0.934", "-0.744" * "]", italic("n")["pairs"] == 
              "32")
      

# corr_test works - robust

    Code
      select(df1, -expression)
    Output
      # A tibble: 1 x 13
        parameter1 parameter2  effectsize                     estimate conf.level
        <chr>      <chr>       <chr>                             <dbl>      <dbl>
      1 brainwt    sleep_total Winsorized Pearson correlation   -0.549        0.5
        conf.low conf.high statistic df.error   p.value method                        
           <dbl>     <dbl>     <dbl>    <int>     <dbl> <chr>                         
      1   -0.611    -0.481     -4.83       54 0.0000117 Winsorized Pearson correlation
        n.obs conf.method
        <int> <chr>      
      1    56 normal     

---

    Code
      df1[["expression"]]
    Output
      [[1]]
      list(italic("t")["Student"] * "(" * 54 * ")" == "-4.8286", italic(p) == 
          "1.1723e-05", widehat(italic("r"))["Winsorized"] == "-0.5491", 
          CI["50%"] ~ "[" * "-0.6106", "-0.4812" * "]", italic("n")["pairs"] == 
              "56")
      

---

    Code
      select(df2, -expression)
    Output
      # A tibble: 1 x 13
        parameter1 parameter2 effectsize                     estimate conf.level
        <chr>      <chr>      <chr>                             <dbl>      <dbl>
      1 wt         mpg        Winsorized Pearson correlation   -0.864       0.95
        conf.low conf.high statistic df.error  p.value method                        
           <dbl>     <dbl>     <dbl>    <int>    <dbl> <chr>                         
      1   -0.932    -0.738     -9.41       30 1.84e-10 Winsorized Pearson correlation
        n.obs conf.method
        <int> <chr>      
      1    32 normal     

---

    Code
      df2[["expression"]]
    Output
      [[1]]
      list(italic("t")["Student"] * "(" * 30 * ")" == "-9.41", italic(p) == 
          "1.84e-10", widehat(italic("r"))["Winsorized"] == "-0.86", 
          CI["95%"] ~ "[" * "-0.93", "-0.74" * "]", italic("n")["pairs"] == 
              "32")
      

# corr_test works - nonparametric

    Code
      select(df1, -expression)
    Output
      # A tibble: 1 x 12
        parameter1 parameter2  effectsize           estimate conf.level conf.low
        <chr>      <chr>       <chr>                   <dbl>      <dbl>    <dbl>
      1 brainwt    sleep_total Spearman correlation   -0.594        0.5   -0.652
        conf.high statistic    p.value method               n.obs conf.method
            <dbl>     <dbl>      <dbl> <chr>                <int> <chr>      
      1    -0.528    46627. 0.00000143 Spearman correlation    56 normal     

---

    Code
      df1[["expression"]]
    Output
      [[1]]
      list(italic("S") == "46627.1234", italic(p) == "1.4262e-06", 
          widehat(rho)["Spearman"] == "-0.5935", CI["50%"] ~ "[" * 
              "-0.6518", "-0.5283" * "]", italic("n")["pairs"] == "56")
      

---

    Code
      select(df2, -expression)
    Output
      # A tibble: 1 x 12
        parameter1 parameter2 effectsize           estimate conf.level conf.low
        <chr>      <chr>      <chr>                   <dbl>      <dbl>    <dbl>
      1 wt         mpg        Spearman correlation   -0.886       0.95   -0.945
        conf.high statistic  p.value method               n.obs conf.method
            <dbl>     <dbl>    <dbl> <chr>                <int> <chr>      
      1    -0.774    10292. 1.49e-11 Spearman correlation    32 normal     

---

    Code
      df2[["expression"]]
    Output
      [[1]]
      list(italic("S") == "10292.3186", italic(p) == "1.4876e-11", 
          widehat(rho)["Spearman"] == "-0.8864", CI["95%"] ~ "[" * 
              "-0.9447", "-0.7740" * "]", italic("n")["pairs"] == "32")
      

# corr_test works - Bayesian

    Code
      df1[["expression"]]
    Output
      [[1]]
      list(log[e] * (BF["01"]) == "0.49", widehat(rho)["Pearson"]^"posterior" == 
          "-0.21", CI["99%"]^HDI ~ "[" * "-0.47", "0.05" * "]", italic("r")["beta"]^"JZS" == 
          "1.25")
      

---

    Code
      df2[["expression"]]
    Output
      [[1]]
      list(log[e] * (BF["01"]) == "-17.84", widehat(rho)["Pearson"]^"posterior" == 
          "-0.84", CI["95%"]^HDI ~ "[" * "-0.93", "-0.73" * "]", italic("r")["beta"]^"JZS" == 
          "1.41")
      
