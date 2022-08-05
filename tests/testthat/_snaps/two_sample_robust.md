# t_robust - within-subjects - without NAs

    Code
      select(df1, -expression)
    Output
      # A tibble: 1 x 14
        statistic df.error p.value method                                            
            <dbl>    <dbl>   <dbl> <chr>                                             
      1      28.7       89       0 Yuen's test on trimmed means for dependent samples
        effectsize                                              estimate conf.level
        <chr>                                                      <dbl>      <dbl>
      1 Algina-Keselman-Penfield robust standardized difference     2.36       0.95
        conf.low conf.high    mu small medium large n.obs
           <dbl>     <dbl> <dbl> <dbl>  <dbl> <dbl> <int>
      1     1.96      2.61     0   0.1    0.3   0.5   150

---

    Code
      df1[["expression"]]
    Output
      [[1]]
      list(italic("t")["Yuen"] * "(" * 89 * ")" == "28.7230", italic(p) == 
          "0.0000", widehat(delta)["R"]^"AKP" == "2.3582", CI["95%"] ~ 
          "[" * "1.9615", "2.6081" * "]", italic("n")["pairs"] == "150")
      

# t_robust - within-subjects - with NAs

    Code
      select(df1, -expression)
    Output
      # A tibble: 1 x 14
        statistic df.error p.value method                                            
            <dbl>    <dbl>   <dbl> <chr>                                             
      1      2.91       53 0.00528 Yuen's test on trimmed means for dependent samples
        effectsize                                              estimate conf.level
        <chr>                                                      <dbl>      <dbl>
      1 Algina-Keselman-Penfield robust standardized difference    0.410       0.95
        conf.low conf.high    mu small medium large n.obs
           <dbl>     <dbl> <dbl> <dbl>  <dbl> <dbl> <int>
      1    0.238     0.611     0   0.1    0.3   0.5    90

---

    Code
      df1[["expression"]]
    Output
      [[1]]
      list(italic("t")["Yuen"] * "(" * 53 * ")" == "2.909", italic(p) == 
          "0.005", widehat(delta)["R"]^"AKP" == "0.410", CI["95%"] ~ 
          "[" * "0.238", "0.611" * "]", italic("n")["pairs"] == "90")
      

# t_robust - between-subjects - without NAs

    Code
      select(df1, -expression)
    Output
      # A tibble: 1 x 10
        statistic df.error   p.value
            <dbl>    <dbl>     <dbl>
      1      5.84     13.6 0.0000485
        method                                              
        <chr>                                               
      1 Yuen's test on trimmed means for independent samples
        effectsize                                              estimate conf.level
        <chr>                                                      <dbl>      <dbl>
      1 Algina-Keselman-Penfield robust standardized difference     2.48       0.99
        conf.low conf.high n.obs
           <dbl>     <dbl> <int>
      1    0.738      5.13    32

---

    Code
      df1[["expression"]]
    Output
      [[1]]
      list(italic("t")["Yuen"] * "(" * 13.584 * ")" == "5.840", italic(p) == 
          "4.846e-05", widehat(delta)["R"]^"AKP" == "2.482", CI["99%"] ~ 
          "[" * "0.738", "5.128" * "]", italic("n")["obs"] == "32")
      

# t_robust - between-subjects - with NAs

    Code
      select(df1, -expression)
    Output
      # A tibble: 1 x 10
        statistic df.error p.value
            <dbl>    <dbl>   <dbl>
      1     0.452     13.8   0.658
        method                                              
        <chr>                                               
      1 Yuen's test on trimmed means for independent samples
        effectsize                                              estimate conf.level
        <chr>                                                      <dbl>      <dbl>
      1 Algina-Keselman-Penfield robust standardized difference   -0.358        0.9
        conf.low conf.high n.obs
           <dbl>     <dbl> <int>
      1    -7.16     0.406    29

---

    Code
      df1[["expression"]]
    Output
      [[1]]
      list(italic("t")["Yuen"] * "(" * 13.8476 * ")" == "0.4521", italic(p) == 
          "0.6582", widehat(delta)["R"]^"AKP" == "-0.3583", CI["90%"] ~ 
          "[" * "-7.1637", "0.4061" * "]", italic("n")["obs"] == "29")
      

