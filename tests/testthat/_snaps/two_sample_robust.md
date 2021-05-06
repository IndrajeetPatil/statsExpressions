# t_robust - within-subjects - without NAs

    Code
      dplyr::select(df1, -expression)
    Output
      # A tibble: 1 x 9
        statistic df.error p.value method                                            
            <dbl>    <dbl>   <dbl> <chr>                                             
      1      28.7       89       0 Yuen's test on trimmed means for dependent samples
        estimate conf.low conf.high conf.level
           <dbl>    <dbl>     <dbl>      <dbl>
      1     2.36     1.96      2.61       0.95
        effectsize                                             
        <chr>                                                  
      1 Algina-Keselman-Penfield robust standardized difference

---

    Code
      df1$expression[[1]]
    Output
      paste(italic("t")["Yuen"], "(", "89", ") = ", "28.7230", ", ", 
          italic("p"), " = ", "0e+00", ", ", widehat(delta)["R"]^"AKP", 
          " = ", "2.3582", ", CI"["95%"], " [", "1.9615", ", ", "2.6081", 
          "]", ", ", italic("n")["pairs"], " = ", "150")

# t_robust - within-subjects - with NAs

    Code
      dplyr::select(df1, -expression)
    Output
      # A tibble: 1 x 9
        statistic df.error p.value method                                            
            <dbl>    <dbl>   <dbl> <chr>                                             
      1      2.91       53 0.00528 Yuen's test on trimmed means for dependent samples
        estimate conf.low conf.high conf.level
           <dbl>    <dbl>     <dbl>      <dbl>
      1    0.410    0.238     0.611       0.95
        effectsize                                             
        <chr>                                                  
      1 Algina-Keselman-Penfield robust standardized difference

---

    Code
      df1$expression[[1]]
    Output
      paste(italic("t")["Yuen"], "(", "53", ") = ", "2.909", ", ", 
          italic("p"), " = ", "0.005", ", ", widehat(delta)["R"]^"AKP", 
          " = ", "0.410", ", CI"["95%"], " [", "0.238", ", ", "0.611", 
          "]", ", ", italic("n")["pairs"], " = ", "90")

# t_robust - between-subjects - without NAs

    Code
      dplyr::select(df1, -expression)
    Output
      # A tibble: 1 x 9
        statistic df.error   p.value
            <dbl>    <dbl>     <dbl>
      1      5.84     13.6 0.0000485
        method                                               estimate conf.low
        <chr>                                                   <dbl>    <dbl>
      1 Yuen's test on trimmed means for independent samples    0.915    0.702
        conf.high conf.level effectsize                        
            <dbl>      <dbl> <chr>                             
      1     0.979       0.99 Explanatory measure of effect size

---

    Code
      df1$expression[[1]]
    Output
      paste(italic("t")["Yuen"], "(", "13.584", ") = ", "5.840", ", ", 
          italic("p"), " = ", "4.85e-05", ", ", widehat(xi), " = ", 
          "0.915", ", CI"["99%"], " [", "0.702", ", ", "0.979", "]", 
          ", ", italic("n")["obs"], " = ", "32")

# t_robust - between-subjects - with NAs

    Code
      dplyr::select(df1, -expression)
    Output
      # A tibble: 1 x 9
        statistic df.error p.value
            <dbl>    <dbl>   <dbl>
      1     0.452     13.8   0.658
        method                                               estimate conf.low
        <chr>                                                   <dbl>    <dbl>
      1 Yuen's test on trimmed means for independent samples    0.366        0
        conf.high conf.level effectsize                        
            <dbl>      <dbl> <chr>                             
      1     0.777        0.9 Explanatory measure of effect size

---

    Code
      df1$expression[[1]]
    Output
      paste(italic("t")["Yuen"], "(", "13.8476", ") = ", "0.4521", 
          ", ", italic("p"), " = ", "0.6582", ", ", widehat(xi), " = ", 
          "0.3659", ", CI"["90%"], " [", "0.0000", ", ", "0.7768", 
          "]", ", ", italic("n")["obs"], " = ", "29")

