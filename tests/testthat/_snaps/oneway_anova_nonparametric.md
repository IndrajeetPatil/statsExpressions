# between-subjects - data with and without NAs

    Code
      select(df1, -expression)
    Output
      # A tibble: 1 x 13
        parameter1 parameter2 statistic df.error      p.value
        <chr>      <chr>          <dbl>    <int>        <dbl>
      1 length     genre           51.4        8 0.0000000217
        method                       estimate conf.level conf.low conf.high
        <chr>                           <dbl>      <dbl>    <dbl>     <dbl>
      1 Kruskal-Wallis rank sum test    0.328       0.95    0.248     0.525
        effectsize      conf.method          conf.iterations
        <chr>           <chr>                          <int>
      1 Epsilon2 (rank) percentile bootstrap             100

---

    Code
      df1$expression[[1]]
    Output
      paste(chi["Kruskal-Wallis"]^2, "(", "8", ") = ", "51.42672", 
          ", ", italic("p"), " = ", "2.1714e-08", ", ", widehat(epsilon)["ordinal"]^2, 
          " = ", "0.32756", ", CI"["95%"], " [", "0.24771", ", ", "0.52467", 
          "], ", italic("n")["obs"], " = ", "158")

---

    Code
      select(df2, -expression)
    Output
      # A tibble: 1 x 13
        parameter1  parameter2 statistic df.error p.value method                      
        <chr>       <chr>          <dbl>    <int>   <dbl> <chr>                       
      1 sleep_cycle vore            5.24        3   0.155 Kruskal-Wallis rank sum test
        estimate conf.level conf.low conf.high effectsize      conf.method         
           <dbl>      <dbl>    <dbl>     <dbl> <chr>           <chr>               
      1    0.175       0.99   0.0443     0.548 Epsilon2 (rank) percentile bootstrap
        conf.iterations
                  <int>
      1             100

---

    Code
      df2$expression[[1]]
    Output
      paste(chi["Kruskal-Wallis"]^2, "(", "3", ") = ", "5.240", ", ", 
          italic("p"), " = ", "0.155", ", ", widehat(epsilon)["ordinal"]^2, 
          " = ", "0.175", ", CI"["99%"], " [", "0.044", ", ", "0.548", 
          "], ", italic("n")["obs"], " = ", "31")

# within-subjects - data with and without NAs

    Code
      df1$expression[[1]]
    Output
      paste(chi["Friedman"]^2, "(", "3", ") = ", "55.8338", ", ", italic("p"), 
          " = ", "4.558e-12", ", ", widehat(italic("W"))["Kendall"], 
          " = ", "0.1750", ", CI"["99%"], " [", "0.1141", ", ", "0.3020", 
          "], ", italic("n")["pairs"], " = ", "88")

---

    Code
      df2$expression[[1]]
    Output
      paste(chi["Friedman"]^2, "(", "3", ") = ", "410.000", ", ", italic("p"), 
          " = ", "1.51e-88", ", ", widehat(italic("W"))["Kendall"], 
          " = ", "0.911", ", CI"["90%"], " [", "0.904", ", ", "0.919", 
          "], ", italic("n")["pairs"], " = ", "150")

