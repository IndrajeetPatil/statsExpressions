# between-subjects

    Code
      select(df1, -expression)
    Output
      # A tibble: 1 x 14
        parameter1 parameter2 statistic df.error      p.value
        <chr>      <chr>          <dbl>    <int>        <dbl>
      1 length     genre           51.4        8 0.0000000217
        method                       effectsize      estimate conf.level conf.low
        <chr>                        <chr>              <dbl>      <dbl>    <dbl>
      1 Kruskal-Wallis rank sum test Epsilon2 (rank)    0.328       0.95    0.258
        conf.high conf.method          conf.iterations n.obs
            <dbl> <chr>                          <int> <int>
      1         1 percentile bootstrap             100   158

---

    Code
      df1[["expression"]]
    Output
      [[1]]
      list(chi["Kruskal-Wallis"]^2 * "(" * 8 * ")" == "51.43", italic(p) == 
          "2.17e-08", widehat(epsilon)["ordinal"]^2 == "0.33", CI["95%"] ~ 
          "[" * "0.26", "1.00" * "]", italic("n")["obs"] == "158")
      

---

    Code
      select(df2, -expression)
    Output
      # A tibble: 1 x 14
        parameter1  parameter2 statistic df.error p.value method                      
        <chr>       <chr>          <dbl>    <int>   <dbl> <chr>                       
      1 sleep_cycle vore            5.24        3   0.155 Kruskal-Wallis rank sum test
        effectsize      estimate conf.level conf.low conf.high conf.method         
        <chr>              <dbl>      <dbl>    <dbl>     <dbl> <chr>               
      1 Epsilon2 (rank)    0.175       0.99   0.0445         1 percentile bootstrap
        conf.iterations n.obs
                  <int> <int>
      1             100    31

---

    Code
      df2[["expression"]]
    Output
      [[1]]
      list(chi["Kruskal-Wallis"]^2 * "(" * 3 * ")" == "5.24", italic(p) == 
          "0.16", widehat(epsilon)["ordinal"]^2 == "0.17", CI["99%"] ~ 
          "[" * "0.04", "1.00" * "]", italic("n")["obs"] == "31")
      

# within-subjects

    Code
      select(df1, -expression)
    Output
      # A tibble: 1 x 14
        parameter1 parameter2 statistic df.error  p.value method                
        <chr>      <chr>          <dbl>    <dbl>    <dbl> <chr>                 
      1 desire     condition       55.8        3 4.56e-12 Friedman rank sum test
        effectsize  estimate conf.level conf.low conf.high conf.method         
        <chr>          <dbl>      <dbl>    <dbl>     <dbl> <chr>               
      1 Kendall's W    0.211       0.99    0.140         1 percentile bootstrap
        conf.iterations n.obs
                  <int> <int>
      1             100    88

---

    Code
      df1[["expression"]]
    Output
      [[1]]
      list(chi["Friedman"]^2 * "(" * 3 * ")" == "55.83", italic(p) == 
          "4.56e-12", widehat(italic("W"))["Kendall"] == "0.21", CI["99%"] ~ 
          "[" * "0.14", "1.00" * "]", italic("n")["pairs"] == "88")
      

---

    Code
      select(df2, -expression)
    Output
      # A tibble: 1 x 14
        parameter1 parameter2 statistic df.error  p.value method                
        <chr>      <chr>          <dbl>    <dbl>    <dbl> <chr>                 
      1 value      condition        410        3 1.51e-88 Friedman rank sum test
        effectsize  estimate conf.level conf.low conf.high conf.method         
        <chr>          <dbl>      <dbl>    <dbl>     <dbl> <chr>               
      1 Kendall's W    0.911        0.9    0.906         1 percentile bootstrap
        conf.iterations n.obs
                  <int> <int>
      1             100   150

---

    Code
      df2[["expression"]]
    Output
      [[1]]
      list(chi["Friedman"]^2 * "(" * 3 * ")" == "410.00", italic(p) == 
          "1.51e-88", widehat(italic("W"))["Kendall"] == "0.91", CI["90%"] ~ 
          "[" * "0.91", "1.00" * "]", italic("n")["pairs"] == "150")
      

