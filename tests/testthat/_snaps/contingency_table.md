# contingency_table works - data without NAs

    Code
      dplyr::select(df1, -expression)
    Output
      # A tibble: 1 x 9
        statistic    df p.value method                     estimate conf.level
            <dbl> <int>   <dbl> <chr>                         <dbl>      <dbl>
      1      8.74     2  0.0126 Pearson's Chi-squared test    0.464       0.99
        conf.low conf.high effectsize       
           <dbl>     <dbl> <chr>            
      1        0     0.888 Cramer's V (adj.)

---

    Code
      df1$expression[[1]]
    Output
      paste(chi["Pearson"]^2, "(", "2", ") = ", "8.74073", ", ", italic("p"), 
          " = ", "0.01265", ", ", widehat(italic("V"))["Cramer"], " = ", 
          "0.46431", ", CI"["99%"], " [", "0.00000", ", ", "0.88767", 
          "]", ", ", italic("n")["obs"], " = ", "32")

---

    Code
      dplyr::select(df2, -expression)
    Output
      # A tibble: 1 x 9
        statistic    df   p.value method                     estimate conf.level
            <dbl> <int>     <dbl> <chr>                         <dbl>      <dbl>
      1      457.     1 2.30e-101 Pearson's Chi-squared test    0.455       0.95
        conf.low conf.high effectsize       
           <dbl>     <dbl> <chr>            
      1    0.413     0.497 Cramer's V (adj.)

---

    Code
      df2$expression[[1]]
    Output
      paste(chi["Pearson"]^2, "(", "1", ") = ", "456.87", ", ", italic("p"), 
          " = ", "2.3e-101", ", ", widehat(italic("V"))["Cramer"], 
          " = ", "0.46", ", CI"["95%"], " [", "0.41", ", ", "0.50", 
          "]", ", ", italic("n")["obs"], " = ", "2,201")

# contingency_table works - data with NAs

    Code
      dplyr::select(df1, -expression)
    Output
      # A tibble: 1 x 9
        statistic    df p.value method                     estimate conf.level
            <dbl> <int>   <dbl> <chr>                         <dbl>      <dbl>
      1      15.8    15   0.399 Pearson's Chi-squared test   0.0558       0.99
        conf.low conf.high effectsize       
           <dbl>     <dbl> <chr>            
      1        0         0 Cramer's V (adj.)

---

    Code
      df1$expression[[1]]
    Output
      paste(chi["Pearson"]^2, "(", "15", ") = ", "15.75", ", ", italic("p"), 
          " = ", "0.399", ", ", widehat(italic("V"))["Cramer"], " = ", 
          "0.06", ", CI"["99%"], " [", "0.00", ", ", "0.00", "]", ", ", 
          italic("n")["obs"], " = ", "52")

# paired contingency_table works - counts data without NAs

    Code
      dplyr::select(df1, -expression)
    Output
      # A tibble: 1 x 9
        statistic    df  p.value method                     estimate conf.level
            <dbl> <dbl>    <dbl> <chr>                         <dbl>      <dbl>
      1      13.3     1 0.000261 McNemar's Chi-squared test    0.333       0.95
        conf.low conf.high effectsize
           <dbl>     <dbl> <chr>     
      1    0.164     0.427 Cohen's g 

---

    Code
      df1$expression[[1]]
    Output
      paste(chi["McNemar"]^2, "(", "1", ") = ", "13.33333", ", ", italic("p"), 
          " = ", "2.6073e-04", ", ", widehat(italic("g"))["Cohen"], 
          " = ", "0.33333", ", CI"["95%"], " [", "0.16436", ", ", "0.42663", 
          "]", ", ", italic("n")["pairs"], " = ", "100")

# paired contingency_table works - with NAs

    Code
      dplyr::select(df1, -expression)
    Output
      # A tibble: 1 x 9
        statistic    df  p.value method                     estimate conf.level
            <dbl> <dbl>    <dbl> <chr>                         <dbl>      <dbl>
      1      13.3     1 0.000261 McNemar's Chi-squared test    0.333        0.9
        conf.low conf.high effectsize
           <dbl>     <dbl> <chr>     
      1    0.195     0.416 Cohen's g 

---

    Code
      df1$expression[[1]]
    Output
      paste(chi["McNemar"]^2, "(", "1", ") = ", "13.333", ", ", italic("p"), 
          " = ", "2.61e-04", ", ", widehat(italic("g"))["Cohen"], " = ", 
          "0.333", ", CI"["90%"], " [", "0.195", ", ", "0.416", "]", 
          ", ", italic("n")["pairs"], " = ", "95")

# Goodness of Fit contingency_table works without counts

    Code
      dplyr::select(df1, -expression)
    Output
      # A tibble: 1 x 9
        statistic    df p.value method                                   estimate
            <dbl> <dbl>   <dbl> <chr>                                       <dbl>
      1      1.12     1   0.289 Chi-squared test for given probabilities   0.0547
        conf.level conf.low conf.high effectsize       
             <dbl>    <dbl>     <dbl> <chr>            
      1       0.99        0     0.499 Cramer's V (adj.)

---

    Code
      df1$expression[[1]]
    Output
      paste(chi["gof"]^2, "(", "1", ") = ", "1.12500", ", ", italic("p"), 
          " = ", "0.28884", ", ", widehat(italic("V"))["Cramer"], " = ", 
          "0.05472", ", CI"["99%"], " [", "0.00000", ", ", "0.49920", 
          "]", ", ", italic("n")["obs"], " = ", "32")

---

    Code
      dplyr::select(df2, -expression)
    Output
      # A tibble: 1 x 9
        statistic    df   p.value method                                   estimate
            <dbl> <dbl>     <dbl> <chr>                                       <dbl>
      1      722.     1 3.92e-159 Chi-squared test for given probabilities    0.573
        conf.level conf.low conf.high effectsize       
             <dbl>    <dbl>     <dbl> <chr>            
      1       0.95    0.465     0.615 Cramer's V (adj.)

---

    Code
      df2$expression[[1]]
    Output
      paste(chi["gof"]^2, "(", "1", ") = ", "722.45", ", ", italic("p"), 
          " = ", "3.92e-159", ", ", widehat(italic("V"))["Cramer"], 
          " = ", "0.57", ", CI"["95%"], " [", "0.47", ", ", "0.62", 
          "]", ", ", italic("n")["obs"], " = ", "2,201")

# Goodness of Fit contingency_table works with counts

    Code
      dplyr::select(df1, -expression)
    Output
      # A tibble: 1 x 9
        statistic    df   p.value method                                   estimate
            <dbl> <dbl>     <dbl> <chr>                                       <dbl>
      1      722.     1 3.92e-159 Chi-squared test for given probabilities    0.573
        conf.level conf.low conf.high effectsize       
             <dbl>    <dbl>     <dbl> <chr>            
      1       0.95    0.465     0.615 Cramer's V (adj.)

---

    Code
      df1$expression[[1]]
    Output
      paste(chi["gof"]^2, "(", "1", ") = ", "722.454", ", ", italic("p"), 
          " = ", "3.92e-159", ", ", widehat(italic("V"))["Cramer"], 
          " = ", "0.573", ", CI"["95%"], " [", "0.465", ", ", "0.615", 
          "]", ", ", italic("n")["obs"], " = ", "2,201")

# works with dataframes with NAs and with ratio

    Code
      dplyr::select(df1, -expression)
    Output
      # A tibble: 1 x 9
        statistic    df     p.value method                                   estimate
            <dbl> <dbl>       <dbl> <chr>                                       <dbl>
      1      33.8     3 0.000000223 Chi-squared test for given probabilities    0.375
        conf.level conf.low conf.high effectsize       
             <dbl>    <dbl>     <dbl> <chr>            
      1       0.95    0.222     0.486 Cramer's V (adj.)

---

    Code
      df1$expression[[1]]
    Output
      paste(chi["gof"]^2, "(", "3", ") = ", "33.76", ", ", italic("p"), 
          " = ", "2.23e-07", ", ", widehat(italic("V"))["Cramer"], 
          " = ", "0.37", ", CI"["95%"], " [", "0.22", ", ", "0.49", 
          "]", ", ", italic("n")["obs"], " = ", "76")

# works even in edge cases

    Code
      dplyr::select(df1, -expression)
    Output
      # A tibble: 1 x 9
        statistic    df p.value method                     estimate conf.level
            <dbl> <int>   <dbl> <chr>                         <dbl>      <dbl>
      1         3     2   0.223 Pearson's Chi-squared test    0.354       0.95
        conf.low conf.high effectsize       
           <dbl>     <dbl> <chr>            
      1        0     0.949 Cramer's V (adj.)

---

    Code
      df1$expression[[1]]
    Output
      paste(chi["Pearson"]^2, "(", "2", ") = ", "3.00", ", ", italic("p"), 
          " = ", "0.223", ", ", widehat(italic("V"))["Cramer"], " = ", 
          "0.35", ", CI"["95%"], " [", "0.00", ", ", "0.95", "]", ", ", 
          italic("n")["obs"], " = ", "6")

---

    Code
      dplyr::select(df2, -expression)
    Output
      # A tibble: 1 x 9
        statistic    df p.value method                                   estimate
            <dbl> <dbl>   <dbl> <chr>                                       <dbl>
      1      7.68     2  0.0214 Chi-squared test for given probabilities    0.406
        conf.level conf.low conf.high effectsize       
             <dbl>    <dbl>     <dbl> <chr>            
      1       0.95        0     0.675 Cramer's V (adj.)

---

    Code
      df2$expression[[1]]
    Output
      paste(chi["gof"]^2, "(", "2", ") = ", "7.68", ", ", italic("p"), 
          " = ", "0.021", ", ", widehat(italic("V"))["Cramer"], " = ", 
          "0.41", ", CI"["95%"], " [", "0.00", ", ", "0.67", "]", ", ", 
          italic("n")["obs"], " = ", "19")

# bayes factor (proportion test)

    Code
      expr_text$expression[[1]]
    Output
      atop(displaystyle("duh"), expr = paste("log"["e"] * "(BF"["01"] * 
          ") = " * "0.55" * ", ", italic("a")["Gunel-Dickey"] * " = " * 
          "10.00"))

# bayes factor (contingency tab)

    Code
      list(expr_text1$expression[[1]], expr_text2$expression[[1]], expr_text3$
        expression[[1]])
    Output
      [[1]]
      paste("log"["e"] * "(BF"["01"] * ") = " * "-3.335" * ", ", widehat(italic("V"))["Cramer"]^"posterior" * 
          " = " * "0.479" * ", ", "CI"["89%"]^"HDI" * " [" * "0.285" * 
          ", " * "0.692" * "], ", italic("a")["Gunel-Dickey"] * " = " * 
          "1.000")
      
      [[2]]
      paste("log"["e"] * "(BF"["01"] * ") = " * "-214.255" * ", ", 
          widehat(italic("V"))["Cramer"]^"posterior" * " = " * "0.455" * 
              ", ", "CI"["99%"]^"HDI" * " [" * "0.402" * ", " * "0.508" * 
              "], ", italic("a")["Gunel-Dickey"] * " = " * "1.000")
      
      [[3]]
      paste("log"["e"] * "(BF"["01"] * ") = " * "-213.873" * ", ", 
          widehat(italic("V"))["Cramer"]^"posterior" * " = " * "0.454" * 
              ", ", "CI"["95%"]^"HDI" * " [" * "0.417" * ", " * "0.495" * 
              "], ", italic("a")["Gunel-Dickey"] * " = " * "1.500")
      

