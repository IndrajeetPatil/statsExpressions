# corr_test works - nonparametric

    Code
      dplyr::select(df1, -expression)
    Output
      # A tibble: 1 x 11
        parameter1 parameter2 estimate conf.level conf.low conf.high statistic
        <chr>      <chr>         <dbl>      <dbl>    <dbl>     <dbl>     <dbl>
      1 rating     length        0.495      0.999    0.153     0.731      10.6
           p.value method               n.obs effectsize          
             <dbl> <chr>                <int> <chr>               
      1 0.00000344 Spearman correlation    79 Spearman correlation

---

    Code
      df1$expression[[1]]
    Output
      paste("log"["e"](italic("S")), " = ", "10.63231", ", ", italic("p"), 
          " = ", "3.4438e-06", ", ", widehat(rho)["Spearman"], " = ", 
          "0.49546", ", CI"["99.9%"], " [", "0.15344", ", ", "0.73147", 
          "]", ", ", italic("n")["pairs"], " = ", "79")

---

    Code
      dplyr::select(df2, -expression)
    Output
      # A tibble: 1 x 11
        parameter1 parameter2 estimate conf.level conf.low conf.high statistic
        <chr>      <chr>         <dbl>      <dbl>    <dbl>     <dbl>     <dbl>
      1 wt         mpg          -0.886       0.95   -0.945    -0.774      9.24
         p.value method               n.obs effectsize          
           <dbl> <chr>                <int> <chr>               
      1 1.49e-11 Spearman correlation    32 Spearman correlation

---

    Code
      df2$expression[[1]]
    Output
      paste("log"["e"](italic("S")), " = ", "9.24", ", ", italic("p"), 
          " = ", "1.49e-11", ", ", widehat(rho)["Spearman"], " = ", 
          "-0.89", ", CI"["95%"], " [", "-0.94", ", ", "-0.77", "]", 
          ", ", italic("n")["pairs"], " = ", "32")

# corr_test works - parametric

    Code
      dplyr::select(df, -expression)
    Output
      # A tibble: 1 x 12
        parameter1 parameter2 estimate conf.level conf.low conf.high statistic
        <chr>      <chr>         <dbl>      <dbl>    <dbl>     <dbl>     <dbl>
      1 brainwt    sleep_rem    -0.221        0.9   -0.438    0.0201     -1.54
        df.error p.value method              n.obs effectsize         
           <int>   <dbl> <chr>               <int> <chr>              
      1       46   0.131 Pearson correlation    48 Pearson correlation

---

    Code
      df$expression[[1]]
    Output
      paste(italic("t")["Student"], "(", "46", ") = ", "-1.539", ", ", 
          italic("p"), " = ", "0.131", ", ", widehat(italic("r"))["Pearson"], 
          " = ", "-0.221", ", CI"["90%"], " [", "-0.438", ", ", "0.020", 
          "]", ", ", italic("n")["pairs"], " = ", "48")

# corr_test works - robust

    Code
      dplyr::select(df, -expression)
    Output
      # A tibble: 1 x 12
        parameter1 parameter2  estimate conf.level conf.low conf.high statistic
        <chr>      <chr>          <dbl>      <dbl>    <dbl>     <dbl>     <dbl>
      1 brainwt    sleep_total   -0.549        0.5   -0.611    -0.481     -4.83
        df.error   p.value method                         n.obs
           <int>     <dbl> <chr>                          <int>
      1       54 0.0000117 Winsorized Pearson correlation    56
        effectsize                    
        <chr>                         
      1 Winsorized Pearson correlation

---

    Code
      df$expression[[1]]
    Output
      paste(italic("t")["Student"], "(", "54", ") = ", "-4.8286", ", ", 
          italic("p"), " = ", "1.172e-05", ", ", widehat(italic("r"))["Winsorized"], 
          " = ", "-0.5491", ", CI"["50%"], " [", "-0.6106", ", ", "-0.4812", 
          "]", ", ", italic("n")["pairs"], " = ", "56")

# bayes factor (correlation test) - without NAs

    Code
      subtitle1$expression[[1]]
    Output
      atop(displaystyle("huh"), expr = paste("log"["e"] * "(BF"["01"] * 
          ") = " * "1.07" * ", ", widehat(rho)["Pearson"]^"posterior" * 
          " = " * "-0.12" * ", ", "CI"["95%"]^"HDI" * " [" * "-0.24" * 
          ", " * "0.02" * "], ", italic("r")["beta"]^"JZS" * " = " * 
          "1.41"))

# bayes factor (correlation test) - with NAs

    Code
      subtitle1$expression[[1]]
    Output
      paste("log"["e"] * "(BF"["01"] * ") = " * "0.487" * ", ", widehat(rho)["Pearson"]^"posterior" * 
          " = " * "-0.210" * ", ", "CI"["99%"]^"HDI" * " [" * "-0.410" * 
          ", " * "0.026" * "], ", italic("r")["beta"]^"JZS" * " = " * 
          "1.250")

