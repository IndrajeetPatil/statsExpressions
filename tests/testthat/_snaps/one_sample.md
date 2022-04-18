# one_sample_test parametric works

    Code
      select(df1, -expression)
    Output
      # A tibble: 1 x 14
           mu statistic df.error p.value method            alternative effectsize
        <dbl>     <dbl>    <dbl>   <dbl> <chr>             <chr>       <chr>     
      1   120     -2.67       78 0.00910 One Sample t-test two.sided   Hedges' g 
        estimate conf.level conf.low conf.high conf.method conf.distribution n.obs
           <dbl>      <dbl>    <dbl>     <dbl> <chr>       <chr>             <int>
      1   -0.298       0.95   -0.524   -0.0743 ncp         t                    79

---

    Code
      df1$expression
    Output
      [[1]]
      list(italic("t")["Student"] * "(" * 78 * ")" == "-2.67496", italic(p) == 
          "0.00910", widehat(italic("g"))["Hedges"] == "-0.29805", 
          CI["95%"] ~ "[" * "-0.52379", "-0.07429" * "]", italic("n")["obs"] == 
              "79")
      

---

    Code
      select(df2, -expression)
    Output
      # A tibble: 1 x 14
           mu statistic df.error p.value method            alternative effectsize
        <dbl>     <dbl>    <dbl>   <dbl> <chr>             <chr>       <chr>     
      1   120     -2.67       78 0.00910 One Sample t-test two.sided   Cohen's d 
        estimate conf.level conf.low conf.high conf.method conf.distribution n.obs
           <dbl>      <dbl>    <dbl>     <dbl> <chr>       <chr>             <int>
      1   -0.301        0.9   -0.492    -0.111 ncp         t                    79

---

    Code
      df2$expression
    Output
      [[1]]
      list(italic("t")["Student"] * "(" * 78 * ")" == "-2.6750", italic(p) == 
          "0.0091", widehat(italic("d"))["Cohen"] == "-0.3010", CI["90%"] ~ 
          "[" * "-0.4924", "-0.1115" * "]", italic("n")["obs"] == "79")
      

# one_sample_test non-parametric works

    Code
      select(df1, -expression)
    Output
      # A tibble: 1 x 11
        statistic p.value method                    alternative effectsize       
            <dbl>   <dbl> <chr>                     <chr>       <chr>            
      1      754.   0.323 Wilcoxon signed rank test two.sided   r (rank biserial)
        estimate conf.level conf.low conf.high conf.method n.obs
           <dbl>      <dbl>    <dbl>     <dbl> <chr>       <int>
      1   -0.149       0.95   -0.416     0.143 normal         60

---

    Code
      df1$expression
    Output
      [[1]]
      list(italic("V")["Wilcoxon"] == "753.5000", italic(p) == "0.3227", 
          widehat(italic("r"))["biserial"]^"rank" == "-0.1486", CI["95%"] ~ 
              "[" * "-0.4162", "0.1427" * "]", italic("n")["obs"] == 
              "60")
      

---

    Code
      select(df2, -expression)
    Output
      # A tibble: 1 x 11
        statistic   p.value method                    alternative effectsize       
            <dbl>     <dbl> <chr>                     <chr>       <chr>            
      1       262 0.0000125 Wilcoxon signed rank test two.sided   r (rank biserial)
        estimate conf.level conf.low conf.high conf.method n.obs
           <dbl>      <dbl>    <dbl>     <dbl> <chr>       <int>
      1   -0.672       0.95   -0.806    -0.472 normal         56

---

    Code
      df2$expression
    Output
      [[1]]
      list(italic("V")["Wilcoxon"] == "262.0000", italic(p) == "1.2527e-05", 
          widehat(italic("r"))["biserial"]^"rank" == "-0.6717", CI["95%"] ~ 
              "[" * "-0.8058", "-0.4720" * "]", italic("n")["obs"] == 
              "56")
      

# one_sample_test robust works

    Code
      select(df1, -expression)
    Output
      # A tibble: 1 x 9
        statistic p.value n.obs method                                 effectsize  
            <dbl>   <dbl> <int> <chr>                                  <chr>       
      1     0.787   0.455    11 Bootstrap-t method for one-sample test Trimmed mean
        estimate conf.level conf.low conf.high
           <dbl>      <dbl>    <dbl>     <dbl>
      1        9        0.9     6.55      11.5

---

    Code
      df1$expression
    Output
      [[1]]
      list(italic("t")["bootstrapped"] == "0.7866", italic(p) == "0.4550", 
          widehat(mu)["trimmed"] == "9.0000", CI["90%"] ~ "[" * "6.5487", 
          "11.4513" * "]", italic("n")["obs"] == "11")
      

---

    Code
      select(df2, -expression)
    Output
      # A tibble: 1 x 9
        statistic p.value n.obs method                                 effectsize  
            <dbl>   <dbl> <int> <chr>                                  <chr>       
      1     -3.81    0.04    56 Bootstrap-t method for one-sample test Trimmed mean
        estimate conf.level conf.low conf.high
           <dbl>      <dbl>    <dbl>     <dbl>
      1   0.0390       0.99  -0.0669     0.145

---

    Code
      df2$expression
    Output
      [[1]]
      list(italic("t")["bootstrapped"] == "-3.8075", italic(p) == "0.0400", 
          widehat(mu)["trimmed"] == "0.0390", CI["99%"] ~ "[" * "-0.0669", 
          "0.1448" * "]", italic("n")["obs"] == "56")
      

# one_sample_test bayes factor works

    Code
      names(df_results)
    Output
       [1] "term"               "effectsize"         "estimate"          
       [4] "conf.level"         "conf.low"           "conf.high"         
       [7] "pd"                 "rope.percentage"    "prior.distribution"
      [10] "prior.location"     "prior.scale"        "bf10"              
      [13] "method"             "log_e_bf10"         "n.obs"             
      [16] "expression"        

---

    Code
      df1$expression
    Output
      [[1]]
      list(log[e] * (BF["01"]) == "-47.84", widehat(delta)["difference"]^"posterior" == 
          "-1.76", CI["90%"]^HDI ~ "[" * "-1.99", "-1.51" * "]", italic("r")["Cauchy"]^"JZS" == 
          "0.99")
      

---

    Code
      df2$expression
    Output
      [[1]]
      list(log[e] * (BF["01"]) == "2.125", widehat(delta)["difference"]^"posterior" == 
          "0.018", CI["95%"]^HDI ~ "[" * "-0.234", "0.274" * "]", italic("r")["Cauchy"]^"JZS" == 
          "0.900")
      

