# one_sample_test parametric works

    Code
      dplyr::select(df1, -expression)
    Output
      # A tibble: 1 x 13
           mu statistic df.error p.value method            alternative estimate
        <dbl>     <dbl>    <dbl>   <dbl> <chr>             <chr>          <dbl>
      1   120     -2.67       78 0.00910 One Sample t-test two.sided     -0.298
        conf.level conf.low conf.high effectsize conf.method conf.distribution
             <dbl>    <dbl>     <dbl> <chr>      <chr>       <chr>            
      1       0.95   -0.524   -0.0743 Hedges' g  ncp         t                

---

    Code
      df1$expression[[1]]
    Output
      paste(italic("t")["Student"], "(", "78", ") = ", "-2.67496", 
          ", ", italic("p"), " = ", "0.00910", ", ", widehat(italic("g"))["Hedges"], 
          " = ", "-0.29805", ", CI"["95%"], " [", "-0.52379", ", ", 
          "-0.07429", "], ", italic("n")["obs"], " = ", "79")

---

    Code
      dplyr::select(df2, -expression)
    Output
      # A tibble: 1 x 13
           mu statistic df.error p.value method            alternative estimate
        <dbl>     <dbl>    <dbl>   <dbl> <chr>             <chr>          <dbl>
      1   120     -2.67       78 0.00910 One Sample t-test two.sided     -0.301
        conf.level conf.low conf.high effectsize conf.method conf.distribution
             <dbl>    <dbl>     <dbl> <chr>      <chr>       <chr>            
      1        0.9   -0.492    -0.111 Cohen's d  ncp         t                

---

    Code
      df2$expression[[1]]
    Output
      paste(italic("t")["Student"], "(", "78", ") = ", "-2.6750", ", ", 
          italic("p"), " = ", "0.0091", ", ", widehat(italic("d"))["Cohen"], 
          " = ", "-0.3010", ", CI"["90%"], " [", "-0.4924", ", ", "-0.1115", 
          "], ", italic("n")["obs"], " = ", "79")

# one_sample_test non-parametric works

    Code
      dplyr::select(df1, -expression)
    Output
      # A tibble: 1 x 10
        statistic p.value method                    alternative estimate conf.level
            <dbl>   <dbl> <chr>                     <chr>          <dbl>      <dbl>
      1      754.   0.323 Wilcoxon signed rank test two.sided     -0.149       0.95
        conf.low conf.high effectsize        conf.method
           <dbl>     <dbl> <chr>             <chr>      
      1   -0.416     0.143 r (rank biserial) normal     

---

    Code
      df1$expression[[1]]
    Output
      paste(italic("V")["Wilcoxon"], " = ", "753.5000", ", ", italic("p"), 
          " = ", "0.3227", ", ", widehat(italic("r"))["biserial"]^"rank", 
          " = ", "-0.1486", ", CI"["95%"], " [", "-0.4162", ", ", "0.1427", 
          "], ", italic("n")["obs"], " = ", "60")

---

    Code
      dplyr::select(df2, -expression)
    Output
      # A tibble: 1 x 10
        statistic   p.value method                    alternative estimate conf.level
            <dbl>     <dbl> <chr>                     <chr>          <dbl>      <dbl>
      1       262 0.0000125 Wilcoxon signed rank test two.sided     -0.672       0.95
        conf.low conf.high effectsize        conf.method
           <dbl>     <dbl> <chr>             <chr>      
      1   -0.806    -0.472 r (rank biserial) normal     

---

    Code
      df2$expression[[1]]
    Output
      paste(italic("V")["Wilcoxon"], " = ", "262.0000", ", ", italic("p"), 
          " = ", "1.253e-05", ", ", widehat(italic("r"))["biserial"]^"rank", 
          " = ", "-0.6717", ", CI"["95%"], " [", "-0.8058", ", ", "-0.4720", 
          "], ", italic("n")["obs"], " = ", "56")

# one_sample_test robust works

    Code
      dplyr::select(df1, -expression)
    Output
      # A tibble: 1 x 9
        statistic p.value n.obs method                                 estimate
            <dbl>   <dbl> <int> <chr>                                     <dbl>
      1     0.787   0.455    11 Bootstrap-t method for one-sample test        9
        conf.level conf.low conf.high effectsize  
             <dbl>    <dbl>     <dbl> <chr>       
      1       0.95     6.09      11.9 Trimmed mean

---

    Code
      df1$expression[[1]]
    Output
      paste(italic("t")["bootstrapped"], " = ", "0.7866", ", ", italic("p"), 
          " = ", "0.4550", ", ", widehat(mu)["trimmed"], " = ", "9.0000", 
          ", CI"["95%"], " [", "6.0950", ", ", "11.9050", "], ", italic("n")["obs"], 
          " = ", "11")

---

    Code
      dplyr::select(df2, -expression)
    Output
      # A tibble: 1 x 9
        statistic p.value n.obs method                                 estimate
            <dbl>   <dbl> <int> <chr>                                     <dbl>
      1     -3.81    0.04    56 Bootstrap-t method for one-sample test   0.0390
        conf.level conf.low conf.high effectsize  
             <dbl>    <dbl>     <dbl> <chr>       
      1       0.95  -0.0132    0.0911 Trimmed mean

---

    Code
      df2$expression[[1]]
    Output
      paste(italic("t")["bootstrapped"], " = ", "-3.8075", ", ", italic("p"), 
          " = ", "0.0400", ", ", widehat(mu)["trimmed"], " = ", "0.0390", 
          ", CI"["95%"], " [", "-0.0132", ", ", "0.0911", "], ", italic("n")["obs"], 
          " = ", "56")

# one_sample_test bayes factor works

    Code
      names(df_results)
    Output
       [1] "term"               "estimate"           "conf.level"        
       [4] "conf.low"           "conf.high"          "pd"                
       [7] "rope.percentage"    "prior.distribution" "prior.location"    
      [10] "prior.scale"        "bf10"               "method"            
      [13] "log_e_bf10"         "expression"        

---

    Code
      subtitle$expression[[1]]
    Output
      paste("log"["e"] * "(BF"["01"] * ") = " * "-47.84" * ", ", widehat(italic(delta))["difference"]^"posterior" * 
          " = " * "1.76" * ", ", "CI"["90%"]^"HDI" * " [" * "1.52" * 
          ", " * "1.99" * "], ", italic("r")["Cauchy"]^"JZS" * " = " * 
          "0.99")

---

    Code
      subtitle2$expression[[1]]
    Output
      paste("log"["e"] * "(BF"["01"] * ") = " * "2.125" * ", ", widehat(italic(delta))["difference"]^"posterior" * 
          " = " * "-0.018" * ", ", "CI"["95%"]^"HDI" * " [" * "-0.265" * 
          ", " * "0.242" * "], ", italic("r")["Cauchy"]^"JZS" * " = " * 
          "0.900")

