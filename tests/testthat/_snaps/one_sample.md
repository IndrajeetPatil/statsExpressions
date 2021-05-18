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
      # A tibble: 1 x 11
        statistic p.value method                    alternative estimate conf.level
            <dbl>   <dbl> <chr>                     <chr>          <dbl>      <dbl>
      1      6.62   0.323 Wilcoxon signed rank test two.sided     -0.149       0.95
        conf.low conf.high effectsize        conf.method conf.iterations
           <dbl>     <dbl> <chr>             <chr>                 <dbl>
      1   -0.424    0.0888 r (rank biserial) bootstrap               200

---

    Code
      df1$expression[[1]]
    Output
      paste("log"["e"](italic("V")["Wilcoxon"]), " = ", "6.6247", ", ", 
          italic("p"), " = ", "0.3227", ", ", widehat(italic("r"))["biserial"]^"rank", 
          " = ", "-0.1486", ", CI"["95%"], " [", "-0.4240", ", ", "0.0888", 
          "], ", italic("n")["obs"], " = ", "60")

---

    Code
      dplyr::select(df2, -expression)
    Output
      # A tibble: 1 x 11
        statistic   p.value method                    alternative estimate conf.level
            <dbl>     <dbl> <chr>                     <chr>          <dbl>      <dbl>
      1      5.57 0.0000125 Wilcoxon signed rank test two.sided     -0.672       0.95
        conf.low conf.high effectsize        conf.method conf.iterations
           <dbl>     <dbl> <chr>             <chr>                 <dbl>
      1   -0.911    -0.388 r (rank biserial) bootstrap               200

---

    Code
      df2$expression[[1]]
    Output
      paste("log"["e"](italic("V")["Wilcoxon"]), " = ", "5.5683", ", ", 
          italic("p"), " = ", "1.253e-05", ", ", widehat(italic("r"))["biserial"]^"rank", 
          " = ", "-0.6717", ", CI"["95%"], " [", "-0.9112", ", ", "-0.3884", 
          "], ", italic("n")["obs"], " = ", "56")

# one_sample_test robust works

    Code
      dplyr::select(df1, -expression)
    Output
      # A tibble: 1 x 8
        statistic p.value method                                 estimate conf.low
            <dbl>   <dbl> <chr>                                     <dbl>    <dbl>
      1     0.787     0.3 Bootstrap-t method for one-sample test        9     3.81
        conf.high conf.level effectsize  
            <dbl>      <dbl> <chr>       
      1      14.2       0.99 Trimmed mean

---

    Code
      df1$expression[[1]]
    Output
      paste(italic("t")["bootstrapped"], " = ", "0.7866", ", ", italic("p"), 
          " = ", "0.3000", ", ", widehat(mu)["trimmed"], " = ", "9.0000", 
          ", CI"["99%"], " [", "3.8097", ", ", "14.1903", "], ", italic("n")["obs"], 
          " = ", "11")

---

    Code
      dplyr::select(df2, -expression)
    Output
      # A tibble: 1 x 8
        statistic p.value method                                 estimate conf.low
            <dbl>   <dbl> <chr>                                     <dbl>    <dbl>
      1     -3.81    0.02 Bootstrap-t method for one-sample test   0.0390   0.0112
        conf.high conf.level effectsize  
            <dbl>      <dbl> <chr>       
      1    0.0667        0.9 Trimmed mean

---

    Code
      df2$expression[[1]]
    Output
      paste(italic("t")["bootstrapped"], " = ", "-3.8075", ", ", italic("p"), 
          " = ", "0.0200", ", ", widehat(mu)["trimmed"], " = ", "0.0390", 
          ", CI"["90%"], " [", "0.0112", ", ", "0.0667", "], ", italic("n")["obs"], 
          " = ", "56")

# one_sample_test bayes factor works

    Code
      subtitle$expression[[1]]
    Output
      paste("log"["e"] * "(BF"["01"] * ") = " * "-47.84" * ", ", widehat(italic(delta))["difference"]^"posterior" * 
          " = " * "1.75" * ", ", "CI"["90%"]^"HDI" * " [" * "1.52" * 
          ", " * "1.99" * "], ", italic("r")["Cauchy"]^"JZS" * " = " * 
          "0.99")

---

    Code
      subtitle2$expression[[1]]
    Output
      paste("log"["e"] * "(BF"["01"] * ") = " * "2.125" * ", ", widehat(italic(delta))["difference"]^"posterior" * 
          " = " * "-0.019" * ", ", "CI"["95%"]^"HDI" * " [" * "-0.265" * 
          ", " * "0.242" * "], ", italic("r")["Cauchy"]^"JZS" * " = " * 
          "0.900")

