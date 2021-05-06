# one_sample_test parametric works

    Code
      dplyr::select(df1, -expression)
    Output
      # A tibble: 1 x 10
           mu statistic df.error p.value method            estimate conf.level
        <dbl>     <dbl>    <dbl>   <dbl> <chr>                <dbl>      <dbl>
      1   120     -2.67       78 0.00910 One Sample t-test   -0.298       0.95
        conf.low conf.high effectsize
           <dbl>     <dbl> <chr>     
      1   -0.524   -0.0743 Hedges' g 

---

    Code
      df1$expression[[1]]
    Output
      paste(italic("t")["Student"], "(", "78", ") = ", "-2.67496", 
          ", ", italic("p"), " = ", "0.00910", ", ", widehat(italic("g"))["Hedges"], 
          " = ", "-0.29805", ", CI"["95%"], " [", "-0.52379", ", ", 
          "-0.07429", "]", ", ", italic("n")["obs"], " = ", "79")

---

    Code
      dplyr::select(df2, -expression)
    Output
      # A tibble: 1 x 10
           mu statistic df.error p.value method            estimate conf.level
        <dbl>     <dbl>    <dbl>   <dbl> <chr>                <dbl>      <dbl>
      1   120     -2.67       78 0.00910 One Sample t-test   -0.301        0.9
        conf.low conf.high effectsize
           <dbl>     <dbl> <chr>     
      1   -0.492    -0.111 Cohen's d 

---

    Code
      df2$expression[[1]]
    Output
      paste(italic("t")["Student"], "(", "78", ") = ", "-2.6750", ", ", 
          italic("p"), " = ", "0.0091", ", ", widehat(italic("d"))["Cohen"], 
          " = ", "-0.3010", ", CI"["90%"], " [", "-0.4924", ", ", "-0.1115", 
          "]", ", ", italic("n")["obs"], " = ", "79")

# one_sample_test non-parametric works

    Code
      dplyr::select(df1, -expression)
    Output
      # A tibble: 1 x 8
        statistic p.value method                    estimate conf.level conf.low
            <dbl>   <dbl> <chr>                        <dbl>      <dbl>    <dbl>
      1      6.62   0.323 Wilcoxon signed rank test   -0.149       0.95   -0.458
        conf.high effectsize       
            <dbl> <chr>            
      1     0.121 r (rank biserial)

---

    Code
      df1$expression[[1]]
    Output
      paste("log"["e"](italic("V")["Wilcoxon"]), " = ", "6.6247", ", ", 
          italic("p"), " = ", "0.3227", ", ", widehat(italic("r"))["biserial"]^"rank", 
          " = ", "-0.1486", ", CI"["95%"], " [", "-0.4584", ", ", "0.1206", 
          "]", ", ", italic("n")["obs"], " = ", "60")

---

    Code
      dplyr::select(df2, -expression)
    Output
      # A tibble: 1 x 8
        statistic   p.value method                    estimate conf.level conf.low
            <dbl>     <dbl> <chr>                        <dbl>      <dbl>    <dbl>
      1      5.57 0.0000125 Wilcoxon signed rank test   -0.672       0.95   -0.905
        conf.high effectsize       
            <dbl> <chr>            
      1    -0.418 r (rank biserial)

---

    Code
      df2$expression[[1]]
    Output
      paste("log"["e"](italic("V")["Wilcoxon"]), " = ", "5.5683", ", ", 
          italic("p"), " = ", "1.253e-05", ", ", widehat(italic("r"))["biserial"]^"rank", 
          " = ", "-0.6717", ", CI"["95%"], " [", "-0.9052", ", ", "-0.4177", 
          "]", ", ", italic("n")["obs"], " = ", "56")

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
          ", CI"["99%"], " [", "3.8097", ", ", "14.1903", "]", ", ", 
          italic("n")["obs"], " = ", "11")

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
          ", CI"["90%"], " [", "0.0112", ", ", "0.0667", "]", ", ", 
          italic("n")["obs"], " = ", "56")

# one_sample_test bayes factor works

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

