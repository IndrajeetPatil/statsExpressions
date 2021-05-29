# t_nonparametric works - between-subjects design

    Code
      dplyr::select(df, -expression)
    Output
      # A tibble: 1 x 12
        parameter1 parameter2 statistic   p.value method                 alternative
        <chr>      <chr>          <dbl>     <dbl> <chr>                  <chr>      
      1 wt         am              5.44 0.0000435 Wilcoxon rank sum test two.sided  
        estimate conf.level conf.low conf.high effectsize        conf.method
           <dbl>      <dbl>    <dbl>     <dbl> <chr>             <chr>      
      1    0.866        0.9    0.749     0.931 r (rank biserial) normal     

---

    Code
      df$expression[[1]]
    Output
      paste("log"["e"](italic("W")["Mann-Whitney"]), " = ", "5.440", 
          ", ", italic("p"), " = ", "4.35e-05", ", ", widehat(italic("r"))["biserial"]^"rank", 
          " = ", "0.866", ", CI"["90%"], " [", "0.749", ", ", "0.931", 
          "], ", italic("n")["obs"], " = ", "32")

# t_nonparametric works - within-subjects design

    Code
      dplyr::select(df2, -expression)
    Output
      # A tibble: 1 x 12
        parameter1 parameter2 statistic p.value method                    alternative
        <chr>      <chr>          <dbl>   <dbl> <chr>                     <chr>      
      1 length     type            2.30 0.00295 Wilcoxon signed rank test two.sided  
        estimate conf.level conf.low conf.high effectsize        conf.method
           <dbl>      <dbl>    <dbl>     <dbl> <chr>             <chr>      
      1   -0.853       0.99   -0.964    -0.489 r (rank biserial) normal     

---

    Code
      df2$expression[[1]]
    Output
      paste("log"["e"](italic("V")["Wilcoxon"]), " = ", "2.30259", 
          ", ", italic("p"), " = ", "0.00295", ", ", widehat(italic("r"))["biserial"]^"rank", 
          " = ", "-0.85294", ", CI"["99%"], " [", "-0.96399", ", ", 
          "-0.48865", "], ", italic("n")["pairs"], " = ", "16")

