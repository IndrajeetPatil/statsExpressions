# t_nonparametric works - between-subjects design

    Code
      dplyr::select(df, -expression)
    Output
      # A tibble: 1 x 10
        parameter1 parameter2 statistic   p.value method                 estimate
        <chr>      <chr>          <dbl>     <dbl> <chr>                     <dbl>
      1 wt         am              5.44 0.0000435 Wilcoxon rank sum test    0.866
        conf.level conf.low conf.high effectsize       
             <dbl>    <dbl>     <dbl> <chr>            
      1        0.9    0.688         1 r (rank biserial)

---

    Code
      df$expression[[1]]
    Output
      paste("log"["e"](italic("W")["Mann-Whitney"]), " = ", "5.440", 
          ", ", italic("p"), " = ", "4.35e-05", ", ", widehat(italic("r"))["biserial"]^"rank", 
          " = ", "0.866", ", CI"["90%"], " [", "0.688", ", ", "1.000", 
          "], ", italic("n")["obs"], " = ", "32")

# t_nonparametric works - within-subjects design

    Code
      dplyr::select(df2, -expression)
    Output
      # A tibble: 1 x 10
        parameter1 parameter2 statistic p.value method                    estimate
        <chr>      <chr>          <dbl>   <dbl> <chr>                        <dbl>
      1 length     type            2.30 0.00295 Wilcoxon signed rank test   -0.853
        conf.level conf.low conf.high effectsize       
             <dbl>    <dbl>     <dbl> <chr>            
      1       0.99       -1    -0.596 r (rank biserial)

---

    Code
      df2$expression[[1]]
    Output
      paste("log"["e"](italic("V")["Wilcoxon"]), " = ", "2.30259", 
          ", ", italic("p"), " = ", "0.00295", ", ", widehat(italic("r"))["biserial"]^"rank", 
          " = ", "-0.85294", ", CI"["99%"], " [", "-1.00000", ", ", 
          "-0.59551", "], ", italic("n")["pairs"], " = ", "16")

