# expr_anova_robust works - between-subjects

    Code
      dplyr::select(df1, -expression)
    Output
      # A tibble: 1 x 10
        statistic    df df.error   p.value
            <dbl> <dbl>    <dbl>     <dbl>
      1      20.2     2     19.0 0.0000196
        method                                            estimate conf.level conf.low
        <chr>                                                <dbl>      <dbl>    <dbl>
      1 A heteroscedastic one-way ANOVA for trimmed means    0.859       0.95    0.853
        conf.high effectsize                        
            <dbl> <chr>                             
      1     0.864 Explanatory measure of effect size

---

    Code
      df1$expression[[1]]
    Output
      paste(italic("F")["trimmed-means"], "(", "2", ",", "18.97383", 
          ") = ", "20.24946", ", ", italic("p"), " = ", "2e-05", ", ", 
          widehat(xi), " = ", "0.85858", ", CI"["95%"], " [", "0.85268", 
          ", ", "0.86448", "], ", italic("n")["obs"], " = ", "32")

---

    Code
      dplyr::select(df2, -expression)
    Output
      # A tibble: 1 x 10
        statistic    df df.error p.value
            <dbl> <dbl>    <dbl>   <dbl>
      1    0.0503     2     21.7   0.951
        method                                            estimate conf.level conf.low
        <chr>                                                <dbl>      <dbl>    <dbl>
      1 A heteroscedastic one-way ANOVA for trimmed means    0.201       0.95   0.0872
        conf.high effectsize                        
            <dbl> <chr>                             
      1     0.754 Explanatory measure of effect size

---

    Code
      df2$expression[[1]]
    Output
      paste(italic("F")["trimmed-means"], "(", "2", ",", "21.6869", 
          ") = ", "0.0503", ", ", italic("p"), " = ", "0.9511", ", ", 
          widehat(xi), " = ", "0.2013", ", CI"["95%"], " [", "0.0872", 
          ", ", "0.7537", "], ", italic("n")["obs"], " = ", "71")

# expr_anova_robust works - within-subjects

    Code
      dplyr::select(df1, -expression)
    Output
      # A tibble: 1 x 10
        statistic    df df.error  p.value
            <dbl> <dbl>    <dbl>    <dbl>
      1      21.0  2.73     145. 1.15e-10
        method                                                              estimate
        <chr>                                                                  <dbl>
      1 A heteroscedastic one-way repeated measures ANOVA for trimmed means    0.664
        conf.level conf.low conf.high
             <dbl>    <dbl>     <dbl>
      1       0.95    0.466     0.971
        effectsize                                                     
        <chr>                                                          
      1 Algina-Keselman-Penfield robust standardized difference average

---

    Code
      df1$expression[[1]]
    Output
      paste(italic("F")["trimmed-means"], "(", "2.7303", ",", "144.7051", 
          ") = ", "20.9752", ", ", italic("p"), " = ", "1.146e-10", 
          ", ", widehat(delta)["R-avg"]^"AKP", " = ", "0.6635", ", CI"["95%"], 
          " [", "0.4660", ", ", "0.9707", "], ", italic("n")["pairs"], 
          " = ", "88")

