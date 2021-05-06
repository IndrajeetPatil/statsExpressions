# parametric anova subtitles work (without NAs)

    Code
      dplyr::select(df, -expression)
    Output
      # A tibble: 1 x 10
        statistic    df df.error   p.value
            <dbl> <dbl>    <dbl>     <dbl>
      1      20.2     2     19.0 0.0000196
        method                                                   estimate conf.level
        <chr>                                                       <dbl>      <dbl>
      1 One-way analysis of means (not assuming equal variances)    0.681       0.95
        conf.low conf.high effectsize
           <dbl>     <dbl> <chr>     
      1    0.377     0.813 Eta2      

---

    Code
      df$expression[[1]]
    Output
      paste(italic("F")["Welch"], "(", "2", ",", "18.97383", ") = ", 
          "20.24946", ", ", italic("p"), " = ", "2e-05", ", ", widehat(eta["p"]^2), 
          " = ", "0.68097", ", CI"["95%"], " [", "0.37741", ", ", "0.81259", 
          "]", ", ", italic("n")["obs"], " = ", "32")

# parametric anova subtitles with partial omega-squared

    Code
      dplyr::select(df1, -expression)
    Output
      # A tibble: 1 x 10
        statistic    df df.error p.value
            <dbl> <dbl>    <dbl>   <dbl>
      1      2.63     3     11.1   0.102
        method                                                   estimate conf.level
        <chr>                                                       <dbl>      <dbl>
      1 One-way analysis of means (not assuming equal variances)    0.245       0.95
        conf.low conf.high effectsize
           <dbl>     <dbl> <chr>     
      1        0     0.539 Omega2    

---

    Code
      df1$expression[[1]]
    Output
      paste(italic("F")["Welch"], "(", "3", ",", "11.1010", ") = ", 
          "2.6325", ", ", italic("p"), " = ", "0.1017", ", ", widehat(omega["p"]^2), 
          " = ", "0.2449", ", CI"["95%"], " [", "0.0000", ", ", "0.5391", 
          "]", ", ", italic("n")["obs"], " = ", "56")

# parametric anova subtitles with partial eta-squared and data with NAs

    Code
      dplyr::select(df1, -expression)
    Output
      # A tibble: 1 x 10
        statistic    df df.error p.value method                    estimate conf.level
            <dbl> <dbl>    <dbl>   <dbl> <chr>                        <dbl>      <dbl>
      1      4.14     3       52  0.0105 One-way analysis of means    0.193       0.95
        conf.low conf.high effectsize
           <dbl>     <dbl> <chr>     
      1   0.0139     0.355 Eta2      

---

    Code
      df1$expression[[1]]
    Output
      paste(italic("F")["Fisher"], "(", "3", ",", "52", ") = ", "4.1361", 
          ", ", italic("p"), " = ", "0.0105", ", ", widehat(eta["p"]^2), 
          " = ", "0.1926", ", CI"["95%"], " [", "0.0139", ", ", "0.3555", 
          "]", ", ", italic("n")["obs"], " = ", "56")

# paired parametric anova subtitles work (without NAs)

    Code
      dplyr::select(df1, -expression)
    Output
      # A tibble: 1 x 14
        term      sumsq sum.squares.error    df df.error meansq statistic  p.value
        <chr>     <dbl>             <dbl> <dbl>    <dbl>  <dbl>     <dbl>    <dbl>
      1 condition 1656.              318.  1.15     171.   1.86      776. 1.32e-69
        method                                              estimate conf.level
        <chr>                                                  <dbl>      <dbl>
      1 ANOVA estimation for factorial designs using 'afex'    0.707       0.99
        conf.low conf.high effectsize      
           <dbl>     <dbl> <chr>           
      1    0.653     0.750 Omega2 (partial)

---

    Code
      df1$expression[[1]]
    Output
      paste(italic("F")["Fisher"], "(", "1.149", ",", "171.217", ") = ", 
          "776.318", ", ", italic("p"), " = ", "1.32e-69", ", ", widehat(omega["p"]^2), 
          " = ", "0.707", ", CI"["99%"], " [", "0.653", ", ", "0.750", 
          "]", ", ", italic("n")["pairs"], " = ", "150")

# too few obs

    Code
      p_sub$expression[[1]]
    Output
      paste(italic("F")["Fisher"], "(", "6.00", ",", "24.00", ") = ", 
          "43.14", ", ", italic("p"), " = ", "1.08e-11", ", ", widehat(eta["p"]^2), 
          " = ", "0.92", ", CI"["95%"], " [", "0.83", ", ", "0.95", 
          "]", ", ", italic("n")["pairs"], " = ", "5")

