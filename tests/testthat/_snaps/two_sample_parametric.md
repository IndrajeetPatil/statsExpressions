# parametric t-test works (between-subjects without NAs)

    Code
      dplyr::select(df1, -expression)
    Output
      # A tibble: 1 x 13
        term  group mean.group1 mean.group2 statistic df.error p.value
        <chr> <chr>       <dbl>       <dbl>     <dbl>    <dbl>   <dbl>
      1 len   supp         20.7        17.0      1.92       58  0.0604
        method            estimate conf.level conf.low conf.high effectsize
        <chr>                <dbl>      <dbl>    <dbl>     <dbl> <chr>     
      1 Two Sample t-test    0.495       0.99   -0.184      1.17 Cohen's d 

---

    Code
      df1$expression[[1]]
    Output
      paste(italic("t")["Student"], "(", "58", ") = ", "1.91527", ", ", 
          italic("p"), " = ", "0.06039", ", ", widehat(italic("d"))["Cohen"], 
          " = ", "0.49452", ", CI"["99%"], " [", "-0.18354", ", ", 
          "1.16839", "]", ", ", italic("n")["obs"], " = ", "60")

# parametric t-test works (between-subjects with NAs)

    Code
      dplyr::select(df1, -expression)
    Output
      # A tibble: 1 x 13
        term  group mean.group1 mean.group2 statistic df.error p.value
        <chr> <chr>       <dbl>       <dbl>     <dbl>    <dbl>   <dbl>
      1 len   supp         20.7        17.0      1.92     55.3  0.0606
        method                  estimate conf.level conf.low conf.high effectsize
        <chr>                      <dbl>      <dbl>    <dbl>     <dbl> <chr>     
      1 Welch Two Sample t-test    0.488        0.9   0.0599     0.911 Hedges' g 

---

    Code
      df1$expression[[1]]
    Output
      paste(italic("t")["Welch"], "(", "55.309", ") = ", "1.915", ", ", 
          italic("p"), " = ", "0.061", ", ", widehat(italic("g"))["Hedges"], 
          " = ", "0.488", ", CI"["90%"], " [", "0.060", ", ", "0.911", 
          "]", ", ", italic("n")["obs"], " = ", "60")

# parametric t-test works (within-subjects without NAs)

    Code
      dplyr::select(df1, -expression)
    Output
      # A tibble: 1 x 11
        term  group     statistic df.error  p.value method        estimate conf.level
        <chr> <chr>         <dbl>    <dbl>    <dbl> <chr>            <dbl>      <dbl>
      1 value condition      34.8      149 1.85e-73 Paired t-test     2.83        0.5
        conf.low conf.high effectsize
           <dbl>     <dbl> <chr>     
      1     2.71      2.96 Hedges' g 

---

    Code
      df1$expression[[1]]
    Output
      paste(italic("t")["Student"], "(", "149", ") = ", "34.8152", 
          ", ", italic("p"), " = ", "1.85e-73", ", ", widehat(italic("g"))["Hedges"], 
          " = ", "2.8283", ", CI"["50%"], " [", "2.7086", ", ", "2.9560", 
          "]", ", ", italic("n")["pairs"], " = ", "150")

# parametric t-test works (within-subjects with NAs)

    Code
      dplyr::select(df1, -expression)
    Output
      # A tibble: 1 x 11
        term   group     statistic df.error  p.value method        estimate conf.level
        <chr>  <chr>         <dbl>    <dbl>    <dbl> <chr>            <dbl>      <dbl>
      1 desire condition      3.61       89 0.000500 Paired t-test    0.381       0.95
        conf.low conf.high effectsize
           <dbl>     <dbl> <chr>     
      1    0.167     0.597 Cohen's d 

---

    Code
      df1$expression[[1]]
    Output
      paste(italic("t")["Student"], "(", "89", ") = ", "3.613", ", ", 
          italic("p"), " = ", "5e-04", ", ", widehat(italic("d"))["Cohen"], 
          " = ", "0.381", ", CI"["95%"], " [", "0.167", ", ", "0.597", 
          "]", ", ", italic("n")["pairs"], " = ", "90")

