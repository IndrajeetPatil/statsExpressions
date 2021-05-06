# parametric t-test works (between-subjects without NAs)

    Code
      dplyr::select(df1, -expression)
    Output
      # A tibble: 1 x 13
        term   group mean.group1 mean.group2 statistic df.error  p.value
        <chr>  <chr>       <dbl>       <dbl>     <dbl>    <dbl>    <dbl>
      1 rating genre        5.43        6.55     -10.5      612 6.10e-24
        method            estimate conf.level conf.low conf.high effectsize
        <chr>                <dbl>      <dbl>    <dbl>     <dbl> <chr>     
      1 Two Sample t-test   -0.925       0.99    -1.16    -0.688 Cohen's d 

---

    Code
      df1$expression[[1]]
    Output
      paste(italic("t")["Student"], "(", "612", ") = ", "-10.52948", 
          ", ", italic("p"), " = ", "6.0984e-24", ", ", widehat(italic("d"))["Cohen"], 
          " = ", "-0.92473", ", CI"["99%"], " [", "-1.16064", ", ", 
          "-0.68822", "]", ", ", italic("n")["obs"], " = ", "614")

# parametric t-test works (between-subjects with NAs)

    Code
      dplyr::select(df1, -expression)
    Output
      # A tibble: 1 x 13
        term   group mean.group1 mean.group2 statistic df.error  p.value
        <chr>  <chr>       <dbl>       <dbl>     <dbl>    <dbl>    <dbl>
      1 rating genre        5.43        6.55     -9.27     271. 5.80e-18
        method                  estimate conf.level conf.low conf.high effectsize
        <chr>                      <dbl>      <dbl>    <dbl>     <dbl> <chr>     
      1 Welch Two Sample t-test   -0.924        0.9    -1.07    -0.773 Hedges' g 

---

    Code
      df1$expression[[1]]
    Output
      paste(italic("t")["Welch"], "(", "271.302", ") = ", "-9.275", 
          ", ", italic("p"), " = ", "5.8e-18", ", ", widehat(italic("g"))["Hedges"], 
          " = ", "-0.924", ", CI"["90%"], " [", "-1.074", ", ", "-0.773", 
          "]", ", ", italic("n")["obs"], " = ", "614")

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

