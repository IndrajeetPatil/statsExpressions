# meta_analysis works - parametric

    Code
      dplyr::select(df, -expression)
    Output
      # A tibble: 1 x 11
        term    estimate std.error conf.level conf.low conf.high statistic p.value
        <chr>      <dbl>     <dbl>      <dbl>    <dbl>     <dbl>     <dbl>   <dbl>
      1 Overall    0.438     0.202       0.95   0.0423     0.833      2.17  0.0300
        weight method                        effectsize                    
         <dbl> <chr>                         <chr>                         
      1     NA Meta-analysis using 'metafor' meta-analytic summary estimate

---

    Code
      df$expression[[1]]
    Output
      paste(italic("z"), " = ", "2.17", ", ", italic("p"), " = ", "0.030", 
          ", ", widehat(beta)["summary"]^"meta", " = ", "0.44", ", CI"["95%"], 
          " [", "0.04", ", ", "0.83", "], ", italic("n")["effects"], 
          " = ", "5")

