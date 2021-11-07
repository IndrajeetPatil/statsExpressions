# meta_analysis works - parametric

    Code
      select(df, -expression)
    Output
      # A tibble: 1 x 10
        term    effectsize                     estimate std.error conf.level conf.low
        <chr>   <chr>                             <dbl>     <dbl>      <dbl>    <dbl>
      1 Overall meta-analytic summary estimate    0.438     0.202       0.95   0.0423
        conf.high statistic p.value method                       
            <dbl>     <dbl>   <dbl> <chr>                        
      1     0.833      2.17  0.0300 Meta-analysis using 'metafor'

---

    Code
      df$expression[[1]]
    Output
      paste(italic("z"), " = ", "2.17", ", ", italic("p"), " = ", "0.030", 
          ", ", widehat(beta)["summary"]^"meta", " = ", "0.44", ", CI"["95%"], 
          " [", "0.04", ", ", "0.83", "], ", italic("n")["effects"], 
          " = ", "5")

