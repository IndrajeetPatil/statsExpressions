# meta_analysis works - robust

    Code
      select(df, -expression)
    Output
      # A tibble: 1 x 11
        term    estimate std.error conf.low conf.high statistic p.value weight
        <chr>      <dbl>     <dbl>    <dbl>     <dbl>     <dbl>   <dbl>  <dbl>
      1 Overall   -0.693     0.368    -1.56    -0.118     -1.88  0.0230     NA
        conf.level method                               
             <dbl> <chr>                                
      1       0.95 Robust meta-analysis using 'metaplus'
        effectsize                    
        <chr>                         
      1 meta-analytic summary estimate

---

    Code
      df$expression[[1]]
    Output
      paste(italic("z"), " = ", "-1.88", ", ", italic("p"), " = ", 
          "0.023", ", ", widehat(beta)["summary"]^"meta", " = ", "-0.69", 
          ", CI"["95%"], " [", "-1.56", ", ", "-0.12", "], ", italic("n")["effects"], 
          " = ", "6")

