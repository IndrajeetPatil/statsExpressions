# meta_analysis works - robust

    Code
      select(df, -expression)
    Output
      # A tibble: 1 x 13
        term    effectsize                     estimate std.error conf.low conf.high
        <chr>   <chr>                             <dbl>     <dbl>    <dbl>     <dbl>
      1 Overall meta-analytic summary estimate   -0.746     0.234    -1.26    -0.343
        statistic  p.value weight conf.level method                               
            <dbl>    <dbl>  <dbl>      <dbl> <chr>                                
      1     -3.20 0.000501     NA       0.95 Robust meta-analysis using 'metaplus'
        conf.method n.obs
        <chr>       <int>
      1 Wald           16

---

    Code
      df[["expression"]]
    Output
      [[1]]
      list(italic("z") == "-3.20", italic(p) == "5.01e-04", widehat(beta)["summary"]^"meta" == 
          "-0.75", CI["95%"] ~ "[" * "-1.26", "-0.34" * "]", italic("n")["effects"] == 
          "16")
      

