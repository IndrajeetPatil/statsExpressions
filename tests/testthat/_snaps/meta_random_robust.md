# meta_analysis works - robust

    Code
      select(df, -expression)
    Output
      # A tibble: 1 x 11
        term    effectsize                     estimate std.error conf.low conf.high
        <chr>   <chr>                             <dbl>     <dbl>    <dbl>     <dbl>
      1 Overall meta-analytic summary estimate   -0.693     0.368    -1.56    -0.118
        statistic p.value conf.level method                                n.obs
            <dbl>   <dbl>      <dbl> <chr>                                 <int>
      1     -1.88  0.0230       0.95 Robust meta-analysis using 'metaplus'     6

---

    Code
      df$expression
    Output
      [[1]]
      list(italic("z") == "-1.88", italic(p) == "0.02", widehat(beta)["summary"]^"meta" == 
          "-0.69", CI["95%"] ~ "[" * "-1.56", "-0.12" * "]", italic("n")["effects"] == 
          "6")
      

