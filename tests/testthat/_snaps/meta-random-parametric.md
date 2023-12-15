# meta_analysis works - parametric

    Code
      select(df, -expression)
    Output
      # A tibble: 1 x 13
        term    effectsize                     estimate std.error conf.level conf.low
        <chr>   <chr>                             <dbl>     <dbl>      <dbl>    <dbl>
      1 Overall meta-analytic summary estimate   -0.767     0.212       0.95    -1.18
        conf.high statistic  p.value weight method                        conf.method
            <dbl>     <dbl>    <dbl>  <dbl> <chr>                         <chr>      
      1    -0.351     -3.62 0.000295     NA Meta-analysis using 'metafor' Wald       
        n.obs
        <int>
      1    16

---

    Code
      df[["expression"]]
    Output
      [[1]]
      list(italic("z") == "-3.62", italic(p) == "2.95e-04", widehat(beta)["summary"]^"meta" == 
          "-0.77", CI["95%"] ~ "[" * "-1.18", "-0.35" * "]", italic("n")["effects"] == 
          "16")
      

