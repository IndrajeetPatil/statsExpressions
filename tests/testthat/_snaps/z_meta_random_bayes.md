# meta_analysis works - bayesian

    Code
      dplyr::select(df, -expression)
    Output
      # A tibble: 2 x 16
        term    effectsize                       estimate std.error conf.level
        <chr>   <chr>                               <dbl>     <dbl>      <dbl>
      1 Overall meta-analytic posterior estimate    0.596     0.133       0.95
      2 tau     meta-analytic posterior estimate    0.270     0.122       0.95
        conf.low conf.high  bf10 component prior.distribution prior.location
           <dbl>     <dbl> <dbl> <chr>     <chr>                       <dbl>
      1    0.313     0.847  36.1 meta      Student's t                     0
      2    0.132     0.579  36.1 meta      Inverse gamma                   1
        prior.scale method                                 conf.method log_e_bf10
              <dbl> <chr>                                  <chr>            <dbl>
      1       0.707 Bayesian meta-analysis using 'metaBMA' ETI               3.59
      2       0.15  Bayesian meta-analysis using 'metaBMA' ETI               3.59
        n.obs
        <int>
      1     5
      2     5

---

    Code
      df$expression
    Output
      [[1]]
      list(log[e] * (BF["01"]) == "-3.587", widehat(delta)["difference"]^"posterior" == 
          "0.596", CI["95%"]^ETI ~ "[" * "0.313", "0.847" * "]", italic("r")["Cauchy"]^"JZS" == 
          "0.707")
      
      [[2]]
      list(log[e] * (BF["01"]) == "-3.587", widehat(delta)["difference"]^"posterior" == 
          "0.270", CI["95%"]^ETI ~ "[" * "0.132", "0.579" * "]", italic("r")["Cauchy"]^"JZS" == 
          "0.150")
      

