# meta_analysis works - bayesian

    Code
      dplyr::select(df, -expression)
    Output
      # A tibble: 2 x 16
        term    effectsize                       estimate std.error conf.level
        <chr>   <chr>                               <dbl>     <dbl>      <dbl>
      1 Overall meta-analytic posterior estimate   -0.650     0.222       0.95
      2 tau     meta-analytic posterior estimate    0.486     0.184       0.95
        conf.low conf.high  bf10 component prior.distribution prior.location
           <dbl>     <dbl> <dbl> <chr>     <chr>                       <dbl>
      1   -1.12     -0.251  53.0 meta      Student's t                     0
      2    0.205     0.917  53.0 meta      Inverse gamma                   1
        prior.scale method                                 conf.method log_e_bf10
              <dbl> <chr>                                  <chr>            <dbl>
      1       0.707 Bayesian meta-analysis using 'metaBMA' ETI               3.97
      2       0.15  Bayesian meta-analysis using 'metaBMA' ETI               3.97
        n.obs
        <int>
      1    16
      2    16

---

    Code
      df[["expression"]]
    Output
      [[1]]
      list(log[e] * (BF["01"]) == "-3.970", widehat(delta)["difference"]^"posterior" == 
          "-0.650", CI["95%"]^ETI ~ "[" * "-1.121", "-0.251" * "]", 
          italic("r")["Cauchy"]^"JZS" == "0.707")
      
      [[2]]
      list(log[e] * (BF["01"]) == "-3.970", widehat(delta)["difference"]^"posterior" == 
          "0.486", CI["95%"]^ETI ~ "[" * "0.205", "0.917" * "]", italic("r")["Cauchy"]^"JZS" == 
          "0.150")
      

