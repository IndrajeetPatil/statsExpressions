# meta_analysis works - bayesian

    Code
      dplyr::select(df, -expression)
    Output
      # A tibble: 2 x 19
        term    effectsize                       estimate std.error conf.level
        <chr>   <chr>                               <dbl>     <dbl>      <dbl>
      1 Overall meta-analytic posterior estimate   -0.650     0.222       0.95
      2 tau     meta-analytic posterior estimate    0.486     0.184       0.95
        conf.low conf.high weight  bf10  rhat   ess component prior.distribution
           <dbl>     <dbl>  <dbl> <dbl> <dbl> <dbl> <chr>     <chr>             
      1   -1.12     -0.251     NA  53.0    NA    NA meta      Student's t       
      2    0.205     0.917     NA  53.0    NA    NA meta      Inverse gamma     
        prior.location prior.scale method                                 conf.method
                 <dbl>       <dbl> <chr>                                  <chr>      
      1              0       0.707 Bayesian meta-analysis using 'metaBMA' ETI        
      2              1       0.15  Bayesian meta-analysis using 'metaBMA' ETI        
        log_e_bf10 n.obs
             <dbl> <int>
      1       3.97    16
      2       3.97    16

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
      

