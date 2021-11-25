# meta_analysis works - bayesian

    Code
      dplyr::select(df, -expression)
    Output
      # A tibble: 2 x 15
        term    effectsize                       estimate std.error conf.level
        <chr>   <chr>                               <dbl>     <dbl>      <dbl>
      1 Overall meta-analytic posterior estimate    0.596     0.133       0.95
      2 tau     meta-analytic posterior estimate    0.270     0.122       0.95
        conf.low conf.high  bf10 component prior.distribution prior.location
           <dbl>     <dbl> <dbl> <chr>     <chr>                       <dbl>
      1    0.321     0.854  36.1 meta      Student's t                     0
      2    0.106     0.496  36.1 meta      Inverse gamma                   1
        prior.scale method                                 log_e_bf10 n.obs
              <dbl> <chr>                                       <dbl> <int>
      1       0.707 Bayesian meta-analysis using 'metaBMA'       3.59     5
      2       0.15  Bayesian meta-analysis using 'metaBMA'       3.59     5

---

    Code
      as.character(df$expression[[1]])
    Output
      [1] "list(atop(\"ayyo arecha\", list(log[e] * (BF[\"01\"]) == \"-3.587\", italic(delta)[difference]^\"posterior\" == \"0.596\", CI[\"95%\"]^HDI ~ \"[\" * \"0.321\", \"0.854\" * \"]\", italic(\"r\")[\"Cauchy\"]^\"JZS\" == \"0.707\")))"

