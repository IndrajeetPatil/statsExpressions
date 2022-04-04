# meta_analysis works - bayesian

    Code
      dplyr::select(df, -expression)
    Output
      # A tibble: 2 x 15
        term    effectsize      estimate std.error conf.level conf.low conf.high  bf10
        <chr>   <chr>              <dbl>     <dbl>      <dbl>    <dbl>     <dbl> <dbl>
      1 Overall meta-analytic ~    0.596     0.133       0.95    0.313     0.847  36.1
      2 tau     meta-analytic ~    0.270     0.122       0.95    0.132     0.579  36.1
      # ... with 7 more variables: component <chr>, prior.distribution <chr>,
      #   prior.location <dbl>, prior.scale <dbl>, method <chr>, log_e_bf10 <dbl>,
      #   n.obs <int>

---

    Code
      as.character(df$expression[[1]])
    Output
      [1] "list(atop(\"ayyo arecha\", list(log[e] * (BF[\"01\"]) == \"-3.587\", widehat(delta)[\"difference\"]^\"posterior\" == \"0.596\", CI[\"95%\"]^HDI ~ \"[\" * \"0.313\", \"0.847\" * \"]\", italic(\"r\")[\"Cauchy\"]^\"JZS\" == \"0.707\")))"

