# corr_test works - nonparametric

    Code
      select(df1, -expression)
    Output
      # A tibble: 1 x 11
        parameter1 parameter2 effectsize           estimate conf.level conf.low
        <chr>      <chr>      <chr>                   <dbl>      <dbl>    <dbl>
      1 rating     length     Spearman correlation    0.495      0.999    0.153
        conf.high statistic    p.value method               n.obs
            <dbl>     <dbl>      <dbl> <chr>                <int>
      1     0.731    41453. 0.00000344 Spearman correlation    79

---

    Code
      as.character(df1$expression[[1]])
    Output
      [1] "list(italic(\"S\") == \"41452.97684\", italic(p) == \"3.44384e-06\", widehat(rho)[\"Spearman\"] == \"0.49546\", CI[\"99.9%\"] ~ \"[\" * \"0.15344\", \"0.73147\" * \"]\", italic(\"n\")[\"pairs\"] == \"79\")"

---

    Code
      select(df2, -expression)
    Output
      # A tibble: 1 x 11
        parameter1 parameter2 effectsize           estimate conf.level conf.low
        <chr>      <chr>      <chr>                   <dbl>      <dbl>    <dbl>
      1 wt         mpg        Spearman correlation   -0.886       0.95   -0.945
        conf.high statistic  p.value method               n.obs
            <dbl>     <dbl>    <dbl> <chr>                <int>
      1    -0.774    10292. 1.49e-11 Spearman correlation    32

---

    Code
      as.character(df2$expression[[1]])
    Output
      [1] "list(italic(\"S\") == \"10292.32\", italic(p) == \"1.49e-11\", widehat(rho)[\"Spearman\"] == \"-0.89\", CI[\"95%\"] ~ \"[\" * \"-0.94\", \"-0.77\" * \"]\", italic(\"n\")[\"pairs\"] == \"32\")"

# corr_test works - parametric

    Code
      select(df, -expression)
    Output
      # A tibble: 1 x 12
        parameter1 parameter2 effectsize          estimate conf.level conf.low
        <chr>      <chr>      <chr>                  <dbl>      <dbl>    <dbl>
      1 brainwt    sleep_rem  Pearson correlation   -0.221        0.9   -0.438
        conf.high statistic df.error p.value method              n.obs
            <dbl>     <dbl>    <int>   <dbl> <chr>               <int>
      1    0.0201     -1.54       46   0.131 Pearson correlation    48

---

    Code
      as.character(df$expression[[1]])
    Output
      [1] "list(italic(\"t\")[\"Student\"] * \"(\" * 46 * \")\" == \"-1.539\", italic(p) == \"0.131\", widehat(italic(\"r\"))[\"Pearson\"] == \"-0.221\", CI[\"90%\"] ~ \"[\" * \"-0.438\", \"0.020\" * \"]\", italic(\"n\")[\"pairs\"] == \"48\")"

# corr_test works - robust

    Code
      select(df, -expression)
    Output
      # A tibble: 1 x 12
        parameter1 parameter2  effectsize                     estimate conf.level
        <chr>      <chr>       <chr>                             <dbl>      <dbl>
      1 brainwt    sleep_total Winsorized Pearson correlation   -0.549        0.5
        conf.low conf.high statistic df.error   p.value method                        
           <dbl>     <dbl>     <dbl>    <int>     <dbl> <chr>                         
      1   -0.611    -0.481     -4.83       54 0.0000117 Winsorized Pearson correlation
        n.obs
        <int>
      1    56

---

    Code
      as.character(df$expression[[1]])
    Output
      [1] "list(italic(\"t\")[\"Student\"] * \"(\" * 54 * \")\" == \"-4.8286\", italic(p) == \"1.1723e-05\", widehat(italic(\"r\"))[\"Winsorized\"] == \"-0.5491\", CI[\"50%\"] ~ \"[\" * \"-0.6106\", \"-0.4812\" * \"]\", italic(\"n\")[\"pairs\"] == \"56\")"

# bayes factor (correlation test) - without NAs

    Code
      as.character(subtitle1$expression[[1]])
    Output
      [1] "list(atop(\"huh is duh\", list(log[e] * (BF[\"01\"]) == \"1.07\", widehat(rho)[\"Pearson\"]^\"posterior\" == \"-0.12\", CI[\"95%\"]^HDI ~ \"[\" * \"-0.28\", \"0.04\" * \"]\", italic(\"r\")[\"beta\"]^\"JZS\" == \"1.41\")))"

# bayes factor (correlation test) - with NAs

    Code
      as.character(subtitle1$expression[[1]])
    Output
      [1] "list(log[e] * (BF[\"01\"]) == \"0.49\", widehat(rho)[\"Pearson\"]^\"posterior\" == \"-0.21\", CI[\"99%\"]^HDI ~ \"[\" * \"-0.47\", \"0.05\" * \"]\", italic(\"r\")[\"beta\"]^\"JZS\" == \"1.25\")"

