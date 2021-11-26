# bayes factor (between-subjects - anova)

    Code
      dplyr::select(df1, -expression)
    Output
      # A tibble: 7 x 16
        term            pd rope.percentage prior.distribution prior.location
        <chr>        <dbl>           <dbl> <chr>                       <dbl>
      1 mu           0.940          0.168  cauchy                          0
      2 vore-carni   0.679          0.293  cauchy                          0
      3 vore-herbi   0.944          0.108  cauchy                          0
      4 vore-insecti 0.688          0.273  cauchy                          0
      5 vore-omni    0.646          0.349  cauchy                          0
      6 sig2         1              0      cauchy                          0
      7 g_vore       1              0.0174 cauchy                          0
        prior.scale  bf10 method                          log_e_bf10
              <dbl> <dbl> <chr>                                <dbl>
      1        0.99 0.118 Bayes factors for linear models      -2.14
      2        0.99 0.118 Bayes factors for linear models      -2.14
      3        0.99 0.118 Bayes factors for linear models      -2.14
      4        0.99 0.118 Bayes factors for linear models      -2.14
      5        0.99 0.118 Bayes factors for linear models      -2.14
      6        0.99 0.118 Bayes factors for linear models      -2.14
      7        0.99 0.118 Bayes factors for linear models      -2.14
        effectsize         estimate std.dev conf.level conf.low conf.high n.obs
        <chr>                 <dbl>   <dbl>      <dbl>    <dbl>     <dbl> <int>
      1 Bayesian R-squared        0       0       0.95        0    0.0800    51
      2 Bayesian R-squared        0       0       0.95        0    0.0800    51
      3 Bayesian R-squared        0       0       0.95        0    0.0800    51
      4 Bayesian R-squared        0       0       0.95        0    0.0800    51
      5 Bayesian R-squared        0       0       0.95        0    0.0800    51
      6 Bayesian R-squared        0       0       0.95        0    0.0800    51
      7 Bayesian R-squared        0       0       0.95        0    0.0800    51

---

    Code
      as.character(df1$expression[[1]])
    Output
      [1] "list(log[e] * (BF[\"01\"]) == \"2.139\", widehat(italic(R^\"2\"))[\"Bayesian\"]^\"posterior\" == \"0.000\", CI[\"95%\"]^HDI ~ \"[\" * \"0.000\", \"0.080\" * \"]\", italic(\"r\")[\"Cauchy\"]^\"JZS\" == \"0.990\")"

---

    Code
      dplyr::select(df2, -expression)
    Output
      # A tibble: 6 x 16
        term                  pd rope.percentage prior.distribution prior.location
        <chr>              <dbl>           <dbl> <chr>                       <dbl>
      1 mu                 1               0     cauchy                          0
      2 Species-setosa     1               0     cauchy                          0
      3 Species-versicolor 0.936           0.435 cauchy                          0
      4 Species-virginica  1               0     cauchy                          0
      5 sig2               1               0     cauchy                          0
      6 g_Species          1               0     cauchy                          0
        prior.scale    bf10 method                          log_e_bf10
              <dbl>   <dbl> <chr>                                <dbl>
      1       0.707 1.87e28 Bayes factors for linear models       65.1
      2       0.707 1.87e28 Bayes factors for linear models       65.1
      3       0.707 1.87e28 Bayes factors for linear models       65.1
      4       0.707 1.87e28 Bayes factors for linear models       65.1
      5       0.707 1.87e28 Bayes factors for linear models       65.1
      6       0.707 1.87e28 Bayes factors for linear models       65.1
        effectsize         estimate std.dev conf.level conf.low conf.high n.obs
        <chr>                 <dbl>   <dbl>      <dbl>    <dbl>     <dbl> <int>
      1 Bayesian R-squared    0.612  0.0311       0.99    0.511     0.679   150
      2 Bayesian R-squared    0.612  0.0311       0.99    0.511     0.679   150
      3 Bayesian R-squared    0.612  0.0311       0.99    0.511     0.679   150
      4 Bayesian R-squared    0.612  0.0311       0.99    0.511     0.679   150
      5 Bayesian R-squared    0.612  0.0311       0.99    0.511     0.679   150
      6 Bayesian R-squared    0.612  0.0311       0.99    0.511     0.679   150

---

    Code
      as.character(df2$expression[[1]])
    Output
      [1] "list(log[e] * (BF[\"01\"]) == \"-65.0969\", widehat(italic(R^\"2\"))[\"Bayesian\"]^\"posterior\" == \"0.6118\", CI[\"99%\"]^HDI ~ \"[\" * \"0.5107\", \"0.6789\" * \"]\", italic(\"r\")[\"Cauchy\"]^\"JZS\" == \"0.7070\")"

# bayes factor (within-subjects - anova)

    Code
      dplyr::select(df1, -expression)
    Output
      # A tibble: 7 x 18
        term           pd rope.percentage prior.distribution prior.location
        <chr>       <dbl>           <dbl> <chr>                       <dbl>
      1 mu          1              0      cauchy                          0
      2 Wine-Wine A 0.97           0.473  cauchy                          0
      3 Wine-Wine B 0.906          0.688  cauchy                          0
      4 Wine-Wine C 0.998          0.0755 cauchy                          0
      5 sig2        1              1      cauchy                          0
      6 g_Wine      1              0      cauchy                          0
      7 g_rowid     1              0      cauchy                          0
        prior.scale effect  bf10 method                          log_e_bf10
              <dbl> <chr>  <dbl> <chr>                                <dbl>
      1        0.88 fixed   7.09 Bayes factors for linear models       1.96
      2        0.88 fixed   7.09 Bayes factors for linear models       1.96
      3        0.88 fixed   7.09 Bayes factors for linear models       1.96
      4        0.88 fixed   7.09 Bayes factors for linear models       1.96
      5        1    fixed   7.09 Bayes factors for linear models       1.96
      6        1    fixed   7.09 Bayes factors for linear models       1.96
      7        1    fixed   7.09 Bayes factors for linear models       1.96
        effectsize         estimate std.dev conf.level conf.low conf.high component  
        <chr>                 <dbl>   <dbl>      <dbl>    <dbl>     <dbl> <chr>      
      1 Bayesian R-squared    0.893  0.0176       0.95    0.847     0.920 conditional
      2 Bayesian R-squared    0.893  0.0176       0.95    0.847     0.920 conditional
      3 Bayesian R-squared    0.893  0.0176       0.95    0.847     0.920 conditional
      4 Bayesian R-squared    0.893  0.0176       0.95    0.847     0.920 conditional
      5 Bayesian R-squared    0.893  0.0176       0.95    0.847     0.920 conditional
      6 Bayesian R-squared    0.893  0.0176       0.95    0.847     0.920 conditional
      7 Bayesian R-squared    0.893  0.0176       0.95    0.847     0.920 conditional
        n.obs
        <int>
      1    22
      2    22
      3    22
      4    22
      5    22
      6    22
      7    22

---

    Code
      as.character(df1$expression[[1]])
    Output
      [1] "list(log[e] * (BF[\"01\"]) == \"-1.96\", widehat(italic(R^\"2\"))[\"Bayesian\"]^\"posterior\" == \"0.89\", CI[\"95%\"]^HDI ~ \"[\" * \"0.85\", \"0.92\" * \"]\", italic(\"r\")[\"Cauchy\"]^\"JZS\" == \"0.88\")"

---

    Code
      dplyr::select(df2, -expression)
    Output
      # A tibble: 8 x 18
        term              pd rope.percentage prior.distribution prior.location
        <chr>          <dbl>           <dbl> <chr>                       <dbl>
      1 mu             1               0     cauchy                          0
      2 condition-HDHF 1               0     cauchy                          0
      3 condition-HDLF 0.862           0.715 cauchy                          0
      4 condition-LDHF 0.995           0.139 cauchy                          0
      5 condition-LDLF 1               0     cauchy                          0
      6 sig2           1               0     cauchy                          0
      7 g_condition    1               0.401 cauchy                          0
      8 g_rowid        1               0     cauchy                          0
        prior.scale effect        bf10 method                          log_e_bf10
              <dbl> <chr>        <dbl> <chr>                                <dbl>
      1       0.707 fixed  1372773375. Bayes factors for linear models       21.0
      2       0.707 fixed  1372773375. Bayes factors for linear models       21.0
      3       0.707 fixed  1372773375. Bayes factors for linear models       21.0
      4       0.707 fixed  1372773375. Bayes factors for linear models       21.0
      5       0.707 fixed  1372773375. Bayes factors for linear models       21.0
      6       1     fixed  1372773375. Bayes factors for linear models       21.0
      7       1     fixed  1372773375. Bayes factors for linear models       21.0
      8       1     fixed  1372773375. Bayes factors for linear models       21.0
        effectsize         estimate std.dev conf.level conf.low conf.high component  
        <chr>                 <dbl>   <dbl>      <dbl>    <dbl>     <dbl> <chr>      
      1 Bayesian R-squared    0.529  0.0330       0.95    0.460     0.586 conditional
      2 Bayesian R-squared    0.529  0.0330       0.95    0.460     0.586 conditional
      3 Bayesian R-squared    0.529  0.0330       0.95    0.460     0.586 conditional
      4 Bayesian R-squared    0.529  0.0330       0.95    0.460     0.586 conditional
      5 Bayesian R-squared    0.529  0.0330       0.95    0.460     0.586 conditional
      6 Bayesian R-squared    0.529  0.0330       0.95    0.460     0.586 conditional
      7 Bayesian R-squared    0.529  0.0330       0.95    0.460     0.586 conditional
      8 Bayesian R-squared    0.529  0.0330       0.95    0.460     0.586 conditional
        n.obs
        <int>
      1    88
      2    88
      3    88
      4    88
      5    88
      6    88
      7    88
      8    88

---

    Code
      as.character(df2$expression[[1]])
    Output
      [1] "list(log[e] * (BF[\"01\"]) == \"-21.04\", widehat(italic(R^\"2\"))[\"Bayesian\"]^\"posterior\" == \"0.53\", CI[\"95%\"]^HDI ~ \"[\" * \"0.46\", \"0.59\" * \"]\", italic(\"r\")[\"Cauchy\"]^\"JZS\" == \"0.71\")"

