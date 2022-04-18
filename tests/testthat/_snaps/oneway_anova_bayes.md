# bayesian (between-subjects - anova)

    Code
      dplyr::select(df1, -dplyr::matches("low$|high$"))
    Output
      # A tibble: 7 x 15
        term            pd rope.percentage prior.distribution prior.location
        <chr>        <dbl>           <dbl> <chr>                       <dbl>
      1 mu           0.947           0.152 cauchy                          0
      2 vore-carni   0.684           0.321 cauchy                          0
      3 vore-herbi   0.936           0.146 cauchy                          0
      4 vore-insecti 0.662           0.292 cauchy                          0
      5 vore-omni    0.641           0.372 cauchy                          0
      6 sig2         1               0     cauchy                          0
      7 g_vore       1               0.124 cauchy                          0
        prior.scale  bf10 method                          log_e_bf10
              <dbl> <dbl> <chr>                                <dbl>
      1       0.707 0.214 Bayes factors for linear models      -1.54
      2       0.707 0.214 Bayes factors for linear models      -1.54
      3       0.707 0.214 Bayes factors for linear models      -1.54
      4       0.707 0.214 Bayes factors for linear models      -1.54
      5       0.707 0.214 Bayes factors for linear models      -1.54
      6       0.707 0.214 Bayes factors for linear models      -1.54
      7       0.707 0.214 Bayes factors for linear models      -1.54
        effectsize         estimate std.dev conf.level n.obs expression
        <chr>                 <dbl>   <dbl>      <dbl> <int> <list>    
      1 Bayesian R-squared        0       0       0.95    51 <language>
      2 Bayesian R-squared        0       0       0.95    51 <language>
      3 Bayesian R-squared        0       0       0.95    51 <language>
      4 Bayesian R-squared        0       0       0.95    51 <language>
      5 Bayesian R-squared        0       0       0.95    51 <language>
      6 Bayesian R-squared        0       0       0.95    51 <language>
      7 Bayesian R-squared        0       0       0.95    51 <language>

---

    Code
      df1$expression[[1]]
    Output
      list(log[e] * (BF["01"]) == "1.54", widehat(italic(R^"2"))["Bayesian"]^"posterior" == 
          "0.00", CI["95%"]^HDI ~ "[" * "0.00", "0.09" * "]", italic("r")["Cauchy"]^"JZS" == 
          "0.71")

---

    Code
      dplyr::select(df2, -dplyr::matches("low$|high$"))
    Output
      # A tibble: 6 x 15
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
        effectsize         estimate std.dev conf.level n.obs expression
        <chr>                 <dbl>   <dbl>      <dbl> <int> <list>    
      1 Bayesian R-squared    0.612  0.0311       0.95   150 <language>
      2 Bayesian R-squared    0.612  0.0311       0.95   150 <language>
      3 Bayesian R-squared    0.612  0.0311       0.95   150 <language>
      4 Bayesian R-squared    0.612  0.0311       0.95   150 <language>
      5 Bayesian R-squared    0.612  0.0311       0.95   150 <language>
      6 Bayesian R-squared    0.612  0.0311       0.95   150 <language>

---

    Code
      df2$expression[[1]]
    Output
      list(log[e] * (BF["01"]) == "-65.10", widehat(italic(R^"2"))["Bayesian"]^"posterior" == 
          "0.61", CI["95%"]^HDI ~ "[" * "0.54", "0.67" * "]", italic("r")["Cauchy"]^"JZS" == 
          "0.71")

# bayesian (within-subjects - anova)

    Code
      dplyr::select(df1, -dplyr::matches("low$|high$"))
    Output
      # A tibble: 7 x 17
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
        effectsize         estimate std.dev conf.level component   n.obs expression
        <chr>                 <dbl>   <dbl>      <dbl> <chr>       <int> <list>    
      1 Bayesian R-squared    0.893  0.0176       0.95 conditional    22 <language>
      2 Bayesian R-squared    0.893  0.0176       0.95 conditional    22 <language>
      3 Bayesian R-squared    0.893  0.0176       0.95 conditional    22 <language>
      4 Bayesian R-squared    0.893  0.0176       0.95 conditional    22 <language>
      5 Bayesian R-squared    0.893  0.0176       0.95 conditional    22 <language>
      6 Bayesian R-squared    0.893  0.0176       0.95 conditional    22 <language>
      7 Bayesian R-squared    0.893  0.0176       0.95 conditional    22 <language>

---

    Code
      df1$expression[[1]]
    Output
      list(log[e] * (BF["01"]) == "-1.96", widehat(italic(R^"2"))["Bayesian"]^"posterior" == 
          "0.89", CI["95%"]^HDI ~ "[" * "0.85", "0.92" * "]", italic("r")["Cauchy"]^"JZS" == 
          "0.88")

---

    Code
      dplyr::select(df2, -dplyr::matches("low$|high$"))
    Output
      # A tibble: 8 x 17
        term              pd rope.percentage prior.distribution prior.location
        <chr>          <dbl>           <dbl> <chr>                       <dbl>
      1 mu             1               0     cauchy                          0
      2 condition-HDHF 1               0     cauchy                          0
      3 condition-HDLF 0.865           0.694 cauchy                          0
      4 condition-LDHF 0.994           0.145 cauchy                          0
      5 condition-LDLF 1               0     cauchy                          0
      6 sig2           1               0     cauchy                          0
      7 g_condition    1               0.402 cauchy                          0
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
        effectsize         estimate std.dev conf.level component   n.obs expression
        <chr>                 <dbl>   <dbl>      <dbl> <chr>       <int> <list>    
      1 Bayesian R-squared    0.529  0.0331       0.95 conditional    88 <language>
      2 Bayesian R-squared    0.529  0.0331       0.95 conditional    88 <language>
      3 Bayesian R-squared    0.529  0.0331       0.95 conditional    88 <language>
      4 Bayesian R-squared    0.529  0.0331       0.95 conditional    88 <language>
      5 Bayesian R-squared    0.529  0.0331       0.95 conditional    88 <language>
      6 Bayesian R-squared    0.529  0.0331       0.95 conditional    88 <language>
      7 Bayesian R-squared    0.529  0.0331       0.95 conditional    88 <language>
      8 Bayesian R-squared    0.529  0.0331       0.95 conditional    88 <language>

---

    Code
      df2$expression[[1]]
    Output
      list(log[e] * (BF["01"]) == "-21.04", widehat(italic(R^"2"))["Bayesian"]^"posterior" == 
          "0.53", CI["95%"]^HDI ~ "[" * "0.46", "0.59" * "]", italic("r")["Cauchy"]^"JZS" == 
          "0.71")

