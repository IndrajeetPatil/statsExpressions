# bayesian (between-subjects - anova)

    Code
      dplyr::select(df1, 1:3)
    Output
      # A tibble: 7 x 3
        term            pd rope.percentage
        <chr>        <dbl>           <dbl>
      1 mu           0.947           0.152
      2 vore-carni   0.684           0.321
      3 vore-herbi   0.936           0.146
      4 vore-insecti 0.662           0.292
      5 vore-omni    0.641           0.372
      6 sig2         1               0    
      7 g_vore       1               0.124

---

    Code
      df1$expression[[1]]
    Output
      list(log[e] * (BF["01"]) == "1.54", widehat(italic(R^"2"))["Bayesian"]^"posterior" == 
          "0.00", CI["95%"]^HDI ~ "[" * "0.00", "0.09" * "]", italic("r")["Cauchy"]^"JZS" == 
          "0.71")

---

    Code
      dplyr::select(df2, 1:3)
    Output
      # A tibble: 6 x 3
        term                  pd rope.percentage
        <chr>              <dbl>           <dbl>
      1 mu                 1               0    
      2 Species-setosa     1               0    
      3 Species-versicolor 0.936           0.435
      4 Species-virginica  1               0    
      5 sig2               1               0    
      6 g_Species          1               0    

---

    Code
      df2$expression[[1]]
    Output
      list(log[e] * (BF["01"]) == "-65.10", widehat(italic(R^"2"))["Bayesian"]^"posterior" == 
          "0.61", CI["95%"]^HDI ~ "[" * "0.54", "0.67" * "]", italic("r")["Cauchy"]^"JZS" == 
          "0.71")

# bayesian (within-subjects - anova)

    Code
      dplyr::select(df1, 1:3)
    Output
      # A tibble: 7 x 3
        term           pd rope.percentage
        <chr>       <dbl>           <dbl>
      1 mu          1              0     
      2 Wine-Wine A 0.97           0.473 
      3 Wine-Wine B 0.906          0.688 
      4 Wine-Wine C 0.998          0.0755
      5 sig2        1              1     
      6 g_Wine      1              0     
      7 g_rowid     1              0     

---

    Code
      df1$expression[[1]]
    Output
      list(log[e] * (BF["01"]) == "-1.96", widehat(italic(R^"2"))["Bayesian"]^"posterior" == 
          "0.89", CI["95%"]^HDI ~ "[" * "0.85", "0.92" * "]", italic("r")["Cauchy"]^"JZS" == 
          "0.88")

---

    Code
      dplyr::select(df2, 1:3)
    Output
      # A tibble: 8 x 3
        term              pd rope.percentage
        <chr>          <dbl>           <dbl>
      1 mu             1               0    
      2 condition-HDHF 1               0    
      3 condition-HDLF 0.865           0.694
      4 condition-LDHF 0.994           0.145
      5 condition-LDLF 1               0    
      6 sig2           1               0    
      7 g_condition    1               0.402
      8 g_rowid        1               0    

---

    Code
      df2$expression[[1]]
    Output
      list(log[e] * (BF["01"]) == "-21.04", widehat(italic(R^"2"))["Bayesian"]^"posterior" == 
          "0.53", CI["95%"]^HDI ~ "[" * "0.46", "0.59" * "]", italic("r")["Cauchy"]^"JZS" == 
          "0.71")

