# tidy_model_expressions works - t

    Code
      select(df_t, -expression)
    Output
      # A tibble: 2 x 10
        term        estimate std.error conf.level conf.low conf.high statistic
        <chr>          <dbl>     <dbl>      <dbl>    <dbl>     <dbl>     <dbl>
      1 (Intercept)    6.05     0.309        0.95    5.42      6.68      19.6 
      2 mpg           -0.141    0.0147       0.95   -0.171    -0.111     -9.56
        df.error  p.value conf.method
           <int>    <dbl> <chr>      
      1       30 1.20e-18 Wald       
      2       30 1.29e-10 Wald       

---

    Code
      df_t[["expression"]]
    Output
      [[1]]
      list(widehat(italic(beta)) == "6.05", italic(t)("30") == "19.59", 
          italic(p) == "1.20e-18")
      
      [[2]]
      list(widehat(italic(beta)) == "-0.14", italic(t)("30") == "-9.56", 
          italic(p) == "1.29e-10")
      

---

    Code
      df_t_na[["expression"]]
    Output
      [[1]]
      list(widehat(italic(beta)) == "6.05", italic(t) == "19.59", italic(p) == 
          "1.20e-18")
      
      [[2]]
      list(widehat(italic(beta)) == "-0.14", italic(t) == "-9.56", 
          italic(p) == "1.29e-10")
      

---

    Code
      df_t_inf[["expression"]]
    Output
      [[1]]
      list(widehat(italic(beta)) == "6.05", italic(t) == "19.59", italic(p) == 
          "1.20e-18")
      
      [[2]]
      list(widehat(italic(beta)) == "-0.14", italic(t) == "-9.56", 
          italic(p) == "1.29e-10")
      

# tidy_model_expressions works - chi2

    Code
      select(df_chi, -expression)
    Output
      # A tibble: 2 x 10
        term  estimate std.error conf.level conf.low conf.high statistic df.error
        <chr>    <dbl>     <dbl>      <dbl>    <dbl>     <dbl>     <dbl>    <dbl>
      1 age     0.0170   0.00923       0.95 -0.00106    0.0351      3.40        1
      2 sex    -0.512    0.168         0.95 -0.840     -0.183       9.31        1
        p.value conf.method
          <dbl> <chr>      
      1 0.0650  Wald       
      2 0.00228 Wald       

---

    Code
      df_chi[["expression"]]
    Output
      [[1]]
      list(widehat(italic(beta)) == "0.02", italic(chi)^2 * ("1") == 
          "3.40", italic(p) == "0.07")
      
      [[2]]
      list(widehat(italic(beta)) == "-0.51", italic(chi)^2 * ("1") == 
          "9.31", italic(p) == "2.28e-03")
      

# tidy_model_expressions works - z

    Code
      select(df_z, -expression)
    Output
      # A tibble: 3 x 10
        term        estimate std.error conf.level conf.low conf.high statistic
        <chr>          <dbl>     <dbl>      <dbl>    <dbl>     <dbl>     <dbl>
      1 (Intercept)   -0.780     0.225       0.95    -1.22    -0.342     -3.47
      2 SexFemale      2.29      0.120       0.95     2.06     2.53      19.1 
      3 AgeAdult      -0.556     0.228       0.95    -1.00    -0.108     -2.44
        df.error  p.value conf.method
           <dbl>    <dbl> <chr>      
      1      Inf 5.14e- 4 profile    
      2      Inf 1.54e-81 profile    
      3      Inf 1.45e- 2 profile    

---

    Code
      df_z[["expression"]]
    Output
      [[1]]
      list(widehat(italic(beta)) == "-0.78", italic(z) == "-3.47", 
          italic(p) == "5.14e-04")
      
      [[2]]
      list(widehat(italic(beta)) == "2.29", italic(z) == "19.13", italic(p) == 
          "1.54e-81")
      
      [[3]]
      list(widehat(italic(beta)) == "-0.56", italic(z) == "-2.44", 
          italic(p) == "0.01")
      

# tidy_model_expressions works - F

    Code
      select(df1, -expression)
    Output
      # A tibble: 3 x 11
        term  statistic    df df.error p.value group   sumsq meansq  estimate
        <chr>     <dbl> <dbl>    <dbl>   <dbl> <chr>   <dbl>  <dbl>     <dbl>
      1 N         9.04      1       15 0.00885 Within 189.   189.    0.184   
      2 P         0.401     1       15 0.536   Within   8.40   8.40 -0.0171  
      3 N:P       1.02      1       15 0.329   Within  21.3   21.3   0.000457
        sum.squares.error mean.square.error
                    <dbl>             <dbl>
      1              314.              314.
      2              314.              314.
      3              314.              314.

---

    Code
      df1[["expression"]]
    Output
      [[1]]
      list(widehat(italic(omega)[p]^2) == "0.18", italic(F)("1", "15") == 
          "9.04", italic(p) == "8.85e-03")
      
      [[2]]
      list(widehat(italic(omega)[p]^2) == "-0.02", italic(F)("1", "15") == 
          "0.40", italic(p) == "0.54")
      
      [[3]]
      list(widehat(italic(omega)[p]^2) == "4.57e-04", italic(F)("1", 
          "15") == "1.02", italic(p) == "0.33")
      

---

    Code
      select(df2, -expression)
    Output
      # A tibble: 3 x 11
        term  statistic    df df.error p.value group   sumsq meansq estimate
        <chr>     <dbl> <dbl>    <dbl>   <dbl> <chr>   <dbl>  <dbl>    <dbl>
      1 N         9.04      1       15 0.00885 Within 189.   189.     0.376 
      2 P         0.401     1       15 0.536   Within   8.40   8.40   0.0261
      3 N:P       1.02      1       15 0.329   Within  21.3   21.3    0.0635
        sum.squares.error mean.square.error
                    <dbl>             <dbl>
      1              314.              314.
      2              314.              314.
      3              314.              314.

---

    Code
      df2[["expression"]]
    Output
      [[1]]
      list(widehat(italic(eta)[p]^2) == "0.38", italic(F)("1", "15") == 
          "9.04", italic(p) == "8.85e-03")
      
      [[2]]
      list(widehat(italic(eta)[p]^2) == "0.03", italic(F)("1", "15") == 
          "0.40", italic(p) == "0.54")
      
      [[3]]
      list(widehat(italic(eta)[p]^2) == "0.06", italic(F)("1", "15") == 
          "1.02", italic(p) == "0.33")
      

