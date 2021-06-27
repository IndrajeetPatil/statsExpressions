# tidy_model_expressions works

    Code
      tidy_model_expressions(tidy_model_parameters(mod_t), statistic = "t")
    Output
      # A tibble: 6 x 10
        term                           estimate std.error conf.level conf.low
        <chr>                             <dbl>     <dbl>      <dbl>    <dbl>
      1 (Intercept)                       2.94      0.323       0.95    2.27 
      2 as.factor(am)1                   -0.893     0.379       0.95   -1.67 
      3 as.factor(cyl)6                   0.454     0.428       0.95   -0.425
      4 as.factor(cyl)8                   1.17      0.361       0.95    0.426
      5 as.factor(am)1:as.factor(cyl)6    0.259     0.571       0.95   -0.916
      6 as.factor(am)1:as.factor(cyl)8    0.159     0.571       0.95   -1.02 
        conf.high statistic df.error       p.value
            <dbl>     <dbl>    <int>         <dbl>
      1     3.60      9.08        26 0.00000000152
      2    -0.114    -2.36        26 0.0263       
      3     1.33      1.06        26 0.298        
      4     1.91      3.23        26 0.00331      
      5     1.43      0.453       26 0.654        
      6     1.33      0.278       26 0.783        
        label                                                                         
        <chr>                                                                         
      1 list(~widehat(italic(beta))=='2.94', ~italic(t)('26')=='9.08', ~italic(p)=='1~
      2 list(~widehat(italic(beta))=='-0.89', ~italic(t)('26')=='-2.36', ~italic(p)==~
      3 list(~widehat(italic(beta))=='0.45', ~italic(t)('26')=='1.06', ~italic(p)=='0~
      4 list(~widehat(italic(beta))=='1.17', ~italic(t)('26')=='3.23', ~italic(p)=='0~
      5 list(~widehat(italic(beta))=='0.26', ~italic(t)('26')=='0.45', ~italic(p)=='0~
      6 list(~widehat(italic(beta))=='0.16', ~italic(t)('26')=='0.28', ~italic(p)=='0~

---

    Code
      tidy_model_expressions(tidy_model_parameters(mod_f, omega_squared = "partial",
        table_wide = TRUE), statistic = "f")
    Output
      # A tibble: 7 x 12
        term  statistic    df df.error p.value group    sumsq  meansq estimate
        <chr>     <dbl> <dbl>    <dbl>   <dbl> <chr>    <dbl>   <dbl>    <dbl>
      1 N       12.3        1       12 0.00437 Within 189.    189.     0.230  
      2 P        0.544      1       12 0.475   Within   8.40    8.40  -0.0122 
      3 K        6.17       1       12 0.0288  Within  95.2    95.2    0.120  
      4 N:P      1.38       1       12 0.263   Within  21.3    21.3    0.00991
      5 N:K      2.15       1       12 0.169   Within  33.1    33.1    0.0294 
      6 P:K      0.0312     1       12 0.863   Within   0.482   0.482 -0.0263 
      7 N:P:K    0.483      1        4 0.525   block   37.0    37.0   -0.0942 
        sum.squares.error mean.square.error
                    <dbl>             <dbl>
      1              185.              185.
      2              185.              185.
      3              185.              185.
      4              185.              185.
      5              185.              185.
      6              185.              185.
      7              306.              306.
        label                                                                         
        <chr>                                                                         
      1 "list(~italic(F)('1'*\",\"*'12')=='12.26', ~italic(p)=='0.004', ~widehat(ital~
      2 "list(~italic(F)('1'*\",\"*'12')=='0.54', ~italic(p)=='0.475', ~widehat(itali~
      3 "list(~italic(F)('1'*\",\"*'12')=='6.17', ~italic(p)=='0.029', ~widehat(itali~
      4 "list(~italic(F)('1'*\",\"*'12')=='1.38', ~italic(p)=='0.263', ~widehat(itali~
      5 "list(~italic(F)('1'*\",\"*'12')=='2.15', ~italic(p)=='0.169', ~widehat(itali~
      6 "list(~italic(F)('1'*\",\"*'12')=='0.03', ~italic(p)=='0.863', ~widehat(itali~
      7 "list(~italic(F)('1'*\",\"*'4')=='0.48', ~italic(p)=='0.525', ~widehat(italic~

---

    Code
      tidy_model_expressions(tidy_model_parameters(mod_f, eta_squared = "partial",
        table_wide = TRUE), statistic = "f", effsize.type = "eta")
    Output
      # A tibble: 7 x 12
        term  statistic    df df.error p.value group    sumsq  meansq estimate
        <chr>     <dbl> <dbl>    <dbl>   <dbl> <chr>    <dbl>   <dbl>    <dbl>
      1 N       12.3        1       12 0.00437 Within 189.    189.     0.505  
      2 P        0.544      1       12 0.475   Within   8.40    8.40   0.0434 
      3 K        6.17       1       12 0.0288  Within  95.2    95.2    0.339  
      4 N:P      1.38       1       12 0.263   Within  21.3    21.3    0.103  
      5 N:K      2.15       1       12 0.169   Within  33.1    33.1    0.152  
      6 P:K      0.0312     1       12 0.863   Within   0.482   0.482  0.00259
      7 N:P:K    0.483      1        4 0.525   block   37.0    37.0    0.108  
        sum.squares.error mean.square.error
                    <dbl>             <dbl>
      1              185.              185.
      2              185.              185.
      3              185.              185.
      4              185.              185.
      5              185.              185.
      6              185.              185.
      7              306.              306.
        label                                                                         
        <chr>                                                                         
      1 "list(~italic(F)('1'*\",\"*'12')=='12.26', ~italic(p)=='0.004', ~widehat(ital~
      2 "list(~italic(F)('1'*\",\"*'12')=='0.54', ~italic(p)=='0.475', ~widehat(itali~
      3 "list(~italic(F)('1'*\",\"*'12')=='6.17', ~italic(p)=='0.029', ~widehat(itali~
      4 "list(~italic(F)('1'*\",\"*'12')=='1.38', ~italic(p)=='0.263', ~widehat(itali~
      5 "list(~italic(F)('1'*\",\"*'12')=='2.15', ~italic(p)=='0.169', ~widehat(itali~
      6 "list(~italic(F)('1'*\",\"*'12')=='0.03', ~italic(p)=='0.863', ~widehat(itali~
      7 "list(~italic(F)('1'*\",\"*'4')=='0.48', ~italic(p)=='0.525', ~widehat(italic~

---

    Code
      tidy_model_expressions(tidy_model_parameters(mod_chi), statistic = "chi")
    Output
      # A tibble: 2 x 9
        term  estimate std.error conf.level conf.low conf.high statistic df.error
        <chr>    <dbl>     <dbl>      <dbl>    <dbl>     <dbl>     <dbl>    <int>
      1 age     0.0170   0.00923       0.95 -0.00106    0.0351      3.40      225
      2 sex    -0.512    0.168         0.95 -0.840     -0.183       9.31      225
        p.value
          <dbl>
      1 0.0650 
      2 0.00228

---

    Code
      tidy_model_expressions(tidy_model_parameters(mod_z), statistic = "z")
    Output
      # A tibble: 3 x 10
        term        estimate std.error conf.level conf.low conf.high statistic
        <chr>          <dbl>     <dbl>      <dbl>    <dbl>     <dbl>     <dbl>
      1 (Intercept)   -0.780     0.225       0.95    -1.22    -0.342     -3.47
      2 SexFemale      2.29      0.120       0.95     2.06     2.53      19.1 
      3 AgeAdult      -0.556     0.228       0.95    -1.00    -0.108     -2.44
        df.error  p.value
           <dbl>    <dbl>
      1      Inf 5.14e- 4
      2      Inf 1.54e-81
      3      Inf 1.45e- 2
        label                                                                         
        <chr>                                                                         
      1 list(~widehat(italic(beta))=='-0.78', ~italic(z)=='-3.47', ~italic(p)=='0.001~
      2 list(~widehat(italic(beta))=='2.29', ~italic(z)=='19.13', ~italic(p)=='1.54e-~
      3 list(~widehat(italic(beta))=='-0.56', ~italic(z)=='-2.44', ~italic(p)=='0.014~

