# tidy_model_expressions works - F

    Code
      tidy_model_expressions(tidy_model_parameters(mod_f, omega_squared = "partial",
        table_wide = TRUE), statistic = "f")
    Output
      # A tibble: 3 x 12
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
        label                                                                         
        <chr>                                                                         
      1 "list(~italic(F)('1'*\",\"*'15')=='9.04', ~italic(p)=='0.009', ~widehat(itali~
      2 "list(~italic(F)('1'*\",\"*'15')=='0.40', ~italic(p)=='0.536', ~widehat(itali~
      3 "list(~italic(F)('1'*\",\"*'15')=='1.02', ~italic(p)=='0.329', ~widehat(itali~

---

    Code
      tidy_model_expressions(tidy_model_parameters(mod_f, eta_squared = "partial",
        table_wide = TRUE), statistic = "f", effsize.type = "eta")
    Output
      # A tibble: 3 x 12
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
        label                                                                         
        <chr>                                                                         
      1 "list(~italic(F)('1'*\",\"*'15')=='9.04', ~italic(p)=='0.009', ~widehat(itali~
      2 "list(~italic(F)('1'*\",\"*'15')=='0.40', ~italic(p)=='0.536', ~widehat(itali~
      3 "list(~italic(F)('1'*\",\"*'15')=='1.02', ~italic(p)=='0.329', ~widehat(itali~

