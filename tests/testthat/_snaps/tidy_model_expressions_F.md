# tidy_model_expressions works - F

    Code
      select(df1, -label)
    Output
      # A tibble: 3 x 11
        term  statistic    df df.error p.value group   sumsq meansq  estimate
        <chr>     <dbl> <dbl>    <dbl>   <dbl> <chr>   <dbl>  <dbl>     <dbl>
      1 N         9.04      1       15 0.00885 Within 189.   189.    0.184   
      2 N:P       1.02      1       15 0.329   Within  21.3   21.3   0.000457
      3 P         0.401     1       15 0.536   Within   8.40   8.40 -0.0171  
        sum.squares.error mean.square.error
                    <dbl>             <dbl>
      1              314.              314.
      2              314.              314.
      3              314.              314.

---

    Code
      df1$label
    Output
      list(widehat(italic(omega)[p]^2)=='0.18', italic(F)('1', '15')=='9.04', italic(p)=='8.85e-03')
      list(widehat(italic(omega)[p]^2)=='4.57e-04', italic(F)('1', '15')=='1.02', italic(p)=='0.33')
      list(widehat(italic(omega)[p]^2)=='-0.02', italic(F)('1', '15')=='0.40', italic(p)=='0.54')

---

    Code
      select(df2, -label)
    Output
      # A tibble: 3 x 11
        term  statistic    df df.error p.value group   sumsq meansq estimate
        <chr>     <dbl> <dbl>    <dbl>   <dbl> <chr>   <dbl>  <dbl>    <dbl>
      1 N         9.04      1       15 0.00885 Within 189.   189.     0.376 
      2 N:P       1.02      1       15 0.329   Within  21.3   21.3    0.0635
      3 P         0.401     1       15 0.536   Within   8.40   8.40   0.0261
        sum.squares.error mean.square.error
                    <dbl>             <dbl>
      1              314.              314.
      2              314.              314.
      3              314.              314.

---

    Code
      df2$label
    Output
      list(widehat(italic(eta)[p]^2)=='0.38', italic(F)('1', '15')=='9.04', italic(p)=='8.85e-03')
      list(widehat(italic(eta)[p]^2)=='0.06', italic(F)('1', '15')=='1.02', italic(p)=='0.33')
      list(widehat(italic(eta)[p]^2)=='0.03', italic(F)('1', '15')=='0.40', italic(p)=='0.54')

