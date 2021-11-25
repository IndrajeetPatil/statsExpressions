# tidy_model_expressions works

    Code
      select(df_t, -label)
    Output
      # A tibble: 2 x 9
        term        estimate std.error conf.level conf.low conf.high statistic
        <chr>          <dbl>     <dbl>      <dbl>    <dbl>     <dbl>     <dbl>
      1 (Intercept)    6.05     0.309        0.95    5.42      6.68      19.6 
      2 mpg           -0.141    0.0147       0.95   -0.171    -0.111     -9.56
        df.error  p.value
           <int>    <dbl>
      1       30 1.20e-18
      2       30 1.29e-10

---

    Code
      df_t$label
    Output
      list(widehat(italic(beta))=='6.05', italic(t)('30')=='19.59', italic(p)=='1.20e-18')
      list(widehat(italic(beta))=='-0.14', italic(t)('30')=='-9.56', italic(p)=='1.29e-10')

---

    Code
      df_t_na$label
    Output
      list(widehat(italic(beta))=='6.05', italic(t)=='19.59', italic(p)=='1.20e-18')
      list(widehat(italic(beta))=='-0.14', italic(t)=='-9.56', italic(p)=='1.29e-10')

---

    Code
      df_t_inf$label
    Output
      list(widehat(italic(beta))=='6.05', italic(t)=='19.59', italic(p)=='1.20e-18')
      list(widehat(italic(beta))=='-0.14', italic(t)=='-9.56', italic(p)=='1.29e-10')

---

    Code
      select(df_chi, -label)
    Output
      # A tibble: 2 x 9
        term  estimate std.error conf.level conf.low conf.high statistic df.error
        <chr>    <dbl>     <dbl>      <dbl>    <dbl>     <dbl>     <dbl>    <dbl>
      1 age     0.0170   0.00923       0.95 -0.00106    0.0351      3.40        1
      2 sex    -0.512    0.168         0.95 -0.840     -0.183       9.31        1
        p.value
          <dbl>
      1 0.0650 
      2 0.00228

---

    Code
      df_chi$label
    Output
      list(widehat(italic(beta))=='0.02', italic(chi)^2*('1')=='3.40', italic(p)=='0.07')
      list(widehat(italic(beta))=='-0.51', italic(chi)^2*('1')=='9.31', italic(p)=='2.28e-03')

---

    Code
      select(df_z, -label)
    Output
      # A tibble: 3 x 9
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

---

    Code
      df_z$label
    Output
      list(widehat(italic(beta))=='-0.78', italic(z)=='-3.47', italic(p)=='5.14e-04')
      list(widehat(italic(beta))=='2.29', italic(z)=='19.13', italic(p)=='1.54e-81')
      list(widehat(italic(beta))=='-0.56', italic(z)=='-2.44', italic(p)=='0.01')

