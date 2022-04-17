# parametric anova subtitles work (without NAs)

    Code
      select(df, -expression)
    Output
      # A tibble: 1 x 13
        statistic    df df.error   p.value
            <dbl> <dbl>    <dbl>     <dbl>
      1      20.2     2     19.0 0.0000196
        method                                                   effectsize estimate
        <chr>                                                    <chr>         <dbl>
      1 One-way analysis of means (not assuming equal variances) Eta2          0.681
        conf.level conf.low conf.high conf.method conf.distribution n.obs
             <dbl>    <dbl>     <dbl> <chr>       <chr>             <int>
      1       0.95    0.437         1 ncp         F                    32

---

    Code
      unlist(df$expression[[1]])
    Output
      expression(list(
      italic("F")["Welch"](2, 18.97383)=='20.24946', italic(p)=='0.00002',
      widehat(eta["p"]^2)=='0.68097', CI['95%']~'['*'0.43668', '1.00000'*']',
      italic("n")["obs"]=='32'))

---

    Code
      select(df1, -expression)
    Output
      # A tibble: 1 x 13
        statistic    df df.error    p.value method                    effectsize
            <dbl> <dbl>    <dbl>      <dbl> <chr>                     <chr>     
      1      22.9     2       29 0.00000107 One-way analysis of means Eta2      
        estimate conf.level conf.low conf.high conf.method conf.distribution n.obs
           <dbl>      <dbl>    <dbl>     <dbl> <chr>       <chr>             <int>
      1    0.612       0.95    0.404         1 ncp         F                    32

---

    Code
      unlist(df1$expression[[1]])
    Output
      expression(list(
      italic("F")["Fisher"](2, 29)=='22.91139', italic(p)=='1.07468e-06',
      widehat(eta["p"]^2)=='0.61242', CI['95%']~'['*'0.40360', '1.00000'*']',
      italic("n")["obs"]=='32'))

# parametric anova subtitles with partial omega-squared

    Code
      select(df1, -expression)
    Output
      # A tibble: 1 x 13
        statistic    df df.error p.value
            <dbl> <dbl>    <dbl>   <dbl>
      1      2.27     3     24.0   0.107
        method                                                   effectsize estimate
        <chr>                                                    <chr>         <dbl>
      1 One-way analysis of means (not assuming equal variances) Omega2        0.119
        conf.level conf.low conf.high conf.method conf.distribution n.obs
             <dbl>    <dbl>     <dbl> <chr>       <chr>             <int>
      1       0.95        0         1 ncp         F                    51

---

    Code
      unlist(df1$expression[[1]])
    Output
      expression(list(
      italic("F")["Welch"](3, 24.0475)=='2.2653', italic(p)=='0.1066',
      widehat(omega["p"]^2)=='0.1192', CI['95%']~'['*'0.0000', '1.0000'*']',
      italic("n")["obs"]=='51'))

# paired parametric anova subtitles work (without NAs)

    Code
      select(df1, -expression)
    Output
      # A tibble: 1 x 17
        term      sumsq sum.squares.error    df df.error meansq statistic  p.value
        <chr>     <dbl>             <dbl> <dbl>    <dbl>  <dbl>     <dbl>    <dbl>
      1 condition 1656.              318.  1.15     171.   1.86      776. 1.32e-69
        method                                              effectsize       estimate
        <chr>                                               <chr>               <dbl>
      1 ANOVA estimation for factorial designs using 'afex' Omega2 (partial)    0.707
        conf.level conf.low conf.high conf.method conf.distribution n.obs
             <dbl>    <dbl>     <dbl> <chr>       <chr>             <int>
      1       0.99    0.658         1 ncp         F                   150

---

    Code
      unlist(df1$expression[[1]])
    Output
      expression(list(
      italic("F")["Fisher"](1.149, 171.217)=='776.318', italic(p)=='1.325e-69',
      widehat(omega["p"]^2)=='0.707', CI['99%']~'['*'0.658', '1.000'*']',
      italic("n")["pairs"]=='150'))

# too few obs

    Code
      unlist(p_sub$expression[[1]])
    Output
      expression(list(
      italic("F")["Fisher"](6.00, 24.00)=='43.14', italic(p)=='1.08e-11',
      widehat(eta["p"]^2)=='0.92', CI['95%']~'['*'0.85', '1.00'*']',
      italic("n")["pairs"]=='5'))

