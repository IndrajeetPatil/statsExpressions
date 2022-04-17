# parametric t-test works (between-subjects without NAs)

    Code
      select(df1, -expression)
    Output
      # A tibble: 1 x 17
        term  group mean.group1 mean.group2 statistic df.error p.value
        <chr> <chr>       <dbl>       <dbl>     <dbl>    <dbl>   <dbl>
      1 len   supp         20.7        17.0      1.92       58  0.0604
        method            alternative effectsize estimate conf.level conf.low
        <chr>             <chr>       <chr>         <dbl>      <dbl>    <dbl>
      1 Two Sample t-test two.sided   Cohen's d     0.495       0.99   -0.184
        conf.high conf.method conf.distribution n.obs
            <dbl> <chr>       <chr>             <int>
      1      1.17 ncp         t                    60

---

    Code
      unlist(df1$expression[[1]])
    Output
      expression(list(
      italic("t")["Student"]*'('*58*')'=='1.91527', italic(p)=='0.06039',
      widehat(italic("d"))["Cohen"]=='0.49452', CI['99%']~'['*'-0.18354', '1.16839'*']',
      italic("n")["obs"]=='60'))

# parametric t-test works (between-subjects with NAs)

    Code
      select(df1, -expression)
    Output
      # A tibble: 1 x 17
        term  group mean.group1 mean.group2 statistic df.error p.value
        <chr> <chr>       <dbl>       <dbl>     <dbl>    <dbl>   <dbl>
      1 len   supp         20.7        17.0      1.92     55.3  0.0606
        method                  alternative effectsize estimate conf.level conf.low
        <chr>                   <chr>       <chr>         <dbl>      <dbl>    <dbl>
      1 Welch Two Sample t-test two.sided   Hedges' g     0.488        0.9   0.0599
        conf.high conf.method conf.distribution n.obs
            <dbl> <chr>       <chr>             <int>
      1     0.911 ncp         t                    60

---

    Code
      unlist(df1$expression[[1]])
    Output
      expression(list(
      italic("t")["Welch"]*'('*55.309*')'=='1.915', italic(p)=='0.061',
      widehat(italic("g"))["Hedges"]=='0.488', CI['90%']~'['*'0.060', '0.911'*']',
      italic("n")["obs"]=='60'))

# parametric t-test works (within-subjects without NAs)

    Code
      select(df1, -expression)
    Output
      # A tibble: 1 x 15
        term  group     statistic df.error  p.value method        alternative
        <chr> <chr>         <dbl>    <dbl>    <dbl> <chr>         <chr>      
      1 value condition      34.8      149 1.85e-73 Paired t-test two.sided  
        effectsize estimate conf.level conf.low conf.high conf.method
        <chr>         <dbl>      <dbl>    <dbl>     <dbl> <chr>      
      1 Hedges' g      2.83        0.5     2.71      2.96 ncp        
        conf.distribution n.obs
        <chr>             <int>
      1 t                   150

---

    Code
      unlist(df1$expression[[1]])
    Output
      expression(list(
      italic("t")["Student"]*'('*149*')'=='34.8152', italic(p)=='1.8496e-73',
      widehat(italic("g"))["Hedges"]=='2.8283', CI['50%']~'['*'2.7086', '2.9560'*']',
      italic("n")["pairs"]=='150'))

# parametric t-test works (within-subjects with NAs)

    Code
      select(df1, -expression)
    Output
      # A tibble: 1 x 15
        term   group     statistic df.error  p.value method        alternative
        <chr>  <chr>         <dbl>    <dbl>    <dbl> <chr>         <chr>      
      1 desire condition      3.61       89 0.000500 Paired t-test two.sided  
        effectsize estimate conf.level conf.low conf.high conf.method
        <chr>         <dbl>      <dbl>    <dbl>     <dbl> <chr>      
      1 Cohen's d     0.381       0.95    0.167     0.597 ncp        
        conf.distribution n.obs
        <chr>             <int>
      1 t                    90

---

    Code
      unlist(df1$expression[[1]])
    Output
      expression(list(
      italic("t")["Student"]*'('*89*')'=='3.613', italic(p)=='5.000e-04',
      widehat(italic("d"))["Cohen"]=='0.381', CI['95%']~'['*'0.167', '0.597'*']',
      italic("n")["pairs"]=='90'))

