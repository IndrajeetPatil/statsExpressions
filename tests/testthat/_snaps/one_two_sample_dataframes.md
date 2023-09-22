#  parametric t-tests

    Code
      df_1
    Output
      # A tibble: 4 x 15
           mu statistic df.error p.value method            alternative effectsize
        <dbl>     <dbl>    <dbl>   <dbl> <chr>             <chr>       <chr>     
      1  0.25     0.242       55   0.810 One Sample t-test two.sided   Hedges' g 
      2  0.25     0.242       55   0.595 One Sample t-test less        Hedges' g 
      3  0.25     0.242       55   0.405 One Sample t-test greater     Hedges' g 
      4  0.25     0.242       55   0.810 One Sample t-test two.sided   Hedges' g 
        estimate conf.level  conf.low conf.high conf.method conf.distribution n.obs
           <dbl>      <dbl>     <dbl>     <dbl> <chr>       <chr>             <int>
      1   0.0319       0.89   -0.179      0.242 ncp         t                    56
      2   0.0319       0.99 -Inf          0.338 ncp         t                    56
      3   0.0319       0.9    -0.137    Inf     ncp         t                    56
      4   0.0319       0.5    -0.0572     0.121 ncp         t                    56
        expression
        <list>    
      1 <language>
      2 <language>
      3 <language>
      4 <language>

---

    Code
      df_2_between
    Output
      # A tibble: 4 x 18
        parameter1 parameter2 mean.parameter1 mean.parameter2 statistic df.error
        <chr>      <chr>                <dbl>           <dbl>     <dbl>    <dbl>
      1 wt         am                    3.77            2.41      5.26     30  
      2 wt         am                    3.77            2.41      5.49     29.2
      3 wt         am                    3.77            2.41      5.26     30  
      4 wt         am                    3.77            2.41      5.49     29.2
           p.value method                  alternative effectsize estimate conf.level
             <dbl> <chr>                   <chr>       <chr>         <dbl>      <dbl>
      1 0.0000113  Two Sample t-test       two.sided   Cohen's d      1.93       0.89
      2 1.00       Welch Two Sample t-test less        Hedges' g      1.88       0.99
      3 0.00000563 Two Sample t-test       greater     Cohen's d      1.93       0.9 
      4 0.00000627 Welch Two Sample t-test two.sided   Hedges' g      1.88       0.5 
        conf.low conf.high conf.method conf.distribution n.obs expression
           <dbl>     <dbl> <chr>       <chr>             <int> <list>    
      1     1.23      2.61 ncp         t                    32 <language>
      2  -Inf         2.86 ncp         t                    32 <language>
      3     1.36    Inf    ncp         t                    32 <language>
      4     1.58      2.15 ncp         t                    32 <language>

---

    Code
      df_2_within
    Output
      # A tibble: 4 x 16
        term   group     statistic df.error  p.value method        alternative
        <chr>  <chr>         <dbl>    <dbl>    <dbl> <chr>         <chr>      
      1 desire condition      3.61       89 0.000500 Paired t-test two.sided  
      2 desire condition      3.61       89 0.000500 Paired t-test two.sided  
      3 desire condition      3.61       89 0.000500 Paired t-test two.sided  
      4 desire condition      3.61       89 0.000500 Paired t-test two.sided  
        effectsize estimate conf.level conf.low conf.high conf.method
        <chr>         <dbl>      <dbl>    <dbl>     <dbl> <chr>      
      1 Cohen's d     0.381       0.89   0.205      0.554 ncp        
      2 Hedges' g     0.378       0.99   0.0978     0.656 ncp        
      3 Cohen's d     0.381       0.9    0.200      0.559 ncp        
      4 Hedges' g     0.378       0.5    0.304      0.450 ncp        
        conf.distribution n.obs expression
        <chr>             <int> <list>    
      1 t                    90 <language>
      2 t                    90 <language>
      3 t                    90 <language>
      4 t                    90 <language>

---

    Code
      df_3_between
    Output
      # A tibble: 4 x 14
        statistic    df df.error p.value
            <dbl> <dbl>    <dbl>   <dbl>
      1      4.14     3     52    0.0105
      2      2.63     3     11.1  0.102 
      3      4.14     3     52    0.0105
      4      2.63     3     11.1  0.102 
        method                                                   effectsize estimate
        <chr>                                                    <chr>         <dbl>
      1 One-way analysis of means                                Eta2          0.193
      2 One-way analysis of means (not assuming equal variances) Omega2        0.245
      3 One-way analysis of means                                Eta2          0.193
      4 One-way analysis of means (not assuming equal variances) Omega2        0.245
        conf.level conf.low conf.high conf.method conf.distribution n.obs expression
             <dbl>    <dbl>     <dbl> <chr>       <chr>             <int> <list>    
      1       0.89   0.0585         1 ncp         F                    56 <language>
      2       0.8    0              1 ncp         F                    56 <language>
      3       0.9    0.0545         1 ncp         F                    56 <language>
      4       0.5    0.0974         1 ncp         F                    56 <language>

---

    Code
      df_3_within
    Output
      # A tibble: 2 x 18
        term      sumsq sum.squares.error    df df.error meansq statistic  p.value
        <chr>     <dbl>             <dbl> <dbl>    <dbl>  <dbl>     <dbl>    <dbl>
      1 condition  233.              984.  2.63     229.   4.30      20.6 8.27e-11
      2 condition  233.              984.  2.63     229.   4.30      20.6 8.27e-11
        method                                              effectsize       estimate
        <chr>                                               <chr>               <dbl>
      1 ANOVA estimation for factorial designs using 'afex' Eta2 (partial)     0.191 
      2 ANOVA estimation for factorial designs using 'afex' Omega2 (partial)   0.0783
        conf.level conf.low conf.high conf.method conf.distribution n.obs expression
             <dbl>    <dbl>     <dbl> <chr>       <chr>             <int> <list>    
      1       0.89   0.136          1 ncp         F                    88 <language>
      2       0.9    0.0362         1 ncp         F                    88 <language>

