#  parametric t-tests

    Code
      df_1
    Output
      # A tibble: 4 x 14
           mu statistic df.error p.value method            alternative estimate
        <dbl>     <dbl>    <dbl>   <dbl> <chr>             <chr>          <dbl>
      1  0.25     0.242       55   0.810 One Sample t-test two.sided     0.0323
      2  0.25     0.242       55   0.595 One Sample t-test less          0.0319
      3  0.25     0.242       55   0.405 One Sample t-test greater       0.0323
      4  0.25     0.242       55   0.810 One Sample t-test two.sided     0.0319
        conf.level conf.low conf.high effectsize conf.method conf.distribution
             <dbl>    <dbl>     <dbl> <chr>      <chr>       <chr>            
      1       0.89  -0.183      0.248 Cohen's d  ncp         t                
      2       0.99  -0.311      0.375 Hedges' g  ncp         t                
      3       0.9   -0.189      0.254 Cohen's d  ncp         t                
      4       0.5   -0.0577     0.122 Hedges' g  ncp         t                
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
      # A tibble: 4 x 17
        term  group mean.group1 mean.group2 statistic df.error    p.value
        <chr> <chr>       <dbl>       <dbl>     <dbl>    <dbl>      <dbl>
      1 wt    am           3.77        2.41      5.26     30   0.0000113 
      2 wt    am           3.77        2.41      5.49     29.2 1.00      
      3 wt    am           3.77        2.41      5.26     30   0.00000563
      4 wt    am           3.77        2.41      5.49     29.2 0.00000627
        method                  alternative estimate conf.level conf.low conf.high
        <chr>                   <chr>          <dbl>      <dbl>    <dbl>     <dbl>
      1 Two Sample t-test       two.sided       1.93       0.89    1.23       2.61
      2 Welch Two Sample t-test less            1.88       0.99    0.793      2.97
      3 Two Sample t-test       greater         1.93       0.9     1.21       2.63
      4 Welch Two Sample t-test two.sided       1.88       0.5     1.58       2.15
        effectsize conf.method conf.distribution expression
        <chr>      <chr>       <chr>             <list>    
      1 Cohen's d  ncp         t                 <language>
      2 Hedges' g  ncp         t                 <language>
      3 Cohen's d  ncp         t                 <language>
      4 Hedges' g  ncp         t                 <language>

---

    Code
      df_2_within
    Output
      # A tibble: 4 x 15
        term   group     statistic df.error  p.value method        alternative
        <chr>  <chr>         <dbl>    <dbl>    <dbl> <chr>         <chr>      
      1 desire condition      3.61       89 0.000500 Paired t-test two.sided  
      2 desire condition      3.61       89 0.000500 Paired t-test two.sided  
      3 desire condition      3.61       89 0.000500 Paired t-test two.sided  
      4 desire condition      3.61       89 0.000500 Paired t-test two.sided  
        estimate conf.level conf.low conf.high effectsize conf.method
           <dbl>      <dbl>    <dbl>     <dbl> <chr>      <chr>      
      1    0.381       0.89   0.206      0.557 Cohen's d  ncp        
      2    0.378       0.99   0.0984     0.659 Hedges' g  ncp        
      3    0.381       0.9    0.201      0.563 Cohen's d  ncp        
      4    0.378       0.5    0.305      0.452 Hedges' g  ncp        
        conf.distribution expression
        <chr>             <list>    
      1 t                 <language>
      2 t                 <language>
      3 t                 <language>
      4 t                 <language>

---

    Code
      df_3_between
    Output
      # A tibble: 4 x 13
        statistic    df df.error p.value
            <dbl> <dbl>    <dbl>   <dbl>
      1      4.14     3     52    0.0105
      2      2.63     3     11.1  0.102 
      3      4.14     3     52    0.0105
      4      2.63     3     11.1  0.102 
        method                                                   estimate conf.level
        <chr>                                                       <dbl>      <dbl>
      1 One-way analysis of means                                   0.193       0.89
      2 One-way analysis of means (not assuming equal variances)    0.245       0.8 
      3 One-way analysis of means                                   0.193       0.9 
      4 One-way analysis of means (not assuming equal variances)    0.245       0.5 
        conf.low conf.high effectsize conf.method conf.distribution expression
           <dbl>     <dbl> <chr>      <chr>       <chr>             <list>    
      1   0.0585         1 Eta2       ncp         F                 <language>
      2   0              1 Omega2     ncp         F                 <language>
      3   0.0545         1 Eta2       ncp         F                 <language>
      4   0.0974         1 Omega2     ncp         F                 <language>

---

    Code
      df_3_within
    Output
      # A tibble: 2 x 17
        term      sumsq sum.squares.error    df df.error meansq statistic  p.value
        <chr>     <dbl>             <dbl> <dbl>    <dbl>  <dbl>     <dbl>    <dbl>
      1 condition  233.              984.  2.63     229.   4.30      20.6 8.27e-11
      2 condition  233.              984.  2.63     229.   4.30      20.6 8.27e-11
        method                                              estimate conf.level
        <chr>                                                  <dbl>      <dbl>
      1 ANOVA estimation for factorial designs using 'afex'   0.191        0.89
      2 ANOVA estimation for factorial designs using 'afex'   0.0783       0.9 
        conf.low conf.high effectsize       conf.method conf.distribution expression
           <dbl>     <dbl> <chr>            <chr>       <chr>             <list>    
      1   0.136          1 Eta2 (partial)   ncp         F                 <language>
      2   0.0362         1 Omega2 (partial) ncp         F                 <language>

