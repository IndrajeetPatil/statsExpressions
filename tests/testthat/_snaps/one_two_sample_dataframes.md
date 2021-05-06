#  parametric t-tests

    Code
      df_1
    Output
      # A tibble: 4 x 11
           mu statistic df.error p.value method            estimate conf.level
        <dbl>     <dbl>    <dbl>   <dbl> <chr>                <dbl>      <dbl>
      1  0.25     0.242       55   0.810 One Sample t-test   0.0323       0.89
      2  0.25     0.242       55   0.810 One Sample t-test   0.0319       0.99
      3  0.25     0.242       55   0.810 One Sample t-test   0.0323       0.9 
      4  0.25     0.242       55   0.810 One Sample t-test   0.0319       0.5 
        conf.low conf.high effectsize expression
           <dbl>     <dbl> <chr>      <list>    
      1  -0.183      0.248 Cohen's d  <language>
      2  -0.311      0.375 Hedges' g  <language>
      3  -0.189      0.254 Cohen's d  <language>
      4  -0.0577     0.122 Hedges' g  <language>

---

    Code
      df_2_between
    Output
      # A tibble: 4 x 14
        term  group mean.group1 mean.group2 statistic df.error    p.value
        <chr> <chr>       <dbl>       <dbl>     <dbl>    <dbl>      <dbl>
      1 wt    am           3.77        2.41      5.26     30   0.0000113 
      2 wt    am           3.77        2.41      5.49     29.2 0.00000627
      3 wt    am           3.77        2.41      5.26     30   0.0000113 
      4 wt    am           3.77        2.41      5.49     29.2 0.00000627
        method                  estimate conf.level conf.low conf.high effectsize
        <chr>                      <dbl>      <dbl>    <dbl>     <dbl> <chr>     
      1 Two Sample t-test           1.89       0.89    1.19       2.57 Cohen's d 
      2 Welch Two Sample t-test     1.84       0.99    0.748      2.93 Hedges' g 
      3 Two Sample t-test           1.89       0.9     1.16       2.59 Cohen's d 
      4 Welch Two Sample t-test     1.84       0.5     1.54       2.11 Hedges' g 
        expression
        <list>    
      1 <language>
      2 <language>
      3 <language>
      4 <language>

---

    Code
      df_2_within
    Output
      # A tibble: 4 x 12
        term   group     statistic df.error  p.value method        estimate conf.level
        <chr>  <chr>         <dbl>    <dbl>    <dbl> <chr>            <dbl>      <dbl>
      1 desire condition      3.61       89 0.000500 Paired t-test    0.381       0.89
      2 desire condition      3.61       89 0.000500 Paired t-test    0.378       0.99
      3 desire condition      3.61       89 0.000500 Paired t-test    0.381       0.9 
      4 desire condition      3.61       89 0.000500 Paired t-test    0.378       0.5 
        conf.low conf.high effectsize expression
           <dbl>     <dbl> <chr>      <list>    
      1   0.206      0.557 Cohen's d  <language>
      2   0.0984     0.659 Hedges' g  <language>
      3   0.201      0.563 Cohen's d  <language>
      4   0.305      0.452 Hedges' g  <language>

---

    Code
      df_3_between
    Output
      # A tibble: 4 x 11
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
        conf.low conf.high effectsize expression
           <dbl>     <dbl> <chr>      <list>    
      1   0.0335     0.321 Eta2       <language>
      2   0          0.414 Omega2     <language>
      3   0.0307     0.326 Eta2       <language>
      4   0          0.275 Omega2     <language>

---

    Code
      df_3_within
    Output
      # A tibble: 2 x 15
        term      sumsq sum.squares.error    df df.error meansq statistic  p.value
        <chr>     <dbl>             <dbl> <dbl>    <dbl>  <dbl>     <dbl>    <dbl>
      1 condition  233.              984.  2.63     229.   4.30      20.6 8.27e-11
      2 condition  233.              984.  2.63     229.   4.30      20.6 8.27e-11
        method                                              estimate conf.level
        <chr>                                                  <dbl>      <dbl>
      1 ANOVA estimation for factorial designs using 'afex'   0.191        0.89
      2 ANOVA estimation for factorial designs using 'afex'   0.0783       0.9 
        conf.low conf.high effectsize       expression
           <dbl>     <dbl> <chr>            <list>    
      1   0.122      0.254 Eta2 (partial)   <language>
      2   0.0280     0.128 Omega2 (partial) <language>

