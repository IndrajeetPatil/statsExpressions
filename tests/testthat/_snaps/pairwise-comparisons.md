# `pairwise_comparisons()` works for between-subjects design

    Code
      df1
    Output
      # A tibble: 6 x 6
        group1  group2  p.value p.adjust.method test        expression
        <chr>   <chr>     <dbl> <chr>           <chr>       <list>    
      1 carni   herbi     1     Bonferroni      Student's t <language>
      2 carni   insecti   1     Bonferroni      Student's t <language>
      3 carni   omni      1     Bonferroni      Student's t <language>
      4 herbi   insecti   1     Bonferroni      Student's t <language>
      5 herbi   omni      0.979 Bonferroni      Student's t <language>
      6 insecti omni      1     Bonferroni      Student's t <language>

---

    Code
      df1[["expression"]]
    Output
      [[1]]
      list(italic(p)["Bonferroni" - adj.] == "1.00")
      
      [[2]]
      list(italic(p)["Bonferroni" - adj.] == "1.00")
      
      [[3]]
      list(italic(p)["Bonferroni" - adj.] == "1.00")
      
      [[4]]
      list(italic(p)["Bonferroni" - adj.] == "1.00")
      
      [[5]]
      list(italic(p)["Bonferroni" - adj.] == "0.98")
      
      [[6]]
      list(italic(p)["Bonferroni" - adj.] == "1.00")
      

---

    Code
      df2
    Output
      # A tibble: 6 x 9
        group1  group2  statistic p.value alternative distribution p.adjust.method
        <chr>   <chr>       <dbl>   <dbl> <chr>       <chr>        <chr>          
      1 carni   herbi        2.17       1 two.sided   q            Bonferroni     
      2 carni   insecti     -2.17       1 two.sided   q            Bonferroni     
      3 carni   omni         1.10       1 two.sided   q            Bonferroni     
      4 herbi   insecti     -2.41       1 two.sided   q            Bonferroni     
      5 herbi   omni        -1.87       1 two.sided   q            Bonferroni     
      6 insecti omni         2.19       1 two.sided   q            Bonferroni     
        test         expression
        <chr>        <list>    
      1 Games-Howell <language>
      2 Games-Howell <language>
      3 Games-Howell <language>
      4 Games-Howell <language>
      5 Games-Howell <language>
      6 Games-Howell <language>

---

    Code
      df2[["expression"]]
    Output
      [[1]]
      list(italic(p)["Bonferroni" - adj.] == "1.00")
      
      [[2]]
      list(italic(p)["Bonferroni" - adj.] == "1.00")
      
      [[3]]
      list(italic(p)["Bonferroni" - adj.] == "1.00")
      
      [[4]]
      list(italic(p)["Bonferroni" - adj.] == "1.00")
      
      [[5]]
      list(italic(p)["Bonferroni" - adj.] == "1.00")
      
      [[6]]
      list(italic(p)["Bonferroni" - adj.] == "1.00")
      

---

    Code
      df3
    Output
      # A tibble: 6 x 9
        group1  group2  statistic p.value alternative distribution p.adjust.method
        <chr>   <chr>       <dbl>   <dbl> <chr>       <chr>        <chr>          
      1 carni   herbi       0.582  0.561  two.sided   z            None           
      2 carni   insecti     1.88   0.0595 two.sided   z            None           
      3 carni   omni        1.14   0.254  two.sided   z            None           
      4 herbi   insecti     1.63   0.102  two.sided   z            None           
      5 herbi   omni        0.717  0.474  two.sided   z            None           
      6 insecti omni        1.14   0.254  two.sided   z            None           
        test  expression
        <chr> <list>    
      1 Dunn  <language>
      2 Dunn  <language>
      3 Dunn  <language>
      4 Dunn  <language>
      5 Dunn  <language>
      6 Dunn  <language>

---

    Code
      df3[["expression"]]
    Output
      [[1]]
      list(italic(p)[unadj.] == "0.56")
      
      [[2]]
      list(italic(p)[unadj.] == "0.06")
      
      [[3]]
      list(italic(p)[unadj.] == "0.25")
      
      [[4]]
      list(italic(p)[unadj.] == "0.10")
      
      [[5]]
      list(italic(p)[unadj.] == "0.47")
      
      [[6]]
      list(italic(p)[unadj.] == "0.25")
      

---

    Code
      df4
    Output
      # A tibble: 6 x 10
        group1  group2  estimate conf.level conf.low conf.high p.value p.adjust.method
        <chr>   <chr>      <dbl>      <dbl>    <dbl>     <dbl>   <dbl> <chr>          
      1 carni   herbi   -0.0323        0.95  -0.248     0.184    0.790 FDR            
      2 carni   insecti  0.0451        0.95  -0.0484    0.139    0.552 FDR            
      3 carni   omni     0.00520       0.95  -0.114     0.124    0.898 FDR            
      4 herbi   insecti  0.0774        0.95  -0.133     0.288    0.552 FDR            
      5 herbi   omni     0.0375        0.95  -0.182     0.257    0.790 FDR            
      6 insecti omni    -0.0399        0.95  -0.142     0.0625   0.552 FDR            
        test                 expression
        <chr>                <list>    
      1 Yuen's trimmed means <language>
      2 Yuen's trimmed means <language>
      3 Yuen's trimmed means <language>
      4 Yuen's trimmed means <language>
      5 Yuen's trimmed means <language>
      6 Yuen's trimmed means <language>

---

    Code
      df4[["expression"]]
    Output
      [[1]]
      list(italic(p)["FDR" - adj.] == "0.79")
      
      [[2]]
      list(italic(p)["FDR" - adj.] == "0.55")
      
      [[3]]
      list(italic(p)["FDR" - adj.] == "0.90")
      
      [[4]]
      list(italic(p)["FDR" - adj.] == "0.55")
      
      [[5]]
      list(italic(p)["FDR" - adj.] == "0.79")
      
      [[6]]
      list(italic(p)["FDR" - adj.] == "0.55")
      

---

    Code
      df5
    Output
      # A tibble: 3 x 6
        group1 group2 p.value p.adjust.method test        expression
        <chr>  <chr>    <dbl> <chr>           <chr>       <list>    
      1 PG     PG-13  0.316   Holm            Student's t <language>
      2 PG     R      0.00283 Holm            Student's t <language>
      3 PG-13  R      0.00310 Holm            Student's t <language>

---

    Code
      df5[["expression"]]
    Output
      [[1]]
      list(italic(p)["Holm" - adj.] == "0.32")
      
      [[2]]
      list(italic(p)["Holm" - adj.] == "2.83e-03")
      
      [[3]]
      list(italic(p)["Holm" - adj.] == "3.10e-03")
      

---

    Code
      df6
    Output
      # A tibble: 6 x 9
        group1  group2  statistic p.value alternative distribution p.adjust.method
        <chr>   <chr>       <dbl>   <dbl> <chr>       <chr>        <chr>          
      1 carni   herbi        2.17       1 two.sided   q            Holm           
      2 carni   insecti     -2.17       1 two.sided   q            Holm           
      3 carni   omni         1.10       1 two.sided   q            Holm           
      4 herbi   insecti     -2.41       1 two.sided   q            Holm           
      5 herbi   omni        -1.87       1 two.sided   q            Holm           
      6 insecti omni         2.19       1 two.sided   q            Holm           
        test         expression
        <chr>        <list>    
      1 Games-Howell <language>
      2 Games-Howell <language>
      3 Games-Howell <language>
      4 Games-Howell <language>
      5 Games-Howell <language>
      6 Games-Howell <language>

---

    Code
      df6[["expression"]]
    Output
      [[1]]
      list(italic(p)["Holm" - adj.] == "1.00")
      
      [[2]]
      list(italic(p)["Holm" - adj.] == "1.00")
      
      [[3]]
      list(italic(p)["Holm" - adj.] == "1.00")
      
      [[4]]
      list(italic(p)["Holm" - adj.] == "1.00")
      
      [[5]]
      list(italic(p)["Holm" - adj.] == "1.00")
      
      [[6]]
      list(italic(p)["Holm" - adj.] == "1.00")
      

# dropped levels are not included

    Code
      df1
    Output
      # A tibble: 1 x 9
        group1 group2 statistic p.value alternative distribution p.adjust.method
        <chr>  <chr>      <dbl>   <dbl> <chr>       <chr>        <chr>          
      1 carni  omni        1.10   0.447 two.sided   q            None           
        test         expression
        <chr>        <list>    
      1 Games-Howell <language>

---

    Code
      df1[["expression"]]
    Output
      [[1]]
      list(italic(p)[unadj.] == "0.45")
      

# data without NAs

    Code
      df
    Output
      # A tibble: 3 x 6
        group1     group2      p.value p.adjust.method test        expression
        <chr>      <chr>         <dbl> <chr>           <chr>       <list>    
      1 setosa     versicolor 1.32e-15 FDR             Student's t <language>
      2 setosa     virginica  6.64e-32 FDR             Student's t <language>
      3 versicolor virginica  2.77e- 9 FDR             Student's t <language>

---

    Code
      df[["expression"]]
    Output
      [[1]]
      list(italic(p)["FDR" - adj.] == "1.32e-15")
      
      [[2]]
      list(italic(p)["FDR" - adj.] == "6.64e-32")
      
      [[3]]
      list(italic(p)["FDR" - adj.] == "2.77e-09")
      

# `pairwise_comparisons()` works for within-subjects design - NAs

    Code
      df1
    Output
      # A tibble: 6 x 6
        group1 group2  p.value p.adjust.method test        expression
        <chr>  <chr>     <dbl> <chr>           <chr>       <list>    
      1 HDHF   HDLF   3.18e- 3 Bonferroni      Student's t <language>
      2 HDHF   LDHF   4.21e- 1 Bonferroni      Student's t <language>
      3 HDHF   LDLF   3.95e-12 Bonferroni      Student's t <language>
      4 HDLF   LDHF   3.37e- 1 Bonferroni      Student's t <language>
      5 HDLF   LDLF   7.94e- 3 Bonferroni      Student's t <language>
      6 LDHF   LDLF   1.33e- 8 Bonferroni      Student's t <language>

---

    Code
      df1[["expression"]]
    Output
      [[1]]
      list(italic(p)["Bonferroni" - adj.] == "0.003")
      
      [[2]]
      list(italic(p)["Bonferroni" - adj.] == "0.421")
      
      [[3]]
      list(italic(p)["Bonferroni" - adj.] == "3.950e-12")
      
      [[4]]
      list(italic(p)["Bonferroni" - adj.] == "0.337")
      
      [[5]]
      list(italic(p)["Bonferroni" - adj.] == "0.008")
      
      [[6]]
      list(italic(p)["Bonferroni" - adj.] == "1.331e-08")
      

---

    Code
      df2
    Output
      # A tibble: 6 x 9
        group1 group2 statistic  p.value alternative distribution p.adjust.method
        <chr>  <chr>      <dbl>    <dbl> <chr>       <chr>        <chr>          
      1 HDHF   HDLF        4.78 1.44e- 5 two.sided   t            BY             
      2 HDHF   LDHF        2.44 4.47e- 2 two.sided   t            BY             
      3 HDHF   LDLF        8.01 5.45e-13 two.sided   t            BY             
      4 HDLF   LDHF        2.34 4.96e- 2 two.sided   t            BY             
      5 HDLF   LDLF        3.23 5.05e- 3 two.sided   t            BY             
      6 LDHF   LDLF        5.57 4.64e- 7 two.sided   t            BY             
        test           expression
        <chr>          <list>    
      1 Durbin-Conover <language>
      2 Durbin-Conover <language>
      3 Durbin-Conover <language>
      4 Durbin-Conover <language>
      5 Durbin-Conover <language>
      6 Durbin-Conover <language>

---

    Code
      df2[["expression"]]
    Output
      [[1]]
      list(italic(p)["BY" - adj.] == "1.436e-05")
      
      [[2]]
      list(italic(p)["BY" - adj.] == "0.045")
      
      [[3]]
      list(italic(p)["BY" - adj.] == "5.447e-13")
      
      [[4]]
      list(italic(p)["BY" - adj.] == "0.050")
      
      [[5]]
      list(italic(p)["BY" - adj.] == "0.005")
      
      [[6]]
      list(italic(p)["BY" - adj.] == "4.635e-07")
      

---

    Code
      df3
    Output
      # A tibble: 6 x 11
        group1 group2 estimate conf.level conf.low conf.high     p.value  p.crit
        <chr>  <chr>     <dbl>      <dbl>    <dbl>     <dbl>       <dbl>   <dbl>
      1 HDHF   HDLF      1.03        0.95   0.140      1.92  0.00999     0.0127 
      2 HDHF   LDHF      0.454       0.95  -0.104      1.01  0.0520      0.025  
      3 HDHF   LDLF      1.95        0.95   1.09       2.82  0.000000564 0.00851
      4 HDLF   LDHF     -0.676       0.95  -1.61       0.256 0.0520      0.05   
      5 HDLF   LDLF      0.889       0.95   0.0244     1.75  0.0203      0.0169 
      6 LDHF   LDLF      1.35        0.95   0.560      2.14  0.000102    0.0102 
        p.adjust.method test                 expression
        <chr>           <chr>                <list>    
      1 Hommel          Yuen's trimmed means <language>
      2 Hommel          Yuen's trimmed means <language>
      3 Hommel          Yuen's trimmed means <language>
      4 Hommel          Yuen's trimmed means <language>
      5 Hommel          Yuen's trimmed means <language>
      6 Hommel          Yuen's trimmed means <language>

---

    Code
      df3[["expression"]]
    Output
      [[1]]
      list(italic(p)["Hommel" - adj.] == "0.010")
      
      [[2]]
      list(italic(p)["Hommel" - adj.] == "0.052")
      
      [[3]]
      list(italic(p)["Hommel" - adj.] == "5.642e-07")
      
      [[4]]
      list(italic(p)["Hommel" - adj.] == "0.052")
      
      [[5]]
      list(italic(p)["Hommel" - adj.] == "0.020")
      
      [[6]]
      list(italic(p)["Hommel" - adj.] == "1.017e-04")
      

---

    Code
      df4
    Output
      # A tibble: 6 x 18
        group1 group2 term       effectsize      estimate conf.level conf.low
        <chr>  <chr>  <chr>      <chr>              <dbl>      <dbl>    <dbl>
      1 HDHF   HDLF   Difference Bayesian t-test    1.10        0.95   0.488 
      2 HDHF   LDHF   Difference Bayesian t-test    0.450       0.95  -0.0551
      3 HDHF   LDLF   Difference Bayesian t-test    2.13        0.95   1.62  
      4 HDLF   LDHF   Difference Bayesian t-test   -0.649       0.95  -1.32  
      5 HDLF   LDLF   Difference Bayesian t-test    0.976       0.95   0.380 
      6 LDHF   LDLF   Difference Bayesian t-test    1.66        0.95   1.15  
        conf.high    pd prior.distribution prior.location prior.scale     bf10
            <dbl> <dbl> <chr>                       <dbl>       <dbl>    <dbl>
      1    1.72   1     cauchy                          0       0.707 4.16e+ 1
      2    0.951  0.954 cauchy                          0       0.707 5.83e- 1
      3    2.63   1     cauchy                          0       0.707 1.20e+10
      4    0.0583 0.968 cauchy                          0       0.707 6.98e- 1
      5    1.60   0.999 cauchy                          0       0.707 1.81e+ 1
      6    2.15   1     cauchy                          0       0.707 4.81e+ 6
        conf.method log_e_bf10 n.obs expression test       
        <chr>            <dbl> <int> <list>     <chr>      
      1 ETI              3.73     88 <language> Student's t
      2 ETI             -0.539    88 <language> Student's t
      3 ETI             23.2      88 <language> Student's t
      4 ETI             -0.359    88 <language> Student's t
      5 ETI              2.90     88 <language> Student's t
      6 ETI             15.4      88 <language> Student's t

---

    Code
      df4[["expression"]]
    Output
      [[1]]
      list(log[e] * (BF["01"]) == "-3.73")
      
      [[2]]
      list(log[e] * (BF["01"]) == "0.54")
      
      [[3]]
      list(log[e] * (BF["01"]) == "-23.21")
      
      [[4]]
      list(log[e] * (BF["01"]) == "0.36")
      
      [[5]]
      list(log[e] * (BF["01"]) == "-2.90")
      
      [[6]]
      list(log[e] * (BF["01"]) == "-15.39")
      

# `pairwise_comparisons()` works for within-subjects design - without NAs

    Code
      df1
    Output
      # A tibble: 3 x 6
        group1 group2  p.value p.adjust.method test        expression
        <chr>  <chr>     <dbl> <chr>           <chr>       <list>    
      1 Wine A Wine B 0.732    None            Student's t <language>
      2 Wine A Wine C 0.0142   None            Student's t <language>
      3 Wine B Wine C 0.000675 None            Student's t <language>

---

    Code
      df1[["expression"]]
    Output
      [[1]]
      list(italic(p)[unadj.] == "0.732")
      
      [[2]]
      list(italic(p)[unadj.] == "0.014")
      
      [[3]]
      list(italic(p)[unadj.] == "6.754e-04")
      

---

    Code
      df2
    Output
      # A tibble: 3 x 9
        group1 group2 statistic  p.value alternative distribution p.adjust.method
        <chr>  <chr>      <dbl>    <dbl> <chr>       <chr>        <chr>          
      1 Wine A Wine B      1.05 0.301    two.sided   t            None           
      2 Wine A Wine C      3.66 0.000691 two.sided   t            None           
      3 Wine B Wine C      2.62 0.0123   two.sided   t            None           
        test           expression
        <chr>          <list>    
      1 Durbin-Conover <language>
      2 Durbin-Conover <language>
      3 Durbin-Conover <language>

---

    Code
      df2[["expression"]]
    Output
      [[1]]
      list(italic(p)[unadj.] == "0.301")
      
      [[2]]
      list(italic(p)[unadj.] == "6.915e-04")
      
      [[3]]
      list(italic(p)[unadj.] == "0.012")
      

---

    Code
      df3
    Output
      # A tibble: 3 x 11
        group1 group2 estimate conf.level conf.low conf.high p.value p.crit
        <chr>  <chr>     <dbl>      <dbl>    <dbl>     <dbl>   <dbl>  <dbl>
      1 Wine A Wine B   0.0214       0.95 -0.0216     0.0645 0.195   0.05  
      2 Wine A Wine C   0.114        0.95  0.0215     0.207  0.00492 0.0169
      3 Wine B Wine C   0.0821       0.95  0.00891    0.155  0.00878 0.025 
        p.adjust.method test                 expression
        <chr>           <chr>                <list>    
      1 None            Yuen's trimmed means <language>
      2 None            Yuen's trimmed means <language>
      3 None            Yuen's trimmed means <language>

---

    Code
      df3[["expression"]]
    Output
      [[1]]
      list(italic(p)[unadj.] == "0.195")
      
      [[2]]
      list(italic(p)[unadj.] == "0.005")
      
      [[3]]
      list(italic(p)[unadj.] == "0.009")
      

---

    Code
      df4
    Output
      # A tibble: 3 x 18
        group1 group2 term       effectsize      estimate conf.level conf.low
        <chr>  <chr>  <chr>      <chr>              <dbl>      <dbl>    <dbl>
      1 Wine A Wine B Difference Bayesian t-test  0.00721       0.95  -0.0418
      2 Wine A Wine C Difference Bayesian t-test  0.0755        0.95   0.0127
      3 Wine B Wine C Difference Bayesian t-test  0.0693        0.95   0.0303
        conf.high    pd prior.distribution prior.location prior.scale   bf10
            <dbl> <dbl> <chr>                       <dbl>       <dbl>  <dbl>
      1    0.0562 0.624 cauchy                          0       0.707  0.235
      2    0.140  0.990 cauchy                          0       0.707  3.71 
      3    0.110  1.000 cauchy                          0       0.707 50.5  
        conf.method log_e_bf10 n.obs expression test       
        <chr>            <dbl> <int> <list>     <chr>      
      1 ETI              -1.45    22 <language> Student's t
      2 ETI               1.31    22 <language> Student's t
      3 ETI               3.92    22 <language> Student's t

---

    Code
      df4[["expression"]]
    Output
      [[1]]
      list(log[e] * (BF["01"]) == "1.45")
      
      [[2]]
      list(log[e] * (BF["01"]) == "-1.31")
      
      [[3]]
      list(log[e] * (BF["01"]) == "-3.92")
      

# additional arguments are passed to underlying methods

    Code
      df1
    Output
      # A tibble: 6 x 6
        group1 group2  p.value p.adjust.method test        expression
        <chr>  <chr>     <dbl> <chr>           <chr>       <list>    
      1 HDHF   HDLF   2.65e- 4 None            Student's t <language>
      2 HDHF   LDHF   3.51e- 2 None            Student's t <language>
      3 HDHF   LDLF   3.29e-13 None            Student's t <language>
      4 HDLF   LDHF   9.72e- 1 None            Student's t <language>
      5 HDLF   LDLF   6.62e- 4 None            Student's t <language>
      6 LDHF   LDLF   1.11e- 9 None            Student's t <language>

---

    Code
      df1[["expression"]]
    Output
      [[1]]
      list(italic(p)[unadj.] == "2.65e-04")
      
      [[2]]
      list(italic(p)[unadj.] == "0.04")
      
      [[3]]
      list(italic(p)[unadj.] == "3.29e-13")
      
      [[4]]
      list(italic(p)[unadj.] == "0.97")
      
      [[5]]
      list(italic(p)[unadj.] == "6.62e-04")
      
      [[6]]
      list(italic(p)[unadj.] == "1.11e-09")
      

---

    Code
      df2
    Output
      # A tibble: 6 x 6
        group1 group2 p.value p.adjust.method test        expression
        <chr>  <chr>    <dbl> <chr>           <chr>       <list>    
      1 HDHF   HDLF    1.000  None            Student's t <language>
      2 HDHF   LDHF    0.965  None            Student's t <language>
      3 HDHF   LDLF    1.000  None            Student's t <language>
      4 HDLF   LDHF    0.0281 None            Student's t <language>
      5 HDLF   LDLF    0.999  None            Student's t <language>
      6 LDHF   LDLF    1.000  None            Student's t <language>

---

    Code
      df2[["expression"]]
    Output
      [[1]]
      list(italic(p)[unadj.] == "1.00")
      
      [[2]]
      list(italic(p)[unadj.] == "0.96")
      
      [[3]]
      list(italic(p)[unadj.] == "1.00")
      
      [[4]]
      list(italic(p)[unadj.] == "0.03")
      
      [[5]]
      list(italic(p)[unadj.] == "1.00")
      
      [[6]]
      list(italic(p)[unadj.] == "1.00")
      

---

    Code
      df3
    Output
      # A tibble: 3 x 6
        group1 group2 p.value p.adjust.method test        expression
        <chr>  <chr>    <dbl> <chr>           <chr>       <list>    
      1 4      6        0.995 None            Student's t <language>
      2 4      8        1.000 None            Student's t <language>
      3 6      8        0.997 None            Student's t <language>

---

    Code
      df3[["expression"]]
    Output
      [[1]]
      list(italic(p)[unadj.] == "0.99")
      
      [[2]]
      list(italic(p)[unadj.] == "1.00")
      
      [[3]]
      list(italic(p)[unadj.] == "1.00")
      

---

    Code
      df4
    Output
      # A tibble: 3 x 6
        group1 group2     p.value p.adjust.method test        expression
        <chr>  <chr>        <dbl> <chr>           <chr>       <list>    
      1 4      6      0.00532     None            Student's t <language>
      2 4      8      0.000000103 None            Student's t <language>
      3 6      8      0.00258     None            Student's t <language>

---

    Code
      df4[["expression"]]
    Output
      [[1]]
      list(italic(p)[unadj.] == "5.32e-03")
      
      [[2]]
      list(italic(p)[unadj.] == "1.03e-07")
      
      [[3]]
      list(italic(p)[unadj.] == "2.58e-03")
      

