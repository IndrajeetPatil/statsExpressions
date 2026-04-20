# pairwise_contingency_table works - basic

    Code
      select(df1, -expression)
    Output
      # A tibble: 3 x 13
        group1 group2 p.value p.value.adj estimate conf.level conf.low conf.high
        <chr>  <chr>    <dbl>       <dbl>    <dbl>      <dbl>    <dbl>     <dbl>
      1 4      6      0.332        0.997     0.180       0.95        0     0.743
      2 4      8      0.00514      0.0154    0.568       0.95        0     0.983
      3 6      8      0.280        0.840     0.229       0.95        0     0.728
        effectsize        conf.method conf.distribution p.adjust.method
        <chr>             <chr>       <chr>             <chr>          
      1 Cramer's V (adj.) ncp         chisq             Bonferroni     
      2 Cramer's V (adj.) ncp         chisq             Bonferroni     
      3 Cramer's V (adj.) ncp         chisq             Bonferroni     
        test               
        <chr>              
      1 Fisher's exact test
      2 Fisher's exact test
      3 Fisher's exact test

---

    Code
      df1[["expression"]]
    Output
      [[1]]
      list(italic(p)["Bonferroni" - adj.] == "1.00")
      
      [[2]]
      list(italic(p)["Bonferroni" - adj.] == "0.02")
      
      [[3]]
      list(italic(p)["Bonferroni" - adj.] == "0.84")
      

# pairwise_contingency_table works - with counts

    Code
      select(df1, -expression)
    Output
      # A tibble: 6 x 13
        group1 group2  p.value p.value.adj estimate conf.level conf.low conf.high
        <chr>  <chr>     <dbl>       <dbl>    <dbl>      <dbl>    <dbl>     <dbl>
      1 1st    2nd    2.78e- 7    4.17e- 7    0.207       0.95   0.125     0.287 
      2 1st    3rd    3.68e-30    1.10e-29    0.357       0.95   0.296     0.419 
      3 1st    Crew   1.81e-34    1.09e-33    0.359       0.95   0.302     0.415 
      4 2nd    3rd    8.19e- 7    9.83e- 7    0.157       0.95   0.0926    0.220 
      5 2nd    Crew   2.77e- 8    5.54e- 8    0.164       0.95   0.105     0.222 
      6 3rd    Crew   5.98e- 1    5.98e- 1    0           0.95   0         0.0581
        effectsize        conf.method conf.distribution p.adjust.method
        <chr>             <chr>       <chr>             <chr>          
      1 Cramer's V (adj.) ncp         chisq             FDR            
      2 Cramer's V (adj.) ncp         chisq             FDR            
      3 Cramer's V (adj.) ncp         chisq             FDR            
      4 Cramer's V (adj.) ncp         chisq             FDR            
      5 Cramer's V (adj.) ncp         chisq             FDR            
      6 Cramer's V (adj.) ncp         chisq             FDR            
        test               
        <chr>              
      1 Fisher's exact test
      2 Fisher's exact test
      3 Fisher's exact test
      4 Fisher's exact test
      5 Fisher's exact test
      6 Fisher's exact test

---

    Code
      df1[["expression"]]
    Output
      [[1]]
      list(italic(p)["FDR" - adj.] == "4.17e-07")
      
      [[2]]
      list(italic(p)["FDR" - adj.] == "1.10e-29")
      
      [[3]]
      list(italic(p)["FDR" - adj.] == "1.09e-33")
      
      [[4]]
      list(italic(p)["FDR" - adj.] == "9.83e-07")
      
      [[5]]
      list(italic(p)["FDR" - adj.] == "5.54e-08")
      
      [[6]]
      list(italic(p)["FDR" - adj.] == "0.60")
      

# pairwise_contingency_table works - no adjustment

    Code
      select(df1, -expression)
    Output
      # A tibble: 3 x 13
        group1 group2 p.value p.value.adj estimate conf.level conf.low conf.high
        <chr>  <chr>    <dbl>       <dbl>    <dbl>      <dbl>    <dbl>     <dbl>
      1 4      6      0.332       0.332      0.180       0.95        0     0.743
      2 4      8      0.00514     0.00514    0.568       0.95        0     0.983
      3 6      8      0.280       0.280      0.229       0.95        0     0.728
        effectsize        conf.method conf.distribution p.adjust.method
        <chr>             <chr>       <chr>             <chr>          
      1 Cramer's V (adj.) ncp         chisq             None           
      2 Cramer's V (adj.) ncp         chisq             None           
      3 Cramer's V (adj.) ncp         chisq             None           
        test               
        <chr>              
      1 Fisher's exact test
      2 Fisher's exact test
      3 Fisher's exact test

---

    Code
      df1[["expression"]]
    Output
      [[1]]
      list(italic(p)[unadj.] == "0.33")
      
      [[2]]
      list(italic(p)[unadj.] == "5.14e-03")
      
      [[3]]
      list(italic(p)[unadj.] == "0.28")
      

# pairwise_contingency_table works - data with NAs

    Code
      select(df1, -expression)
    Output
      # A tibble: 6 x 13
        group1  group2  p.value p.value.adj estimate conf.level conf.low conf.high
        <chr>   <chr>     <dbl>       <dbl>    <dbl>      <dbl>    <dbl>     <dbl>
      1 carni   herbi     0.783       1        0           0.95        0     0.180
      2 carni   insecti   0.712       1        0           0.95        0     0.499
      3 carni   omni      0.138       0.828    0.306       0.95        0     0.713
      4 herbi   insecti   0.537       1        0           0.95        0     0.395
      5 herbi   omni      0.292       1        0.243       0.95        0     0.569
      6 insecti omni      0.455       1        0.343       0.95        0     1    
        effectsize        conf.method conf.distribution p.adjust.method
        <chr>             <chr>       <chr>             <chr>          
      1 Cramer's V (adj.) ncp         chisq             Holm           
      2 Cramer's V (adj.) ncp         chisq             Holm           
      3 Cramer's V (adj.) ncp         chisq             Holm           
      4 Cramer's V (adj.) ncp         chisq             Holm           
      5 Cramer's V (adj.) ncp         chisq             Holm           
      6 Cramer's V (adj.) ncp         chisq             Holm           
        test               
        <chr>              
      1 Fisher's exact test
      2 Fisher's exact test
      3 Fisher's exact test
      4 Fisher's exact test
      5 Fisher's exact test
      6 Fisher's exact test

---

    Code
      df1[["expression"]]
    Output
      [[1]]
      list(italic(p)["Holm" - adj.] == "1.00")
      
      [[2]]
      list(italic(p)["Holm" - adj.] == "1.00")
      
      [[3]]
      list(italic(p)["Holm" - adj.] == "0.83")
      
      [[4]]
      list(italic(p)["Holm" - adj.] == "1.00")
      
      [[5]]
      list(italic(p)["Holm" - adj.] == "1.00")
      
      [[6]]
      list(italic(p)["Holm" - adj.] == "1.00")
      

# pairwise_contingency_table works - custom conf.level and alternative

    Code
      select(df1, -expression)
    Output
      # A tibble: 3 x 13
        group1 group2 p.value p.value.adj estimate conf.level conf.low conf.high
        <chr>  <chr>    <dbl>       <dbl>    <dbl>      <dbl>    <dbl>     <dbl>
      1 4      6        0.961           1    0.180        0.9    0             1
      2 4      8        1.000           1    0.568        0.9    0.272         1
      3 6      8        0.975           1    0.229        0.9    0             1
        effectsize        conf.method conf.distribution p.adjust.method
        <chr>             <chr>       <chr>             <chr>          
      1 Cramer's V (adj.) ncp         chisq             Holm           
      2 Cramer's V (adj.) ncp         chisq             Holm           
      3 Cramer's V (adj.) ncp         chisq             Holm           
        test               
        <chr>              
      1 Fisher's exact test
      2 Fisher's exact test
      3 Fisher's exact test

---

    Code
      df1[["expression"]]
    Output
      [[1]]
      list(italic(p)["Holm" - adj.] == "1.00")
      
      [[2]]
      list(italic(p)["Holm" - adj.] == "1.00")
      
      [[3]]
      list(italic(p)["Holm" - adj.] == "1.00")
      

# pairwise_contingency_table works - custom digits

    Code
      select(df1, -expression)
    Output
      # A tibble: 3 x 13
        group1 group2 p.value p.value.adj estimate conf.level conf.low conf.high
        <chr>  <chr>    <dbl>       <dbl>    <dbl>      <dbl>    <dbl>     <dbl>
      1 4      6      0.332        0.332     0.180       0.95        0     0.743
      2 4      8      0.00514      0.0154    0.568       0.95        0     0.983
      3 6      8      0.280        0.332     0.229       0.95        0     0.728
        effectsize        conf.method conf.distribution p.adjust.method
        <chr>             <chr>       <chr>             <chr>          
      1 Cramer's V (adj.) ncp         chisq             FDR            
      2 Cramer's V (adj.) ncp         chisq             FDR            
      3 Cramer's V (adj.) ncp         chisq             FDR            
        test               
        <chr>              
      1 Fisher's exact test
      2 Fisher's exact test
      3 Fisher's exact test

---

    Code
      df1[["expression"]]
    Output
      [[1]]
      list(italic(p)["FDR" - adj.] == "0.3322")
      
      [[2]]
      list(italic(p)["FDR" - adj.] == "0.0154")
      
      [[3]]
      list(italic(p)["FDR" - adj.] == "0.3322")
      

