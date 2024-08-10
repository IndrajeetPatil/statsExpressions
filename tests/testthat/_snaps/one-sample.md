# one-sample-test without missing data:  type=bayes, conf.level=0.95, effsize.type=g

    Code
      names(res)
    Output
       [1] "term"               "effectsize"         "estimate"          
       [4] "conf.level"         "conf.low"           "conf.high"         
       [7] "pd"                 "prior.distribution" "prior.location"    
      [10] "prior.scale"        "bf10"               "method"            
      [13] "conf.method"        "log_e_bf10"         "n.obs"             
      [16] "expression"        

---

    Code
      res$bf10[[1L]]
    Output
      [1] 1.199152e+15

# one-sample-test without missing data:  type=bayes, conf.level=0.9, effsize.type=d

    Code
      names(res)
    Output
       [1] "term"               "effectsize"         "estimate"          
       [4] "conf.level"         "conf.low"           "conf.high"         
       [7] "pd"                 "prior.distribution" "prior.location"    
      [10] "prior.scale"        "bf10"               "method"            
      [13] "conf.method"        "log_e_bf10"         "n.obs"             
      [16] "expression"        

---

    Code
      res$bf10[[1L]]
    Output
      [1] 1.199152e+15

# one-sample-test without missing data:  type=parametric, conf.level=0.95, effsize.type=g

    Code
      select(res, -expression)
    Output
      # A tibble: 1 x 14
           mu statistic df.error  p.value method            alternative effectsize
        <dbl>     <dbl>    <dbl>    <dbl> <chr>             <chr>       <chr>     
      1     5      11.1       82 4.70e-18 One Sample t-test two.sided   Hedges' g 
        estimate conf.level conf.low conf.high conf.method conf.distribution n.obs
           <dbl>      <dbl>    <dbl>     <dbl> <chr>       <chr>             <int>
      1     1.21       0.95    0.925      1.49 ncp         t                    83

---

    Code
      res[["expression"]]
    Output
      [[1]]
      list(italic("t")["Student"] * "(" * 82 * ")" == "11.1235", italic(p) == 
          "4.6953e-18", widehat(italic("g"))["Hedges"] == "1.2098", 
          CI["95%"] ~ "[" * "0.9254", "1.4897" * "]", italic("n")["obs"] == 
              "83")
      

# one-sample-test without missing data:  type=parametric, conf.level=0.9, effsize.type=d

    Code
      select(res, -expression)
    Output
      # A tibble: 1 x 14
           mu statistic df.error  p.value method            alternative effectsize
        <dbl>     <dbl>    <dbl>    <dbl> <chr>             <chr>       <chr>     
      1     5      11.1       82 4.70e-18 One Sample t-test two.sided   Cohen's d 
        estimate conf.level conf.low conf.high conf.method conf.distribution n.obs
           <dbl>      <dbl>    <dbl>     <dbl> <chr>       <chr>             <int>
      1     1.22        0.9    0.979      1.46 ncp         t                    83

---

    Code
      res[["expression"]]
    Output
      [[1]]
      list(italic("t")["Student"] * "(" * 82 * ")" == "11.1235", italic(p) == 
          "4.6953e-18", widehat(italic("d"))["Cohen"] == "1.2210", 
          CI["90%"] ~ "[" * "0.9792", "1.4572" * "]", italic("n")["obs"] == 
              "83")
      

# one-sample-test without missing data:  type=nonparametric, conf.level=0.95, effsize.type=g

    Code
      select(res, -expression)
    Output
      # A tibble: 1 x 11
        statistic  p.value method                    alternative effectsize       
            <dbl>    <dbl> <chr>                     <chr>       <chr>            
      1     3338. 4.43e-13 Wilcoxon signed rank test two.sided   r (rank biserial)
        estimate conf.level conf.low conf.high conf.method n.obs
           <dbl>      <dbl>    <dbl>     <dbl> <chr>       <int>
      1    0.915       0.95    0.865     0.948 normal         83

---

    Code
      res[["expression"]]
    Output
      [[1]]
      list(italic("V")["Wilcoxon"] == "3338.5000", italic(p) == "4.4279e-13", 
          widehat(italic("r"))["biserial"]^"rank" == "0.9154", CI["95%"] ~ 
              "[" * "0.8648", "0.9476" * "]", italic("n")["obs"] == 
              "83")
      

# one-sample-test without missing data:  type=nonparametric, conf.level=0.9, effsize.type=d

    Code
      select(res, -expression)
    Output
      # A tibble: 1 x 11
        statistic  p.value method                    alternative effectsize       
            <dbl>    <dbl> <chr>                     <chr>       <chr>            
      1     3338. 4.43e-13 Wilcoxon signed rank test two.sided   r (rank biserial)
        estimate conf.level conf.low conf.high conf.method n.obs
           <dbl>      <dbl>    <dbl>     <dbl> <chr>       <int>
      1    0.915        0.9    0.874     0.943 normal         83

---

    Code
      res[["expression"]]
    Output
      [[1]]
      list(italic("V")["Wilcoxon"] == "3338.5000", italic(p) == "4.4279e-13", 
          widehat(italic("r"))["biserial"]^"rank" == "0.9154", CI["90%"] ~ 
              "[" * "0.8745", "0.9433" * "]", italic("n")["obs"] == 
              "83")
      

# one-sample-test without missing data:  type=robust, conf.level=0.95, effsize.type=g

    Code
      select(res, -expression)
    Output
      # A tibble: 1 x 9
        statistic p.value n.obs method                                 effectsize  
            <dbl>   <dbl> <int> <chr>                                  <chr>       
      1      9.70       0    83 Bootstrap-t method for one-sample test Trimmed mean
        estimate conf.level conf.low conf.high
           <dbl>      <dbl>    <dbl>     <dbl>
      1     10.5       0.95     9.34      11.6

---

    Code
      res[["expression"]]
    Output
      [[1]]
      list(italic("t")["bootstrapped"] == "9.7023", italic(p) == "0.0000", 
          widehat(mu)["trimmed"] == "10.4824", CI["95%"] ~ "[" * "9.3380", 
          "11.6267" * "]", italic("n")["obs"] == "83")
      

# one-sample-test without missing data:  type=robust, conf.level=0.9, effsize.type=d

    Code
      select(res, -expression)
    Output
      # A tibble: 1 x 9
        statistic p.value n.obs method                                 effectsize  
            <dbl>   <dbl> <int> <chr>                                  <chr>       
      1      9.70       0    83 Bootstrap-t method for one-sample test Trimmed mean
        estimate conf.level conf.low conf.high
           <dbl>      <dbl>    <dbl>     <dbl>
      1     10.5        0.9     9.61      11.4

---

    Code
      res[["expression"]]
    Output
      [[1]]
      list(italic("t")["bootstrapped"] == "9.7023", italic(p) == "0.0000", 
          widehat(mu)["trimmed"] == "10.4824", CI["90%"] ~ "[" * "9.6096", 
          "11.3551" * "]", italic("n")["obs"] == "83")
      

# one-sample-test with missing data:  type=bayes, conf.level=0.95, effsize.type=g

    Code
      names(res)
    Output
       [1] "term"               "effectsize"         "estimate"          
       [4] "conf.level"         "conf.low"           "conf.high"         
       [7] "pd"                 "prior.distribution" "prior.location"    
      [10] "prior.scale"        "bf10"               "method"            
      [13] "conf.method"        "log_e_bf10"         "n.obs"             
      [16] "expression"        

---

    Code
      res$bf10[[1L]]
    Output
      [1] 0.1500561

# one-sample-test with missing data:  type=bayes, conf.level=0.9, effsize.type=d

    Code
      names(res)
    Output
       [1] "term"               "effectsize"         "estimate"          
       [4] "conf.level"         "conf.low"           "conf.high"         
       [7] "pd"                 "prior.distribution" "prior.location"    
      [10] "prior.scale"        "bf10"               "method"            
      [13] "conf.method"        "log_e_bf10"         "n.obs"             
      [16] "expression"        

---

    Code
      res$bf10[[1L]]
    Output
      [1] 0.1500561

# one-sample-test with missing data:  type=parametric, conf.level=0.95, effsize.type=g

    Code
      select(res, -expression)
    Output
      # A tibble: 1 x 14
           mu statistic df.error p.value method            alternative effectsize
        <dbl>     <dbl>    <dbl>   <dbl> <chr>             <chr>       <chr>     
      1  0.25     0.242       55   0.810 One Sample t-test two.sided   Hedges' g 
        estimate conf.level conf.low conf.high conf.method conf.distribution n.obs
           <dbl>      <dbl>    <dbl>     <dbl> <chr>       <chr>             <int>
      1   0.0319       0.95   -0.227     0.290 ncp         t                    56

---

    Code
      res[["expression"]]
    Output
      [[1]]
      list(italic("t")["Student"] * "(" * 55 * ")" == "0.2420", italic(p) == 
          "0.8096", widehat(italic("g"))["Hedges"] == "0.0319", CI["95%"] ~ 
          "[" * "-0.2266", "0.2901" * "]", italic("n")["obs"] == "56")
      

# one-sample-test with missing data:  type=parametric, conf.level=0.9, effsize.type=d

    Code
      select(res, -expression)
    Output
      # A tibble: 1 x 14
           mu statistic df.error p.value method            alternative effectsize
        <dbl>     <dbl>    <dbl>   <dbl> <chr>             <chr>       <chr>     
      1  0.25     0.242       55   0.810 One Sample t-test two.sided   Cohen's d 
        estimate conf.level conf.low conf.high conf.method conf.distribution n.obs
           <dbl>      <dbl>    <dbl>     <dbl> <chr>       <chr>             <int>
      1   0.0323        0.9   -0.188     0.252 ncp         t                    56

---

    Code
      res[["expression"]]
    Output
      [[1]]
      list(italic("t")["Student"] * "(" * 55 * ")" == "0.2420", italic(p) == 
          "0.8096", widehat(italic("d"))["Cohen"] == "0.0323", CI["90%"] ~ 
          "[" * "-0.1877", "0.2521" * "]", italic("n")["obs"] == "56")
      

# one-sample-test with missing data:  type=nonparametric, conf.level=0.95, effsize.type=g

    Code
      select(res, -expression)
    Output
      # A tibble: 1 x 11
        statistic   p.value method                    alternative effectsize       
            <dbl>     <dbl> <chr>                     <chr>       <chr>            
      1       262 0.0000125 Wilcoxon signed rank test two.sided   r (rank biserial)
        estimate conf.level conf.low conf.high conf.method n.obs
           <dbl>      <dbl>    <dbl>     <dbl> <chr>       <int>
      1   -0.672       0.95   -0.806    -0.472 normal         56

---

    Code
      res[["expression"]]
    Output
      [[1]]
      list(italic("V")["Wilcoxon"] == "262.0000", italic(p) == "1.2527e-05", 
          widehat(italic("r"))["biserial"]^"rank" == "-0.6717", CI["95%"] ~ 
              "[" * "-0.8058", "-0.4720" * "]", italic("n")["obs"] == 
              "56")
      

# one-sample-test with missing data:  type=nonparametric, conf.level=0.9, effsize.type=d

    Code
      select(res, -expression)
    Output
      # A tibble: 1 x 11
        statistic   p.value method                    alternative effectsize       
            <dbl>     <dbl> <chr>                     <chr>       <chr>            
      1       262 0.0000125 Wilcoxon signed rank test two.sided   r (rank biserial)
        estimate conf.level conf.low conf.high conf.method n.obs
           <dbl>      <dbl>    <dbl>     <dbl> <chr>       <int>
      1   -0.672        0.9   -0.788    -0.509 normal         56

---

    Code
      res[["expression"]]
    Output
      [[1]]
      list(italic("V")["Wilcoxon"] == "262.0000", italic(p) == "1.2527e-05", 
          widehat(italic("r"))["biserial"]^"rank" == "-0.6717", CI["90%"] ~ 
              "[" * "-0.7881", "-0.5088" * "]", italic("n")["obs"] == 
              "56")
      

# one-sample-test with missing data:  type=robust, conf.level=0.95, effsize.type=g

    Code
      select(res, -expression)
    Output
      # A tibble: 1 x 9
        statistic p.value n.obs method                                 effectsize  
            <dbl>   <dbl> <int> <chr>                                  <chr>       
      1     -13.2   0.015    56 Bootstrap-t method for one-sample test Trimmed mean
        estimate conf.level conf.low conf.high
           <dbl>      <dbl>    <dbl>     <dbl>
      1   0.0390       0.95 0.000118    0.0778

---

    Code
      res[["expression"]]
    Output
      [[1]]
      list(italic("t")["bootstrapped"] == "-13.1637", italic(p) == 
          "0.0150", widehat(mu)["trimmed"] == "0.0390", CI["95%"] ~ 
          "[" * "0.0001", "0.0778" * "]", italic("n")["obs"] == "56")
      

# one-sample-test with missing data:  type=robust, conf.level=0.9, effsize.type=d

    Code
      select(res, -expression)
    Output
      # A tibble: 1 x 9
        statistic p.value n.obs method                                 effectsize  
            <dbl>   <dbl> <int> <chr>                                  <chr>       
      1     -13.2       0    56 Bootstrap-t method for one-sample test Trimmed mean
        estimate conf.level conf.low conf.high
           <dbl>      <dbl>    <dbl>     <dbl>
      1   0.0390        0.9  0.00933    0.0686

---

    Code
      res[["expression"]]
    Output
      [[1]]
      list(italic("t")["bootstrapped"] == "-13.1637", italic(p) == 
          "0.0000", widehat(mu)["trimmed"] == "0.0390", CI["90%"] ~ 
          "[" * "0.0093", "0.0686" * "]", italic("n")["obs"] == "56")
      

