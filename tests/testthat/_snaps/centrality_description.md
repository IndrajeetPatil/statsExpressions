# contingency_table works

    Code
      df
    Output
      # A tibble: 12 x 16
         Species    Sepal.Length n_obs variable     std.dev   iqr conf.low conf.high
         <fct>             <dbl> <int> <chr>          <dbl> <dbl>    <dbl>     <dbl>
       1 setosa             5.01    50 Sepal.Length   0.352 0.400     4.90      5.08
       2 versicolor         5.94    50 Sepal.Length   0.516 0.7       5.81      6.06
       3 virginica          6.59    50 Sepal.Length   0.636 0.750     6.42      6.74
       4 setosa             5       50 Sepal.Length  NA     0.400     4.9       5.1 
       5 versicolor         5.9     50 Sepal.Length  NA     0.7       5.7       6.1 
       6 virginica          6.5     50 Sepal.Length  NA     0.750     6.3       6.7 
       7 setosa             5       50 Sepal.Length   0.352 0.400     4.88      5.11
       8 versicolor         5.91    50 Sepal.Length   0.516 0.7       5.77      6.07
       9 virginica          6.55    50 Sepal.Length   0.636 0.750     6.42      6.75
      10 setosa             5.02    50 Sepal.Length  NA     0.400     4.92      5.11
      11 versicolor         5.75    50 Sepal.Length  NA     0.7       5.58      6.18
      12 virginica          6.40    50 Sepal.Length  NA     0.750     6.26      6.72
           min   max skewness kurtosis n.missing expression                          
         <dbl> <dbl>    <dbl>    <dbl>     <int> <chr>                               
       1   4.3   5.8    0.120  -0.253          0 list(~widehat(mu)[mean]=='5.01')    
       2   4.9   7      0.105  -0.533          0 list(~widehat(mu)[mean]=='5.94')    
       3   4.9   7.9    0.118   0.0329         0 list(~widehat(mu)[mean]=='6.59')    
       4   4.3   5.8    0.120  -0.253          0 list(~widehat(mu)[median]=='5.000') 
       5   4.9   7      0.105  -0.533          0 list(~widehat(mu)[median]=='5.900') 
       6   4.9   7.9    0.118   0.0329         0 list(~widehat(mu)[median]=='6.500') 
       7   4.3   5.8    0.120  -0.253          0 list(~widehat(mu)[trimmed]=='5.000')
       8   4.9   7      0.105  -0.533          0 list(~widehat(mu)[trimmed]=='5.910')
       9   4.9   7.9    0.118   0.0329         0 list(~widehat(mu)[trimmed]=='6.547')
      10   4.3   5.8    0.120  -0.253          0 list(~widehat(mu)[MAP]=='5.02')     
      11   4.9   7      0.105  -0.533          0 list(~widehat(mu)[MAP]=='5.75')     
      12   4.9   7.9    0.118   0.0329         0 list(~widehat(mu)[MAP]=='6.40')     
         n_label                   mad
         <chr>                   <dbl>
       1 "setosa\n(n = 50)"     NA    
       2 "versicolor\n(n = 50)" NA    
       3 "virginica\n(n = 50)"  NA    
       4 "setosa\n(n = 50)"      0.297
       5 "versicolor\n(n = 50)"  0.519
       6 "virginica\n(n = 50)"   0.593
       7 "setosa\n(n = 50)"     NA    
       8 "versicolor\n(n = 50)" NA    
       9 "virginica\n(n = 50)"  NA    
      10 "setosa\n(n = 50)"     NA    
      11 "versicolor\n(n = 50)" NA    
      12 "virginica\n(n = 50)"  NA    

---

    Code
      df_na
    Output
      # A tibble: 16 x 16
         condition desire n_obs variable std.dev   iqr conf.low conf.high   min   max
         <fct>      <dbl> <int> <chr>      <dbl> <dbl>    <dbl>     <dbl> <dbl> <dbl>
       1 HDHF        7.85    92 desire      2.47   4       7.42      8.33   0      10
       2 HDLF        6.74    91 desire      3.11   5       6.15      7.33   0      10
       3 LDHF        7.38    91 desire      2.52   3.5     6.96      7.95   0.5    10
       4 LDLF        5.72    93 desire      2.71   4       5.14      6.27   0      10
       5 HDHF        8.75    92 desire     NA      4       8         9.5    0      10
       6 HDLF        8       91 desire     NA      5       6.5       8.5    0      10
       7 LDHF        8       91 desire     NA      3.5     7.5       8.5    0.5    10
       8 LDLF        6       93 desire     NA      4       5         6      0      10
       9 HDHF        8.47    92 desire      2.47   4       7.71      8.74   0      10
      10 HDLF        7.32    91 desire      3.11   5       6.23      7.81   0      10
      11 LDHF        7.88    91 desire      2.52   3.5     7.05      8.17   0.5    10
      12 LDLF        5.72    93 desire      2.71   4       5.20      6.44   0      10
      13 HDHF        9.98    92 desire     NA      4       9.90     10      0      10
      14 HDLF        9.73    91 desire     NA      5       8.39      9.99   0      10
      15 LDHF        9.85    91 desire     NA      3.5     7.86      9.99   0.5    10
      16 LDLF        5.99    93 desire     NA      4       2.80      8.52   0      10
         skewness kurtosis n.missing expression                          
            <dbl>    <dbl>     <int> <chr>                               
       1   -1.13     0.486         0 list(~widehat(mu)[mean]=='7.85')    
       2   -0.740   -0.663         0 list(~widehat(mu)[mean]=='6.74')    
       3   -0.947    0.160         0 list(~widehat(mu)[mean]=='7.38')    
       4   -0.132   -0.761         0 list(~widehat(mu)[mean]=='5.72')    
       5   -1.13     0.486         0 list(~widehat(mu)[median]=='8.750') 
       6   -0.740   -0.663         0 list(~widehat(mu)[median]=='8.000') 
       7   -0.947    0.160         0 list(~widehat(mu)[median]=='8.000') 
       8   -0.132   -0.761         0 list(~widehat(mu)[median]=='6.000') 
       9   -1.13     0.486         0 list(~widehat(mu)[trimmed]=='8.473')
      10   -0.740   -0.663         0 list(~widehat(mu)[trimmed]=='7.318')
      11   -0.947    0.160         0 list(~widehat(mu)[trimmed]=='7.882')
      12   -0.132   -0.761         0 list(~widehat(mu)[trimmed]=='5.719')
      13   -1.13     0.486         0 list(~widehat(mu)[MAP]=='9.98')     
      14   -0.740   -0.663         0 list(~widehat(mu)[MAP]=='9.73')     
      15   -0.947    0.160         0 list(~widehat(mu)[MAP]=='9.85')     
      16   -0.132   -0.761         0 list(~widehat(mu)[MAP]=='5.99')     
         n_label            mad
         <chr>            <dbl>
       1 "HDHF\n(n = 92)" NA   
       2 "HDLF\n(n = 91)" NA   
       3 "LDHF\n(n = 91)" NA   
       4 "LDLF\n(n = 93)" NA   
       5 "HDHF\n(n = 92)"  1.85
       6 "HDLF\n(n = 91)"  2.97
       7 "LDHF\n(n = 91)"  2.97
       8 "LDLF\n(n = 93)"  2.97
       9 "HDHF\n(n = 92)" NA   
      10 "HDLF\n(n = 91)" NA   
      11 "LDHF\n(n = 91)" NA   
      12 "LDLF\n(n = 93)" NA   
      13 "HDHF\n(n = 92)" NA   
      14 "HDLF\n(n = 91)" NA   
      15 "LDHF\n(n = 91)" NA   
      16 "LDLF\n(n = 93)" NA   

