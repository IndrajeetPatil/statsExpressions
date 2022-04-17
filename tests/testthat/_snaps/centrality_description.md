# centrality description works as expected

    Code
      select(df, -expression)
    Output
      # A tibble: 12 x 15
         Species    Sepal.Length variable     std.dev   iqr conf.low conf.high   min
         <fct>             <dbl> <chr>          <dbl> <dbl>    <dbl>     <dbl> <dbl>
       1 setosa             5.01 Sepal.Length   0.352 0.400     4.90      5.10   4.3
       2 versicolor         5.94 Sepal.Length   0.516 0.7       5.80      6.07   4.9
       3 virginica          6.59 Sepal.Length   0.636 0.750     6.39      6.79   4.9
       4 setosa             5    Sepal.Length  NA     0.400     4.9       5.1    4.3
       5 versicolor         5.9  Sepal.Length  NA     0.7       5.7       6.1    4.9
       6 virginica          6.5  Sepal.Length  NA     0.750     6.4       6.7    4.9
       7 setosa             5    Sepal.Length   0.352 0.400     4.92      5.10   4.3
       8 versicolor         5.91 Sepal.Length   0.516 0.7       5.77      6.09   4.9
       9 virginica          6.55 Sepal.Length   0.636 0.750     6.37      6.74   4.9
      10 setosa             5.02 Sepal.Length  NA     0.400     4.91      5.11   4.3
      11 versicolor         5.75 Sepal.Length  NA     0.7       5.57      6.13   4.9
      12 virginica          6.40 Sepal.Length  NA     0.750     6.26      6.53   4.9
           max skewness kurtosis n.obs missing.obs n.expression              mad
         <dbl>    <dbl>    <dbl> <int>       <int> <chr>                   <dbl>
       1   5.8    0.120  -0.253     50           0 "setosa\n(n = 50)"     NA    
       2   7      0.105  -0.533     50           0 "versicolor\n(n = 50)" NA    
       3   7.9    0.118   0.0329    50           0 "virginica\n(n = 50)"  NA    
       4   5.8    0.120  -0.253     50           0 "setosa\n(n = 50)"      0.297
       5   7      0.105  -0.533     50           0 "versicolor\n(n = 50)"  0.519
       6   7.9    0.118   0.0329    50           0 "virginica\n(n = 50)"   0.593
       7   5.8    0.120  -0.253     50           0 "setosa\n(n = 50)"     NA    
       8   7      0.105  -0.533     50           0 "versicolor\n(n = 50)" NA    
       9   7.9    0.118   0.0329    50           0 "virginica\n(n = 50)"  NA    
      10   5.8    0.120  -0.253     50           0 "setosa\n(n = 50)"     NA    
      11   7      0.105  -0.533     50           0 "versicolor\n(n = 50)" NA    
      12   7.9    0.118   0.0329    50           0 "virginica\n(n = 50)"  NA    

---

    Code
      unlist(df$expression)
    Output
      [[1]]
      widehat(mu)[mean] == "5.01"
      
      [[2]]
      widehat(mu)[mean] == "5.94"
      
      [[3]]
      widehat(mu)[mean] == "6.59"
      
      [[4]]
      widehat(mu)[median] == "5.000"
      
      [[5]]
      widehat(mu)[median] == "5.900"
      
      [[6]]
      widehat(mu)[median] == "6.500"
      
      [[7]]
      widehat(mu)[trimmed] == "5.000"
      
      [[8]]
      widehat(mu)[trimmed] == "5.910"
      
      [[9]]
      widehat(mu)[trimmed] == "6.547"
      
      [[10]]
      widehat(mu)[MAP] == "5.02"
      
      [[11]]
      widehat(mu)[MAP] == "5.75"
      
      [[12]]
      widehat(mu)[MAP] == "6.40"
      

---

    Code
      select(df_na, -expression)
    Output
      # A tibble: 16 x 15
         condition desire variable std.dev   iqr conf.low conf.high   min   max
         <fct>      <dbl> <chr>      <dbl> <dbl>    <dbl>     <dbl> <dbl> <dbl>
       1 HDHF        7.85 desire      2.47   4       7.42      8.33   0      10
       2 HDLF        6.74 desire      3.11   5       6.15      7.33   0      10
       3 LDHF        7.38 desire      2.52   3.5     6.96      7.95   0.5    10
       4 LDLF        5.72 desire      2.71   4       5.14      6.27   0      10
       5 HDHF        8.75 desire     NA      4       8         9.5    0      10
       6 HDLF        8    desire     NA      5       6.5       8.5    0      10
       7 LDHF        8    desire     NA      3.5     7.5       8.5    0.5    10
       8 LDLF        6    desire     NA      4       5         6      0      10
       9 HDHF        8.47 desire      2.47   4       7.71      8.74   0      10
      10 HDLF        7.32 desire      3.11   5       6.23      7.81   0      10
      11 LDHF        7.88 desire      2.52   3.5     7.05      8.17   0.5    10
      12 LDLF        5.72 desire      2.71   4       5.20      6.44   0      10
      13 HDHF        9.98 desire     NA      4       9.90     10      0      10
      14 HDLF        9.73 desire     NA      5       8.39      9.99   0      10
      15 LDHF        9.85 desire     NA      3.5     7.86      9.99   0.5    10
      16 LDLF        5.99 desire     NA      4       2.80      8.52   0      10
         skewness kurtosis n.obs missing.obs n.expression       mad
            <dbl>    <dbl> <int>       <int> <chr>            <dbl>
       1   -1.13     0.486    92           0 "HDHF\n(n = 92)" NA   
       2   -0.740   -0.663    91           0 "HDLF\n(n = 91)" NA   
       3   -0.947    0.160    91           0 "LDHF\n(n = 91)" NA   
       4   -0.132   -0.761    93           0 "LDLF\n(n = 93)" NA   
       5   -1.13     0.486    92           0 "HDHF\n(n = 92)"  1.85
       6   -0.740   -0.663    91           0 "HDLF\n(n = 91)"  2.97
       7   -0.947    0.160    91           0 "LDHF\n(n = 91)"  2.97
       8   -0.132   -0.761    93           0 "LDLF\n(n = 93)"  2.97
       9   -1.13     0.486    92           0 "HDHF\n(n = 92)" NA   
      10   -0.740   -0.663    91           0 "HDLF\n(n = 91)" NA   
      11   -0.947    0.160    91           0 "LDHF\n(n = 91)" NA   
      12   -0.132   -0.761    93           0 "LDLF\n(n = 93)" NA   
      13   -1.13     0.486    92           0 "HDHF\n(n = 92)" NA   
      14   -0.740   -0.663    91           0 "HDLF\n(n = 91)" NA   
      15   -0.947    0.160    91           0 "LDHF\n(n = 91)" NA   
      16   -0.132   -0.761    93           0 "LDLF\n(n = 93)" NA   

---

    Code
      unlist(df_na$expression)
    Output
      [[1]]
      widehat(mu)[mean] == "7.85"
      
      [[2]]
      widehat(mu)[mean] == "6.74"
      
      [[3]]
      widehat(mu)[mean] == "7.38"
      
      [[4]]
      widehat(mu)[mean] == "5.72"
      
      [[5]]
      widehat(mu)[median] == "8.750"
      
      [[6]]
      widehat(mu)[median] == "8.000"
      
      [[7]]
      widehat(mu)[median] == "8.000"
      
      [[8]]
      widehat(mu)[median] == "6.000"
      
      [[9]]
      widehat(mu)[trimmed] == "8.473"
      
      [[10]]
      widehat(mu)[trimmed] == "7.318"
      
      [[11]]
      widehat(mu)[trimmed] == "7.882"
      
      [[12]]
      widehat(mu)[trimmed] == "5.719"
      
      [[13]]
      widehat(mu)[MAP] == "9.98"
      
      [[14]]
      widehat(mu)[MAP] == "9.73"
      
      [[15]]
      widehat(mu)[MAP] == "9.85"
      
      [[16]]
      widehat(mu)[MAP] == "5.99"
      

