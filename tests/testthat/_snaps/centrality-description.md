# centrality description works as expected - no missing data

    Code
      select(df, -expression)
    Output
      # A tibble: 12 x 14
         Species    Sepal.Length std.dev   iqr conf.low conf.high   min   max skewness
         <fct>             <dbl>   <dbl> <dbl>    <dbl>     <dbl> <dbl> <dbl>    <dbl>
       1 setosa             5.01   0.352 0.400     4.93      5.09   4.3   5.8    0.120
       2 versicolor         5.94   0.516 0.7       5.82      6.05   4.9   7      0.105
       3 virginica          6.59   0.636 0.750     6.46      6.75   4.9   7.9    0.118
       4 setosa             5     NA     0.400     4.9       5.1    4.3   5.8    0.120
       5 versicolor         5.9   NA     0.7       5.65      6.1    4.9   7      0.105
       6 virginica          6.5   NA     0.750     6.32      6.7    4.9   7.9    0.118
       7 setosa             5      0.352 0.400     4.92      5.09   4.3   5.8    0.120
       8 versicolor         5.91   0.516 0.7       5.81      6.07   4.9   7      0.105
       9 virginica          6.55   0.636 0.750     6.42      6.71   4.9   7.9    0.118
      10 setosa             5.02  NA     0.400     5.00      5.05   4.3   5.8    0.120
      11 versicolor         5.75  NA     0.7       5.63      5.85   4.9   7      0.105
      12 virginica          6.40  NA     0.750     6.34      6.42   4.9   7.9    0.118
         kurtosis n.obs missing.obs n.expression              mad
            <dbl> <int>       <int> <chr>                   <dbl>
       1  -0.253     50           0 "setosa\n(n = 50)"     NA    
       2  -0.533     50           0 "versicolor\n(n = 50)" NA    
       3   0.0329    50           0 "virginica\n(n = 50)"  NA    
       4  -0.253     50           0 "setosa\n(n = 50)"      0.297
       5  -0.533     50           0 "versicolor\n(n = 50)"  0.519
       6   0.0329    50           0 "virginica\n(n = 50)"   0.593
       7  -0.253     50           0 "setosa\n(n = 50)"     NA    
       8  -0.533     50           0 "versicolor\n(n = 50)" NA    
       9   0.0329    50           0 "virginica\n(n = 50)"  NA    
      10  -0.253     50           0 "setosa\n(n = 50)"     NA    
      11  -0.533     50           0 "versicolor\n(n = 50)" NA    
      12   0.0329    50           0 "virginica\n(n = 50)"  NA    

---

    Code
      df[["expression"]]
    Output
      [[1]]
      list(widehat(mu)[mean] == "5.01")
      
      [[2]]
      list(widehat(mu)[mean] == "5.94")
      
      [[3]]
      list(widehat(mu)[mean] == "6.59")
      
      [[4]]
      list(widehat(mu)[median] == "5.000")
      
      [[5]]
      list(widehat(mu)[median] == "5.900")
      
      [[6]]
      list(widehat(mu)[median] == "6.500")
      
      [[7]]
      list(widehat(mu)[trimmed] == "5.000")
      
      [[8]]
      list(widehat(mu)[trimmed] == "5.910")
      
      [[9]]
      list(widehat(mu)[trimmed] == "6.547")
      
      [[10]]
      list(widehat(mu)[MAP] == "5.02")
      
      [[11]]
      list(widehat(mu)[MAP] == "5.75")
      
      [[12]]
      list(widehat(mu)[MAP] == "6.40")
      

# centrality description works as expected - missing data

    Code
      select(df_na, -expression)
    Output
      # A tibble: 16 x 14
         condition desire std.dev   iqr conf.low conf.high   min   max skewness
         <chr>      <dbl>   <dbl> <dbl>    <dbl>     <dbl> <dbl> <dbl>    <dbl>
       1 HDHF        7.85    2.47   4       7.50      8.20   0      10   -1.13 
       2 HDLF        6.74    3.11   5       6.22      7.26   0      10   -0.740
       3 LDHF        7.38    2.52   3.5     7.01      7.88   0.5    10   -0.947
       4 LDLF        5.72    2.71   4       5.34      6.10   0      10   -0.132
       5 HDHF        8.75   NA      4       8         9.88   0      10   -1.13 
       6 HDLF        8      NA      5       6         8.5    0      10   -0.740
       7 LDHF        8      NA      3.5     7.25      8.5    0.5    10   -0.947
       8 LDLF        6      NA      4       5         6.25   0      10   -0.132
       9 HDHF        8.47    2.47   4       7.73      8.67   0      10   -1.13 
      10 HDLF        7.32    3.11   5       6.35      7.77   0      10   -0.740
      11 LDHF        7.88    2.52   3.5     7.12      8.12   0.5    10   -0.947
      12 LDLF        5.72    2.71   4       5.27      6.35   0      10   -0.132
      13 HDHF        9.98   NA      4       9.97      9.99   0      10   -1.13 
      14 HDLF        9.73   NA      5       9.10      9.92   0      10   -0.740
      15 LDHF        9.85   NA      3.5     9.82      9.97   0.5    10   -0.947
      16 LDLF        5.99   NA      4       5.58      6.26   0      10   -0.132
         kurtosis n.obs missing.obs n.expression       mad
            <dbl> <int>       <int> <chr>            <dbl>
       1    0.486    92           0 "HDHF\n(n = 92)" NA   
       2   -0.663    91           0 "HDLF\n(n = 91)" NA   
       3    0.160    91           0 "LDHF\n(n = 91)" NA   
       4   -0.761    93           0 "LDLF\n(n = 93)" NA   
       5    0.486    92           0 "HDHF\n(n = 92)"  1.85
       6   -0.663    91           0 "HDLF\n(n = 91)"  2.97
       7    0.160    91           0 "LDHF\n(n = 91)"  2.97
       8   -0.761    93           0 "LDLF\n(n = 93)"  2.97
       9    0.486    92           0 "HDHF\n(n = 92)" NA   
      10   -0.663    91           0 "HDLF\n(n = 91)" NA   
      11    0.160    91           0 "LDHF\n(n = 91)" NA   
      12   -0.761    93           0 "LDLF\n(n = 93)" NA   
      13    0.486    92           0 "HDHF\n(n = 92)" NA   
      14   -0.663    91           0 "HDLF\n(n = 91)" NA   
      15    0.160    91           0 "LDHF\n(n = 91)" NA   
      16   -0.761    93           0 "LDLF\n(n = 93)" NA   

---

    Code
      df_na[["expression"]]
    Output
      [[1]]
      list(widehat(mu)[mean] == "7.85")
      
      [[2]]
      list(widehat(mu)[mean] == "6.74")
      
      [[3]]
      list(widehat(mu)[mean] == "7.38")
      
      [[4]]
      list(widehat(mu)[mean] == "5.72")
      
      [[5]]
      list(widehat(mu)[median] == "8.750")
      
      [[6]]
      list(widehat(mu)[median] == "8.000")
      
      [[7]]
      list(widehat(mu)[median] == "8.000")
      
      [[8]]
      list(widehat(mu)[median] == "6.000")
      
      [[9]]
      list(widehat(mu)[trimmed] == "8.473")
      
      [[10]]
      list(widehat(mu)[trimmed] == "7.318")
      
      [[11]]
      list(widehat(mu)[trimmed] == "7.882")
      
      [[12]]
      list(widehat(mu)[trimmed] == "5.719")
      
      [[13]]
      list(widehat(mu)[MAP] == "9.98")
      
      [[14]]
      list(widehat(mu)[MAP] == "9.73")
      
      [[15]]
      list(widehat(mu)[MAP] == "9.85")
      
      [[16]]
      list(widehat(mu)[MAP] == "5.99")
      

# centrality description works when variable is named `variable`

    Code
      select(res, -expression)
    Output
      # A tibble: 3 x 11
        variable    wt std.dev   iqr   min   max skewness kurtosis n.obs missing.obs
           <dbl> <dbl>   <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl> <int>       <int>
      1        4  2.29   0.570 0.945  1.51  3.19    0.404  -0.851     11           0
      2        6  3.12   0.356 0.67   2.62  3.46   -0.363  -2.08       7           0
      3        8  4.00   0.759 0.865  3.17  5.42    1.24    0.0780    14           0
        n.expression 
        <chr>        
      1 "4\n(n = 11)"
      2 "6\n(n = 7)" 
      3 "8\n(n = 14)"

---

    Code
      res[["expression"]]
    Output
      [[1]]
      list(widehat(mu)[mean] == "2.29")
      
      [[2]]
      list(widehat(mu)[mean] == "3.12")
      
      [[3]]
      list(widehat(mu)[mean] == "4.00")
      

