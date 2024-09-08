# centrality description works as expected - no missing data

    Code
      select(df, -expression)
    Output
      # A tibble: 8 x 14
           am    wt std.dev   iqr conf.low conf.high   min   max skewness kurtosis
        <dbl> <dbl>   <dbl> <dbl>    <dbl>     <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1     0  3.77   0.777 0.41      3.48      4.09  2.46  5.42    1.15     1.06 
      2     1  2.41   0.617 0.942     2.20      2.69  1.51  3.57    0.269   -0.654
      3     0  3.52  NA     0.41      3.44      3.84  2.46  5.42    1.15     1.06 
      4     1  2.32  NA     0.942     1.99      3.17  1.51  3.57    0.269   -0.654
      5     0  3.60   0.777 0.41      3.46      4.05  2.46  5.42    1.15     1.06 
      6     1  2.39   0.617 0.942     2.06      2.74  1.51  3.57    0.269   -0.654
      7     0  3.47  NA     0.41      3.44      3.53  2.46  5.42    1.15     1.06 
      8     1  2.34  NA     0.942     2.01      2.76  1.51  3.57    0.269   -0.654
        n.obs missing.obs n.expression     mad
        <int>       <int> <chr>          <dbl>
      1    19           0 "0\n(n = 19)" NA    
      2    13           0 "1\n(n = 13)" NA    
      3    19           0 "0\n(n = 19)"  0.452
      4    13           0 "1\n(n = 13)"  0.682
      5    19           0 "0\n(n = 19)" NA    
      6    13           0 "1\n(n = 13)" NA    
      7    19           0 "0\n(n = 19)" NA    
      8    13           0 "1\n(n = 13)" NA    

---

    Code
      df[["expression"]]
    Output
      [[1]]
      list(widehat(mu)[mean] == "3.769")
      
      [[2]]
      list(widehat(mu)[mean] == "2.411")
      
      [[3]]
      list(widehat(mu)[median] == "3.520")
      
      [[4]]
      list(widehat(mu)[median] == "2.320")
      
      [[5]]
      list(widehat(mu)[trimmed] == "3.599")
      
      [[6]]
      list(widehat(mu)[trimmed] == "2.386")
      
      [[7]]
      list(widehat(mu)[MAP] == "3.472")
      
      [[8]]
      list(widehat(mu)[MAP] == "2.339")
      

# centrality description works as expected - missing data

    Code
      select(df_na, -expression)
    Output
      # A tibble: 8 x 14
        gender desire std.dev   iqr conf.low conf.high   min   max skewness kurtosis
        <fct>   <dbl>   <dbl> <dbl>    <dbl>     <dbl> <dbl> <dbl>    <dbl>    <dbl>
      1 Female   7.07    2.87     5     6.77      7.36     0    10   -0.733   -0.550
      2 Male     6.53    2.73     4     6.20      6.98     0    10   -0.628   -0.447
      3 Female   8      NA        5     7         8.5      0    10   -0.733   -0.550
      4 Male     7      NA        4     6         8        0    10   -0.628   -0.447
      5 Female   7.63    2.87     5     7.05      7.69     0    10   -0.733   -0.550
      6 Male     6.86    2.73     4     6.27      7.27     0    10   -0.628   -0.447
      7 Female  10      NA        5     9.99     10        0    10   -0.733   -0.550
      8 Male     7.89   NA        4     7.57      8.91     0    10   -0.628   -0.447
        n.obs missing.obs n.expression          mad
        <int>       <int> <chr>               <dbl>
      1   247           0 "Female\n(n = 247)" NA   
      2   116           0 "Male\n(n = 116)"   NA   
      3   247           0 "Female\n(n = 247)"  2.97
      4   116           0 "Male\n(n = 116)"    2.97
      5   247           0 "Female\n(n = 247)" NA   
      6   116           0 "Male\n(n = 116)"   NA   
      7   247           0 "Female\n(n = 247)" NA   
      8   116           0 "Male\n(n = 116)"   NA   

---

    Code
      df_na[["expression"]]
    Output
      [[1]]
      list(widehat(mu)[mean] == "7.073")
      
      [[2]]
      list(widehat(mu)[mean] == "6.534")
      
      [[3]]
      list(widehat(mu)[median] == "8.000")
      
      [[4]]
      list(widehat(mu)[median] == "7.000")
      
      [[5]]
      list(widehat(mu)[trimmed] == "7.631")
      
      [[6]]
      list(widehat(mu)[trimmed] == "6.857")
      
      [[7]]
      list(widehat(mu)[MAP] == "10.000")
      
      [[8]]
      list(widehat(mu)[MAP] == "7.889")
      

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
      

