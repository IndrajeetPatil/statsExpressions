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
      list(widehat(mu)[mean] == "7.853")
      
      [[2]]
      list(widehat(mu)[mean] == "6.742")
      
      [[3]]
      list(widehat(mu)[mean] == "7.379")
      
      [[4]]
      list(widehat(mu)[mean] == "5.715")
      
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
      list(widehat(mu)[MAP] == "9.980")
      
      [[14]]
      list(widehat(mu)[MAP] == "9.726")
      
      [[15]]
      list(widehat(mu)[MAP] == "9.851")
      
      [[16]]
      list(widehat(mu)[MAP] == "5.992")
      

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
      

