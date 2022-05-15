# long_to_wide_converter works - spread true

    Code
      list(df1, df2, df3, df4)
    Output
      [[1]]
      # A tibble: 150 x 5
         .rowid Petal.Length Petal.Width Sepal.Length Sepal.Width
          <int>        <dbl>       <dbl>        <dbl>       <dbl>
       1      1          1.4         0.2          5.1         3.5
       2      2          1.4         0.2          4.9         3  
       3      3          1.3         0.2          4.7         3.2
       4      4          1.5         0.2          4.6         3.1
       5      5          1.4         0.2          5           3.6
       6      6          1.7         0.4          5.4         3.9
       7      7          1.4         0.3          4.6         3.4
       8      8          1.5         0.2          5           3.4
       9      9          1.4         0.2          4.4         2.9
      10     10          1.5         0.1          4.9         3.1
      # ... with 140 more rows
      
      [[2]]
      # A tibble: 32 x 3
         .rowid am       wt
          <int> <fct> <dbl>
       1      1 0      3.22
       2      2 0      3.44
       3      3 0      3.46
       4      4 0      3.57
       5      5 0      3.19
       6      6 0      3.15
       7      7 0      3.44
       8      8 0      3.44
       9      9 0      4.07
      10     10 0      3.73
      # ... with 22 more rows
      
      [[3]]
      # A tibble: 88 x 5
         .rowid  HDHF  HDLF  LDHF  LDLF
          <int> <dbl> <dbl> <dbl> <dbl>
       1      1  10     9     6     6  
       2      3  10    10    10     5  
       3      4   9     6     9     6  
       4      5   8.5   5.5   6.5   3  
       5      6   3     7.5   0.5   2  
       6      7  10    10    10    10  
       7      8  10     9    10    10  
       8      9  10     6     9.5   9.5
       9     11   0     0     2.5   0  
      10     12  10     8.5   7.5   9.5
      # ... with 78 more rows
      
      [[4]]
      # A tibble: 51 x 3
         .rowid vore  brainwt
          <int> <fct>   <dbl>
       1      3 carni  0.07  
       2      4 carni  0.0108
       3      5 carni  0.0256
       4      7 carni  0.325 
       5      9 carni  0.0125
       6     12 carni  0.157 
       7     17 carni  0.0175
       8     18 carni  0.0445
       9     19 carni  0.0504
      10     21 herbi  0.423 
      # ... with 41 more rows
      

# long_to_wide_converter works - spread false

    Code
      list(df1, df2, df3, df4)
    Output
      [[1]]
      # A tibble: 600 x 3
         .rowid condition    value
          <int> <fct>        <dbl>
       1      1 Petal.Length   1.4
       2      1 Petal.Width    0.2
       3      1 Sepal.Length   5.1
       4      1 Sepal.Width    3.5
       5      2 Petal.Length   1.4
       6      2 Petal.Width    0.2
       7      2 Sepal.Length   4.9
       8      2 Sepal.Width    3  
       9      3 Petal.Length   1.3
      10      3 Petal.Width    0.2
      # ... with 590 more rows
      
      [[2]]
      # A tibble: 32 x 3
         .rowid am       wt
          <int> <fct> <dbl>
       1      1 0      3.22
       2      2 0      3.44
       3      3 0      3.46
       4      4 0      3.57
       5      5 0      3.19
       6      6 0      3.15
       7      7 0      3.44
       8      8 0      3.44
       9      9 0      4.07
      10     10 0      3.73
      # ... with 22 more rows
      
      [[3]]
      # A tibble: 352 x 3
         .rowid condition desire
          <int> <fct>      <dbl>
       1      1 HDHF          10
       2      1 HDLF           9
       3      1 LDHF           6
       4      1 LDLF           6
       5      3 HDHF          10
       6      3 HDLF          10
       7      3 LDHF          10
       8      3 LDLF           5
       9      4 HDHF           9
      10      4 HDLF           6
      # ... with 342 more rows
      
      [[4]]
      # A tibble: 51 x 3
         .rowid vore  brainwt
          <int> <fct>   <dbl>
       1      3 carni  0.07  
       2      4 carni  0.0108
       3      5 carni  0.0256
       4      7 carni  0.325 
       5      9 carni  0.0125
       6     12 carni  0.157 
       7     17 carni  0.0175
       8     18 carni  0.0445
       9     19 carni  0.0504
      10     21 herbi  0.423 
      # ... with 41 more rows
      

