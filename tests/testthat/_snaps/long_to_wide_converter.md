# long_to_wide_converter works - spread true

    Code
      purrr::walk(list(df1, df2, df3, df4), dplyr::glimpse)
    Output
      Rows: 150
      Columns: 5
      $ .rowid       <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17~
      $ Petal.Length <dbl> 1.4, 1.4, 1.3, 1.5, 1.4, 1.7, 1.4, 1.5, 1.4, 1.5, 1.5, 1.~
      $ Petal.Width  <dbl> 0.2, 0.2, 0.2, 0.2, 0.2, 0.4, 0.3, 0.2, 0.2, 0.1, 0.2, 0.~
      $ Sepal.Length <dbl> 5.1, 4.9, 4.7, 4.6, 5.0, 5.4, 4.6, 5.0, 4.4, 4.9, 5.4, 4.~
      $ Sepal.Width  <dbl> 3.5, 3.0, 3.2, 3.1, 3.6, 3.9, 3.4, 3.4, 2.9, 3.1, 3.7, 3.~
      Rows: 32
      Columns: 3
      $ .rowid <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, ~
      $ `0`    <dbl> 3.215, 3.440, 3.460, 3.570, 3.190, 3.150, 3.440, 3.440, 4.070, ~
      $ `1`    <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,~
      Rows: 88
      Columns: 5
      $ .rowid <int> 1, 3, 4, 5, 6, 7, 8, 9, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20,~
      $ HDHF   <dbl> 10.0, 10.0, 9.0, 8.5, 3.0, 10.0, 10.0, 10.0, 0.0, 10.0, 8.5, 8.~
      $ HDLF   <dbl> 9.0, 10.0, 6.0, 5.5, 7.5, 10.0, 9.0, 6.0, 0.0, 8.5, 6.5, 4.0, 6~
      $ LDHF   <dbl> 6.0, 10.0, 9.0, 6.5, 0.5, 10.0, 10.0, 9.5, 2.5, 7.5, 8.5, 8.0, ~
      $ LDLF   <dbl> 6.0, 5.0, 6.0, 3.0, 2.0, 10.0, 10.0, 9.5, 0.0, 9.5, 7.0, 3.0, 4~
      Rows: 51
      Columns: 5
      $ .rowid  <int> 3, 4, 5, 7, 9, 12, 17, 18, 19, 21, 23, 24, 25, 26, 27, 28, 29,~
      $ carni   <dbl> 0.0700, 0.0108, 0.0256, 0.3250, 0.0125, 0.1570, 0.0175, 0.0445~
      $ herbi   <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, 0.42300, 0.09820, 0.11500,~
      $ insecti <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA~
      $ omni    <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA~

---

    Code
      purrr::map(list(df1, df2, df3, df4), summary)
    Output
      [[1]]
           .rowid        Petal.Length    Petal.Width     Sepal.Length  
       Min.   :  1.00   Min.   :1.000   Min.   :0.100   Min.   :4.300  
       1st Qu.: 38.25   1st Qu.:1.600   1st Qu.:0.300   1st Qu.:5.100  
       Median : 75.50   Median :4.350   Median :1.300   Median :5.800  
       Mean   : 75.50   Mean   :3.758   Mean   :1.199   Mean   :5.843  
       3rd Qu.:112.75   3rd Qu.:5.100   3rd Qu.:1.800   3rd Qu.:6.400  
       Max.   :150.00   Max.   :6.900   Max.   :2.500   Max.   :7.900  
        Sepal.Width   
       Min.   :2.000  
       1st Qu.:2.800  
       Median :3.000  
       Mean   :3.057  
       3rd Qu.:3.300  
       Max.   :4.400  
      
      [[2]]
           .rowid            0               1        
       Min.   : 1.00   Min.   :2.465   Min.   :1.513  
       1st Qu.: 8.75   1st Qu.:3.438   1st Qu.:1.935  
       Median :16.50   Median :3.520   Median :2.320  
       Mean   :16.50   Mean   :3.769   Mean   :2.411  
       3rd Qu.:24.25   3rd Qu.:3.842   3rd Qu.:2.780  
       Max.   :32.00   Max.   :5.424   Max.   :3.570  
                       NA's   :13      NA's   :19     
      
      [[3]]
           .rowid           HDHF             HDLF             LDHF       
       Min.   : 1.00   Min.   : 0.000   Min.   : 0.000   Min.   : 0.500  
       1st Qu.:24.75   1st Qu.: 6.000   1st Qu.: 4.375   1st Qu.: 6.000  
       Median :47.50   Median : 8.500   Median : 7.750   Median : 8.000  
       Mean   :47.57   Mean   : 7.824   Mean   : 6.676   Mean   : 7.352  
       3rd Qu.:70.25   3rd Qu.:10.000   3rd Qu.: 9.500   3rd Qu.: 9.500  
       Max.   :93.00   Max.   :10.000   Max.   :10.000   Max.   :10.000  
            LDLF       
       Min.   : 0.000  
       1st Qu.: 3.500  
       Median : 6.000  
       Mean   : 5.659  
       3rd Qu.: 7.500  
       Max.   :10.000  
      
      [[4]]
           .rowid          carni             herbi             insecti       
       Min.   : 3.00   Min.   :0.01080   Min.   :0.000400   Min.   :0.00025  
       1st Qu.:25.50   1st Qu.:0.01750   1st Qu.:0.005125   1st Qu.:0.00030  
       Median :46.00   Median :0.04450   Median :0.012285   Median :0.00120  
       Mean   :42.96   Mean   :0.07926   Mean   :0.621598   Mean   :0.02155  
       3rd Qu.:61.50   3rd Qu.:0.07000   3rd Qu.:0.236000   3rd Qu.:0.02500  
       Max.   :76.00   Max.   :0.32500   Max.   :5.712000   Max.   :0.08100  
                       NA's   :42        NA's   :31         NA's   :46       
            omni        
       Min.   :0.00014  
       1st Qu.:0.00260  
       Median :0.00660  
       Mean   :0.14573  
       3rd Qu.:0.17900  
       Max.   :1.32000  
       NA's   :34       
      

# long_to_wide_converter works - spread false

    Code
      purrr::walk(list(df1, df2, df3, df4), dplyr::glimpse)
    Output
      Rows: 600
      Columns: 3
      $ .rowid    <int> 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, ~
      $ condition <fct> Petal.Length, Petal.Width, Sepal.Length, Sepal.Width, Petal.~
      $ value     <dbl> 1.4, 0.2, 5.1, 3.5, 1.4, 0.2, 4.9, 3.0, 1.3, 0.2, 4.7, 3.2, ~
      Rows: 32
      Columns: 3
      $ .rowid <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, ~
      $ am     <fct> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, ~
      $ wt     <dbl> 3.215, 3.440, 3.460, 3.570, 3.190, 3.150, 3.440, 3.440, 4.070, ~
      Rows: 352
      Columns: 3
      $ .rowid    <int> 1, 1, 1, 1, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 6, 6, 6, 6, ~
      $ condition <fct> HDHF, HDLF, LDHF, LDLF, HDHF, HDLF, LDHF, LDLF, HDHF, HDLF, ~
      $ desire    <dbl> 10.0, 9.0, 6.0, 6.0, 10.0, 10.0, 10.0, 5.0, 9.0, 6.0, 9.0, 6~
      Rows: 51
      Columns: 3
      $ .rowid  <int> 3, 4, 5, 7, 9, 12, 17, 18, 19, 21, 23, 24, 25, 26, 27, 28, 29,~
      $ vore    <fct> carni, carni, carni, carni, carni, carni, carni, carni, carni,~
      $ brainwt <dbl> 0.07000, 0.01080, 0.02560, 0.32500, 0.01250, 0.15700, 0.01750,~

---

    Code
      purrr::map(list(df1, df2, df3, df4), summary)
    Output
      [[1]]
           .rowid             condition       value      
       Min.   :  1.0   Petal.Length:150   Min.   :0.100  
       1st Qu.: 38.0   Petal.Width :150   1st Qu.:1.700  
       Median : 75.5   Sepal.Length:150   Median :3.200  
       Mean   : 75.5   Sepal.Width :150   Mean   :3.465  
       3rd Qu.:113.0                      3rd Qu.:5.100  
       Max.   :150.0                      Max.   :7.900  
      
      [[2]]
           .rowid      am           wt       
       Min.   : 1.00   0:19   Min.   :1.513  
       1st Qu.: 8.75   1:13   1st Qu.:2.581  
       Median :16.50          Median :3.325  
       Mean   :16.50          Mean   :3.217  
       3rd Qu.:24.25          3rd Qu.:3.610  
       Max.   :32.00          Max.   :5.424  
      
      [[3]]
           .rowid      condition     desire      
       Min.   : 1.00   HDHF:88   Min.   : 0.000  
       1st Qu.:24.75   HDLF:88   1st Qu.: 5.000  
       Median :47.50   LDHF:88   Median : 7.500  
       Mean   :47.57   LDLF:88   Mean   : 6.878  
       3rd Qu.:70.25             3rd Qu.: 9.500  
       Max.   :93.00             Max.   :10.000  
      
      [[4]]
           .rowid           vore       brainwt       
       Min.   : 3.00   carni  : 9   Min.   :0.00014  
       1st Qu.:25.50   herbi  :20   1st Qu.:0.00375  
       Median :46.00   insecti: 5   Median :0.01550  
       Mean   :42.96   omni   :17   Mean   :0.30844  
       3rd Qu.:61.50                3rd Qu.:0.16300  
       Max.   :76.00                Max.   :5.71200  
      

