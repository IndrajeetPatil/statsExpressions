# format_num works

    Code
      list(format_num(1.234e-05), format_num(1.234e-05, TRUE), format_num(1.234e-05,
        k = 8, TRUE), format_num(0.001, k = 4, TRUE), format_num(1e-04, k = 4, TRUE),
      format_num(1e-04, k = 3, TRUE), format_num(1e-04, k = 3, FALSE), format_num(
        0.00333, k = 1, TRUE))
    Output
      [[1]]
      [1] "0.00"
      
      [[2]]
      [1] "0.0"
      
      [[3]]
      [1] "1.234e-05"
      
      [[4]]
      [1] "0.0010"
      
      [[5]]
      [1] "1e-04"
      
      [[6]]
      [1] "1e-04"
      
      [[7]]
      [1] "0.000"
      
      [[8]]
      [1] "0.003"
      

---

    Code
      list(format_num(123), format_num(1234), format_num(1234, k = 3), format_num(
        123445678, k = 4), format_num(123445678, k = 4), format_num(123445678, k = 3),
      format_num(123445678, k = 3), format_num(123445678, k = 1))
    Output
      [[1]]
      [1] "123.00"
      
      [[2]]
      [1] "1.2e+03"
      
      [[3]]
      [1] "1.2e+03"
      
      [[4]]
      [1] "1.2e+08"
      
      [[5]]
      [1] "1.2e+08"
      
      [[6]]
      [1] "1.2e+08"
      
      [[7]]
      [1] "1.2e+08"
      
      [[8]]
      [1] "1.2e+08"
      

