# bayes factor (independent samples t-test)

    Code
      df$expression
    Output
      [[1]]
      list(log[e] * (BF["01"]) == "-0.18", widehat(delta)["difference"]^"posterior" == 
          "3.16", CI["99%"]^HDI ~ "[" * "-1.43", "8.05" * "]", italic("r")["Cauchy"]^"JZS" == 
          "0.71")
      

# bayes factor (paired t-test)

    Code
      df$expression
    Output
      [[1]]
      list(log[e] * (BF["01"]) == "-3.70", widehat(delta)["difference"]^"posterior" == 
          "1.09", CI["95%"]^HDI ~ "[" * "0.49", "1.70" * "]", italic("r")["Cauchy"]^"JZS" == 
          "0.80")
      

