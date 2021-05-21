# bayes factor (independent samples t-test)

    Code
      bf_expr$expression[[1]]
    Output
      paste("log"["e"] * "(BF"["01"] * ") = " * "-0.18" * ", ", widehat(italic(delta))["difference"]^"posterior" * 
          " = " * "-3.16" * ", ", "CI"["99%"]^"HDI" * " [" * "-8.13" * 
          ", " * "1.35" * "], ", italic("r")["Cauchy"]^"JZS" * " = " * 
          "0.71")

# bayes factor (paired t-test)

    Code
      bf_expr$expression[[1]]
    Output
      atop(displaystyle("bla"), expr = paste("log"["e"] * "(BF"["01"] * 
          ") = " * "-3.70" * ", ", widehat(italic(delta))["difference"]^"posterior" * 
          " = " * "-1.09" * ", ", "CI"["95%"]^"HDI" * " [" * "-1.70" * 
          ", " * "-0.49" * "], ", italic("r")["Cauchy"]^"JZS" * " = " * 
          "0.80"))

