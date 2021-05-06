# bayes factor (between-subjects - anova)

    Code
      results$expression[[1]]
    Output
      paste("log"["e"] * "(BF"["01"] * ") = " * "1.92" * ", ", widehat(italic(R^"2"))["Bayesian"]^"posterior" * 
          " = " * "0.00" * ", ", "CI"["95%"]^"HDI" * " [" * "0.00" * 
          ", " * "0.08" * "], ", italic("r")["Cauchy"]^"JZS" * " = " * 
          "0.88")

---

    Code
      results2$expression[[1]]
    Output
      paste("log"["e"] * "(BF"["01"] * ") = " * "-65.10" * ", ", widehat(italic(R^"2"))["Bayesian"]^"posterior" * 
          " = " * "0.61" * ", ", "CI"["99%"]^"HDI" * " [" * "0.51" * 
          ", " * "0.68" * "], ", italic("r")["Cauchy"]^"JZS" * " = " * 
          "0.71")

# bayes factor (within-subjects - anova)

    Code
      results$expression[[1]]
    Output
      paste("log"["e"] * "(BF"["01"] * ") = " * "-1.96" * ", ", widehat(italic(R^"2"))["Bayesian"]^"posterior" * 
          " = " * "0.89" * ", ", "CI"["95%"]^"HDI" * " [" * "0.85" * 
          ", " * "0.92" * "], ", italic("r")["Cauchy"]^"JZS" * " = " * 
          "0.88")

---

    Code
      results_na$expression[[1]]
    Output
      paste("log"["e"] * "(BF"["01"] * ") = " * "-21.04" * ", ", widehat(italic(R^"2"))["Bayesian"]^"posterior" * 
          " = " * "0.53" * ", ", "CI"["95%"]^"HDI" * " [" * "0.46" * 
          ", " * "0.59" * "], ", italic("r")["Cauchy"]^"JZS" * " = " * 
          "0.71")

