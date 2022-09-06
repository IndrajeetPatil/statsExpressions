# bayesian (between-subjects - anova)

    Code
      dim(df1)
    Output
      [1]  7 18

---

    Code
      df1[["expression"]][[1]]
    Output
      list(log[e] * (BF["01"]) == "1.54", widehat(italic(R^"2"))["Bayesian"]^"posterior" == 
          "0.00", CI["95%"]^HDI ~ "[" * "0.00", "0.09" * "]", italic("r")["Cauchy"]^"JZS" == 
          "0.71")

---

    Code
      dim(df2)
    Output
      [1]  6 18

---

    Code
      df2[["expression"]][[1]]
    Output
      list(log[e] * (BF["01"]) == "-65.10", widehat(italic(R^"2"))["Bayesian"]^"posterior" == 
          "0.61", CI["95%"]^HDI ~ "[" * "0.54", "0.67" * "]", italic("r")["Cauchy"]^"JZS" == 
          "0.71")

# bayesian (within-subjects - anova)

    Code
      dim(df1)
    Output
      [1]  7 20

---

    Code
      df1[["expression"]][[1]]
    Output
      list(log[e] * (BF["01"]) == "-1.96", widehat(italic(R^"2"))["Bayesian"]^"posterior" == 
          "0.89", CI["95%"]^HDI ~ "[" * "0.85", "0.92" * "]", italic("r")["Cauchy"]^"JZS" == 
          "0.88")

---

    Code
      dim(df2)
    Output
      [1]  8 20

---

    Code
      df2[["expression"]][[1]]
    Output
      list(log[e] * (BF["01"]) == "-21.04", widehat(italic(R^"2"))["Bayesian"]^"posterior" == 
          "0.53", CI["95%"]^HDI ~ "[" * "0.46", "0.59" * "]", italic("r")["Cauchy"]^"JZS" == 
          "0.71")
