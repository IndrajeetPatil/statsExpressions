# bayes factor (between-subjects - anova)

    Code
      as.character(results$expression[[1]])
    Output
      [1] "list(log[e] * (BF[\"01\"]) == \"1.92\", italic(R^\"2\")[Bayesian]^\"posterior\" == \"0.24\", CI[\"95%\"]^HDI ~ \"[\" * \"-0.06\", \"0.55\" * \"]\", italic(\"r\")[\"Cauchy\"]^\"JZS\" == \"0.88\")"

---

    Code
      as.character(results2$expression[[1]])
    Output
      [1] "list(log[e] * (BF[\"01\"]) == \"-65.10\", italic(R^\"2\")[Bayesian]^\"posterior\" == \"5.84\", CI[\"99%\"]^HDI ~ \"[\" * \"5.73\", \"5.95\" * \"]\", italic(\"r\")[\"Cauchy\"]^\"JZS\" == \"0.71\")"

# bayes factor (within-subjects - anova)

    Code
      as.character(results$expression[[1]])
    Output
      [1] "list(log[e] * (BF[\"01\"]) == \"-1.96\", italic(R^\"2\")[Bayesian]^\"posterior\" == \"5.51\", CI[\"95%\"]^HDI ~ \"[\" * \"5.40\", \"5.62\" * \"]\", italic(\"r\")[\"Cauchy\"]^\"JZS\" == \"0.88\")"

---

    Code
      as.character(results_na$expression[[1]])
    Output
      [1] "list(log[e] * (BF[\"01\"]) == \"-21.04\", italic(R^\"2\")[Bayesian]^\"posterior\" == \"6.88\", CI[\"95%\"]^HDI ~ \"[\" * \"6.43\", \"7.33\" * \"]\", italic(\"r\")[\"Cauchy\"]^\"JZS\" == \"0.71\")"

