# Bayesian oneway_anova() works: between-subjects with NAs

    Code
      dim(df)
    Output
      [1]  7 17

---

    Code
      colnames(df)
    Output
       [1] "term"               "pd"                 "prior.distribution"
       [4] "prior.location"     "prior.scale"        "bf10"              
       [7] "method"             "log_e_bf10"         "effectsize"        
      [10] "estimate"           "std.dev"            "conf.level"        
      [13] "conf.low"           "conf.high"          "conf.method"       
      [16] "n.obs"              "expression"        

---

    Code
      df[["expression"]][[1]]
    Output
      list(log[e] * (BF["01"]) == "1.54", widehat(italic(R^"2"))["Bayesian"]^"posterior" == 
          "0.00", CI["95%"]^HDI ~ "[" * "0.00", "0.09" * "]", italic("r")["Cauchy"]^"JZS" == 
          "0.71")

# Bayesian oneway_anova() works: between-subjects without NAs

    Code
      dim(df)
    Output
      [1]  6 17

---

    Code
      colnames(df)
    Output
       [1] "term"               "pd"                 "prior.distribution"
       [4] "prior.location"     "prior.scale"        "bf10"              
       [7] "method"             "log_e_bf10"         "effectsize"        
      [10] "estimate"           "std.dev"            "conf.level"        
      [13] "conf.low"           "conf.high"          "conf.method"       
      [16] "n.obs"              "expression"        

---

    Code
      df[["expression"]][[1]]
    Output
      list(log[e] * (BF["01"]) == "-65.10", widehat(italic(R^"2"))["Bayesian"]^"posterior" == 
          "0.61", CI["95%"]^HDI ~ "[" * "0.54", "0.67" * "]", italic("r")["Cauchy"]^"JZS" == 
          "0.71")

# Bayesian oneway_anova() works: within-subjects without NAs

    Code
      dim(df)
    Output
      [1]  7 19

---

    Code
      colnames(df)
    Output
       [1] "term"               "pd"                 "prior.distribution"
       [4] "prior.location"     "prior.scale"        "effect"            
       [7] "bf10"               "method"             "log_e_bf10"        
      [10] "effectsize"         "estimate"           "std.dev"           
      [13] "conf.level"         "conf.low"           "conf.high"         
      [16] "conf.method"        "component"          "n.obs"             
      [19] "expression"        

---

    Code
      df[["expression"]][[1]]
    Output
      list(log[e] * (BF["01"]) == "-1.96", widehat(italic(R^"2"))["Bayesian"]^"posterior" == 
          "0.89", CI["95%"]^HDI ~ "[" * "0.85", "0.92" * "]", italic("r")["Cauchy"]^"JZS" == 
          "0.88")

# Bayesian oneway_anova() works: within-subjects with NAs

    Code
      dim(df)
    Output
      [1]  8 19

---

    Code
      colnames(df)
    Output
       [1] "term"               "pd"                 "prior.distribution"
       [4] "prior.location"     "prior.scale"        "effect"            
       [7] "bf10"               "method"             "log_e_bf10"        
      [10] "effectsize"         "estimate"           "std.dev"           
      [13] "conf.level"         "conf.low"           "conf.high"         
      [16] "conf.method"        "component"          "n.obs"             
      [19] "expression"        

---

    Code
      df[["expression"]][[1]]
    Output
      list(log[e] * (BF["01"]) == "-21.04", widehat(italic(R^"2"))["Bayesian"]^"posterior" == 
          "0.53", CI["95%"]^HDI ~ "[" * "0.46", "0.59" * "]", italic("r")["Cauchy"]^"JZS" == 
          "0.71")

# tidy_model_parameters handles Bayesian ANOVA correctly

    Code
      dim(result)
    Output
      [1]  6 15

