# between-subjects - data with and without NAs

    Code
      select(df1, -expression)
    Output
      # A tibble: 1 x 14
        parameter1 parameter2 statistic df.error      p.value
        <chr>      <chr>          <dbl>    <int>        <dbl>
      1 length     genre           51.4        8 0.0000000217
        method                       effectsize      estimate conf.level conf.low
        <chr>                        <chr>              <dbl>      <dbl>    <dbl>
      1 Kruskal-Wallis rank sum test Epsilon2 (rank)    0.328       0.95    0.258
        conf.high conf.method          conf.iterations n.obs
            <dbl> <chr>                          <int> <int>
      1         1 percentile bootstrap             100   158

---

    Code
      as.character(df1$expression[[1]])
    Output
      [1] "list(chi[\"Kruskal-Wallis\"]^2 * \"(\" * 8 * \")\" == \"51.42672\", italic(p) == \"2.17135e-08\", widehat(epsilon)[\"ordinal\"]^2 == \"0.32756\", CI[\"95%\"] ~ \"[\" * \"0.25829\", \"1.00000\" * \"]\", italic(\"n\")[\"obs\"] == \"158\")"

---

    Code
      select(df2, -expression)
    Output
      # A tibble: 1 x 14
        parameter1  parameter2 statistic df.error p.value method                      
        <chr>       <chr>          <dbl>    <int>   <dbl> <chr>                       
      1 sleep_cycle vore            5.24        3   0.155 Kruskal-Wallis rank sum test
        effectsize      estimate conf.level conf.low conf.high conf.method         
        <chr>              <dbl>      <dbl>    <dbl>     <dbl> <chr>               
      1 Epsilon2 (rank)    0.175       0.99   0.0445         1 percentile bootstrap
        conf.iterations n.obs
                  <int> <int>
      1             100    31

---

    Code
      as.character(df2$expression[[1]])
    Output
      [1] "list(chi[\"Kruskal-Wallis\"]^2 * \"(\" * 3 * \")\" == \"5.240\", italic(p) == \"0.155\", widehat(epsilon)[\"ordinal\"]^2 == \"0.175\", CI[\"99%\"] ~ \"[\" * \"0.045\", \"1.000\" * \"]\", italic(\"n\")[\"obs\"] == \"31\")"

# within-subjects - data with and without NAs

    Code
      as.character(df1$expression[[1]])
    Output
      [1] "list(chi[\"Friedman\"]^2 * \"(\" * 3 * \")\" == \"55.8338\", italic(p) == \"4.5584e-12\", widehat(italic(\"W\"))[\"Kendall\"] == \"0.1750\", CI[\"99%\"] ~ \"[\" * \"0.1142\", \"1.0000\" * \"]\", italic(\"n\")[\"pairs\"] == \"88\")"

---

    Code
      as.character(df2$expression[[1]])
    Output
      [1] "list(chi[\"Friedman\"]^2 * \"(\" * 3 * \")\" == \"410.000\", italic(p) == \"1.510e-88\", widehat(italic(\"W\"))[\"Kendall\"] == \"0.911\", CI[\"90%\"] ~ \"[\" * \"0.906\", \"1.000\" * \"]\", italic(\"n\")[\"pairs\"] == \"150\")"

