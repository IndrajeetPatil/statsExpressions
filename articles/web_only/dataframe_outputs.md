# Data frame outputs

This vignette can be cited as:

    To cite package 'statsExpressions' in publications use:

      Patil, I., (2021). statsExpressions: R Package for Tidy Dataframes
      and Expressions with Statistical Details. Journal of Open Source
      Software, 6(61), 3236, https://doi.org/10.21105/joss.03236

    A BibTeX entry for LaTeX users is

      @Article{,
        doi = {10.21105/joss.03236},
        year = {2021},
        publisher = {{The Open Journal}},
        volume = {6},
        number = {61},
        pages = {3236},
        author = {Indrajeet Patil},
        title = {{statsExpressions: {R} Package for Tidy Dataframes and Expressions with Statistical Details}},
        journal = {{Journal of Open Source Software}},
      }

## One-sample tests

``` r

# for reproducibility
set.seed(123)

# ----------------------- parametric -----------------------

one_sample_test(mtcars, wt, test.value = 3)
#> # A tibble: 1 × 15
#>      mu statistic df.error p.value method            alternative effectsize
#>   <dbl>     <dbl>    <dbl>   <dbl> <chr>             <chr>       <chr>     
#> 1     3      1.26       31   0.218 One Sample t-test two.sided   Hedges' g 
#>   estimate conf.level conf.low conf.high conf.method conf.distribution n.obs
#>      <dbl>      <dbl>    <dbl>     <dbl> <chr>       <chr>             <int>
#> 1    0.217       0.95   -0.127     0.557 ncp         t                    32
#>   expression
#>   <list>    
#> 1 <language>

# ----------------------- non-parametric -------------------

one_sample_test(mtcars, wt, test.value = 3, type = "nonparametric")
#> # A tibble: 1 × 12
#>   statistic p.value method                    alternative effectsize       
#>       <dbl>   <dbl> <chr>                     <chr>       <chr>            
#> 1       319   0.308 Wilcoxon signed rank test two.sided   r (rank biserial)
#>   estimate conf.level conf.low conf.high conf.method n.obs expression
#>      <dbl>      <dbl>    <dbl>     <dbl> <chr>       <int> <list>    
#> 1    0.208       0.95   -0.184     0.543 normal         32 <language>

# ----------------------- robust ---------------------------

one_sample_test(mtcars, wt, test.value = 3, type = "robust")
#> # A tibble: 1 × 10
#>   statistic p.value n.obs method                                 effectsize  
#>       <dbl>   <dbl> <int> <chr>                                  <chr>       
#> 1      1.18   0.275    32 Bootstrap-t method for one-sample test Trimmed mean
#>   estimate conf.level conf.low conf.high expression
#>      <dbl>      <dbl>    <dbl>     <dbl> <list>    
#> 1     3.20       0.95     2.85      3.54 <language>

# ----------------------- Bayesian -------------------------

one_sample_test(mtcars, wt, test.value = 3, type = "bayes")
#> # A tibble: 1 × 16
#>   term       effectsize      estimate conf.level conf.low conf.high    pd
#>   <chr>      <chr>              <dbl>      <dbl>    <dbl>     <dbl> <dbl>
#> 1 Difference Bayesian t-test    0.195       0.95   -0.165     0.555  0.86
#>   prior.distribution prior.location prior.scale  bf10 method         
#>   <chr>                       <dbl>       <dbl> <dbl> <chr>          
#> 1 cauchy                          0       0.707 0.387 Bayesian t-test
#>   conf.method log_e_bf10 n.obs expression
#>   <chr>            <dbl> <int> <list>    
#> 1 ETI             -0.950    32 <language>
```

## Two-sample tests

### within-subjects design

``` r

# ----------------------- within-subjects -------------------------------------

# data
df <- dplyr::filter(bugs_long, condition %in% c("LDLF", "LDHF"))

# for reproducibility
set.seed(123)

# ----------------------- parametric ---------------------------------------

two_sample_test(df, condition, desire, subject.id = subject, paired = TRUE, type = "parametric")
#> # A tibble: 1 × 16
#>   term   group     statistic df.error       p.value method        alternative
#>   <chr>  <chr>         <dbl>    <dbl>         <dbl> <chr>         <chr>      
#> 1 desire condition      6.65       90 0.00000000222 Paired t-test two.sided  
#>   effectsize estimate conf.level conf.low conf.high conf.method
#>   <chr>         <dbl>      <dbl>    <dbl>     <dbl> <chr>      
#> 1 Hedges' g     0.691       0.95    0.462     0.917 ncp        
#>   conf.distribution n.obs expression
#>   <chr>             <int> <list>    
#> 1 t                    91 <language>

# ----------------------- non-parametric -----------------------------------

two_sample_test(df, condition, desire, subject.id = subject, paired = TRUE, type = "nonparametric")
#> # A tibble: 1 × 14
#>   parameter1 parameter2 statistic      p.value method                   
#>   <chr>      <chr>          <dbl>        <dbl> <chr>                    
#> 1 desire     condition      2250. 0.0000000241 Wilcoxon signed rank test
#>   alternative effectsize        estimate conf.level conf.low conf.high
#>   <chr>       <chr>                <dbl>      <dbl>    <dbl>     <dbl>
#> 1 two.sided   r (rank biserial)    0.761       0.95    0.642     0.844
#>   conf.method n.obs expression
#>   <chr>       <int> <list>    
#> 1 normal         91 <language>

# ----------------------- robust --------------------------------------------

two_sample_test(df, condition, desire, subject.id = subject, paired = TRUE, type = "robust")
#> # A tibble: 1 × 15
#>   statistic df.error      p.value
#>       <dbl>    <dbl>        <dbl>
#> 1      6.46       54 0.0000000313
#>   method                                            
#>   <chr>                                             
#> 1 Yuen's test on trimmed means for dependent samples
#>   effectsize                                              estimate conf.level
#>   <chr>                                                      <dbl>      <dbl>
#> 1 Algina-Keselman-Penfield robust standardized difference    0.533       0.95
#>   conf.low conf.high    mu small medium large n.obs expression
#>      <dbl>     <dbl> <dbl> <dbl>  <dbl> <dbl> <int> <list>    
#> 1    0.369     0.707     0   0.1    0.3   0.5    91 <language>

# ----------------------- Bayesian ---------------------------------------

two_sample_test(df, condition, desire, subject.id = subject, paired = TRUE, type = "bayes")
#> # A tibble: 1 × 16
#>   term       effectsize      estimate conf.level conf.low conf.high    pd
#>   <chr>      <chr>              <dbl>      <dbl>    <dbl>     <dbl> <dbl>
#> 1 Difference Bayesian t-test     1.63       0.95     1.13      2.11     1
#>   prior.distribution prior.location prior.scale     bf10 method         
#>   <chr>                       <dbl>       <dbl>    <dbl> <chr>          
#> 1 cauchy                          0       0.707 4762370. Bayesian t-test
#>   conf.method log_e_bf10 n.obs expression
#>   <chr>            <dbl> <int> <list>    
#> 1 ETI               15.4    91 <language>
```

### between-subjects design

``` r

# ----------------------- between-subjects -------------------------------------

# for reproducibility
set.seed(123)

# ----------------------- parametric ---------------------------------------

# unequal variance
two_sample_test(ToothGrowth, supp, len, type = "parametric")
#> # A tibble: 1 × 18
#>   parameter1 parameter2 mean.parameter1 mean.parameter2 statistic df.error
#>   <chr>      <chr>                <dbl>           <dbl>     <dbl>    <dbl>
#> 1 len        supp                  20.7            17.0      1.92     55.3
#>   p.value method                  alternative effectsize estimate conf.level
#>     <dbl> <chr>                   <chr>       <chr>         <dbl>      <dbl>
#> 1  0.0606 Welch Two Sample t-test two.sided   Hedges' g     0.488       0.95
#>   conf.low conf.high conf.method conf.distribution n.obs expression
#>      <dbl>     <dbl> <chr>       <chr>             <int> <list>    
#> 1  -0.0217     0.993 ncp         t                    60 <language>

# equal variance
two_sample_test(ToothGrowth, supp, len, type = "parametric", var.equal = TRUE)
#> # A tibble: 1 × 18
#>   parameter1 parameter2 mean.parameter1 mean.parameter2 statistic df.error
#>   <chr>      <chr>                <dbl>           <dbl>     <dbl>    <dbl>
#> 1 len        supp                  20.7            17.0      1.92       58
#>   p.value method            alternative effectsize estimate conf.level conf.low
#>     <dbl> <chr>             <chr>       <chr>         <dbl>      <dbl>    <dbl>
#> 1  0.0604 Two Sample t-test two.sided   Hedges' g     0.488       0.95  -0.0217
#>   conf.high conf.method conf.distribution n.obs expression
#>       <dbl> <chr>       <chr>             <int> <list>    
#> 1     0.993 ncp         t                    60 <language>

# ----------------------- non-parametric -----------------------------------

two_sample_test(ToothGrowth, supp, len, type = "nonparametric")
#> # A tibble: 1 × 14
#>   parameter1 parameter2 statistic p.value method                 alternative
#>   <chr>      <chr>          <dbl>   <dbl> <chr>                  <chr>      
#> 1 len        supp            576.  0.0645 Wilcoxon rank sum test two.sided  
#>   effectsize        estimate conf.level conf.low conf.high conf.method n.obs
#>   <chr>                <dbl>      <dbl>    <dbl>     <dbl> <chr>       <int>
#> 1 r (rank biserial)    0.279       0.95 -0.00812     0.523 normal         60
#>   expression
#>   <list>    
#> 1 <language>

# ----------------------- robust --------------------------------------------

two_sample_test(ToothGrowth, supp, len, type = "robust")
#> # A tibble: 1 × 11
#>   statistic df.error p.value
#>       <dbl>    <dbl>   <dbl>
#> 1      2.29     33.5  0.0286
#>   method                                              
#>   <chr>                                               
#> 1 Yuen's test on trimmed means for independent samples
#>   effectsize                                              estimate conf.level
#>   <chr>                                                      <dbl>      <dbl>
#> 1 Algina-Keselman-Penfield robust standardized difference    0.683       0.95
#>   conf.low conf.high n.obs expression
#>      <dbl>     <dbl> <int> <list>    
#> 1 -0.00736      2.36    60 <language>

# ----------------------- Bayesian ---------------------------------------

two_sample_test(ToothGrowth, supp, len, type = "bayes")
#> # A tibble: 1 × 16
#>   term       effectsize      estimate conf.level conf.low conf.high    pd
#>   <chr>      <chr>              <dbl>      <dbl>    <dbl>     <dbl> <dbl>
#> 1 Difference Bayesian t-test     3.16       0.95   -0.338      6.78 0.961
#>   prior.distribution prior.location prior.scale  bf10 method         
#>   <chr>                       <dbl>       <dbl> <dbl> <chr>          
#> 1 cauchy                          0       0.707  1.20 Bayesian t-test
#>   conf.method log_e_bf10 n.obs expression
#>   <chr>            <dbl> <int> <list>    
#> 1 ETI              0.181    60 <language>
```

## One-way ANOVAs

### within-subjects design

``` r

suppressPackageStartupMessages(library(afex))

# ----------------------- parametric ---------------------------------------

set.seed(123)
oneway_anova(
  data       = bugs_long,
  x          = condition,
  y          = desire,
  paired     = TRUE,
  subject.id = subject,
  type       = "p"
)
#> # A tibble: 1 × 18
#>   term      sumsq sum.squares.error    df df.error meansq statistic  p.value
#>   <chr>     <dbl>             <dbl> <dbl>    <dbl>  <dbl>     <dbl>    <dbl>
#> 1 condition  233.              984.  2.63     229.   4.30      20.6 8.27e-11
#>   method                                              effectsize       estimate
#>   <chr>                                               <chr>               <dbl>
#> 1 ANOVA estimation for factorial designs using 'afex' Omega2 (partial)   0.0783
#>   conf.level conf.low conf.high conf.method conf.distribution n.obs expression
#>        <dbl>    <dbl>     <dbl> <chr>       <chr>             <int> <list>    
#> 1       0.95   0.0280         1 ncp         F                    88 <language>

# ----------------------- non-parametric -----------------------------------

set.seed(123)
oneway_anova(
  data       = bugs_long,
  x          = condition,
  y          = desire,
  paired     = TRUE,
  subject.id = subject,
  type       = "np"
)
#> # A tibble: 1 × 15
#>   parameter1 parameter2 statistic df.error  p.value method                
#>   <chr>      <chr>          <dbl>    <dbl>    <dbl> <chr>                 
#> 1 desire     condition       55.8        3 4.56e-12 Friedman rank sum test
#>   effectsize  estimate conf.level conf.low conf.high conf.method         
#>   <chr>          <dbl>      <dbl>    <dbl>     <dbl> <chr>               
#> 1 Kendall's W    0.211       0.95    0.148         1 percentile bootstrap
#>   conf.iterations n.obs expression
#>             <int> <int> <list>    
#> 1             100    88 <language>

# ----------------------- robust --------------------------------------------

set.seed(123)
oneway_anova(
  data       = bugs_long,
  x          = condition,
  y          = desire,
  paired     = TRUE,
  subject.id = subject,
  type       = "r"
)
#> # A tibble: 1 × 12
#>   statistic    df df.error  p.value
#>       <dbl> <dbl>    <dbl>    <dbl>
#> 1      21.0  2.73     145. 1.15e-10
#>   method                                                             
#>   <chr>                                                              
#> 1 A heteroscedastic one-way repeated measures ANOVA for trimmed means
#>   effectsize                                                      estimate
#>   <chr>                                                              <dbl>
#> 1 Algina-Keselman-Penfield robust standardized difference average    0.664
#>   conf.level conf.low conf.high n.obs expression
#>        <dbl>    <dbl>     <dbl> <int> <list>    
#> 1       0.95    0.466     0.971    88 <language>

# ----------------------- Bayesian ---------------------------------------

set.seed(123)
oneway_anova(
  data       = bugs_long,
  x          = condition,
  y          = desire,
  paired     = TRUE,
  subject.id = subject,
  type       = "bayes"
)
#> # A tibble: 8 × 19
#>   term              pd prior.distribution prior.location prior.scale effect
#>   <chr>          <dbl> <chr>                       <dbl>       <dbl> <chr> 
#> 1 mu             1     cauchy                          0       0.707 fixed 
#> 2 condition-HDHF 1     cauchy                          0       0.707 fixed 
#> 3 condition-HDLF 0.862 cauchy                          0       0.707 fixed 
#> 4 condition-LDHF 0.995 cauchy                          0       0.707 fixed 
#> 5 condition-LDLF 1     cauchy                          0       0.707 fixed 
#> 6 sig2           1     cauchy                          0       1     fixed 
#> 7 g_condition    1     cauchy                          0       1     fixed 
#> 8 g_.rowid       1     cauchy                          0       1     fixed 
#>          bf10 method                          log_e_bf10 effectsize        
#>         <dbl> <chr>                                <dbl> <chr>             
#> 1 1372773377. Bayes factors for linear models       21.0 Bayesian R-squared
#> 2 1372773377. Bayes factors for linear models       21.0 Bayesian R-squared
#> 3 1372773377. Bayes factors for linear models       21.0 Bayesian R-squared
#> 4 1372773377. Bayes factors for linear models       21.0 Bayesian R-squared
#> 5 1372773377. Bayes factors for linear models       21.0 Bayesian R-squared
#> 6 1372773377. Bayes factors for linear models       21.0 Bayesian R-squared
#> 7 1372773377. Bayes factors for linear models       21.0 Bayesian R-squared
#> 8 1372773377. Bayes factors for linear models       21.0 Bayesian R-squared
#>   estimate std.dev conf.level conf.low conf.high conf.method component   n.obs
#>      <dbl>   <dbl>      <dbl>    <dbl>     <dbl> <chr>       <chr>       <int>
#> 1    0.529  0.0333       0.95    0.461     0.587 HDI         conditional    88
#> 2    0.529  0.0333       0.95    0.461     0.587 HDI         conditional    88
#> 3    0.529  0.0333       0.95    0.461     0.587 HDI         conditional    88
#> 4    0.529  0.0333       0.95    0.461     0.587 HDI         conditional    88
#> 5    0.529  0.0333       0.95    0.461     0.587 HDI         conditional    88
#> 6    0.529  0.0333       0.95    0.461     0.587 HDI         conditional    88
#> 7    0.529  0.0333       0.95    0.461     0.587 HDI         conditional    88
#> 8    0.529  0.0333       0.95    0.461     0.587 HDI         conditional    88
#>   expression
#>   <list>    
#> 1 <language>
#> 2 <language>
#> 3 <language>
#> 4 <language>
#> 5 <language>
#> 6 <language>
#> 7 <language>
#> 8 <language>
```

### between-subjects design

``` r

# ----------------------- parametric ---------------------------------------

# unequal variance
set.seed(123)
oneway_anova(
  data      = iris,
  x         = Species,
  y         = Sepal.Length,
  type      = "p"
)
#> # A tibble: 1 × 14
#>   statistic    df df.error  p.value
#>       <dbl> <dbl>    <dbl>    <dbl>
#> 1      139.     2     92.2 1.51e-28
#>   method                                                   effectsize estimate
#>   <chr>                                                    <chr>         <dbl>
#> 1 One-way analysis of means (not assuming equal variances) Omega2        0.743
#>   conf.level conf.low conf.high conf.method conf.distribution n.obs expression
#>        <dbl>    <dbl>     <dbl> <chr>       <chr>             <int> <list>    
#> 1       0.95    0.671         1 ncp         F                   150 <language>

# equal variance
set.seed(123)
oneway_anova(
  data      = iris,
  x         = Species,
  y         = Sepal.Length,
  var.equal = TRUE,
  type      = "p"
)
#> # A tibble: 1 × 14
#>   statistic    df df.error  p.value method                    effectsize
#>       <dbl> <dbl>    <dbl>    <dbl> <chr>                     <chr>     
#> 1      119.     2      147 1.67e-31 One-way analysis of means Omega2    
#>   estimate conf.level conf.low conf.high conf.method conf.distribution n.obs
#>      <dbl>      <dbl>    <dbl>     <dbl> <chr>       <chr>             <int>
#> 1    0.612       0.95    0.534         1 ncp         F                   150
#>   expression
#>   <list>    
#> 1 <language>

# ----------------------- non-parametric -----------------------------------

set.seed(123)
oneway_anova(
  data      = iris,
  x         = Species,
  y         = Sepal.Length,
  type      = "np"
)
#> # A tibble: 1 × 15
#>   parameter1   parameter2 statistic df.error  p.value
#>   <chr>        <chr>          <dbl>    <int>    <dbl>
#> 1 Sepal.Length Species         96.9        2 8.92e-22
#>   method                       effectsize      estimate conf.level conf.low
#>   <chr>                        <chr>              <dbl>      <dbl>    <dbl>
#> 1 Kruskal-Wallis rank sum test Epsilon2 (rank)    0.651       0.95    0.595
#>   conf.high conf.method          conf.iterations n.obs expression
#>       <dbl> <chr>                          <int> <int> <list>    
#> 1         1 percentile bootstrap             100   150 <language>

# ----------------------- robust --------------------------------------------

set.seed(123)
oneway_anova(
  data      = iris,
  x         = Species,
  y         = Sepal.Length,
  type      = "r"
)
#> # A tibble: 1 × 12
#>   statistic    df df.error   p.value
#>       <dbl> <dbl>    <dbl>     <dbl>
#> 1      112.     2     53.8 2.23e-308
#>   method                                           
#>   <chr>                                            
#> 1 A heteroscedastic one-way ANOVA for trimmed means
#>   effectsize                         estimate conf.level conf.low conf.high
#>   <chr>                                 <dbl>      <dbl>    <dbl>     <dbl>
#> 1 Explanatory measure of effect size    0.846       0.95    0.759     0.933
#>   n.obs expression
#>   <int> <list>    
#> 1   150 <language>

# ----------------------- Bayesian ---------------------------------------

set.seed(123)
oneway_anova(
  data      = iris,
  x         = Species,
  y         = Sepal.Length,
  type      = "bayes"
)
#> # A tibble: 6 × 17
#>   term                  pd prior.distribution prior.location prior.scale    bf10
#>   <chr>              <dbl> <chr>                       <dbl>       <dbl>   <dbl>
#> 1 mu                 1     cauchy                          0       0.707 1.87e28
#> 2 Species-setosa     1     cauchy                          0       0.707 1.87e28
#> 3 Species-versicolor 0.936 cauchy                          0       0.707 1.87e28
#> 4 Species-virginica  1     cauchy                          0       0.707 1.87e28
#> 5 sig2               1     cauchy                          0       0.707 1.87e28
#> 6 g_Species          1     cauchy                          0       0.707 1.87e28
#>   method                          log_e_bf10 effectsize         estimate std.dev
#>   <chr>                                <dbl> <chr>                 <dbl>   <dbl>
#> 1 Bayes factors for linear models       65.1 Bayesian R-squared    0.612  0.0311
#> 2 Bayes factors for linear models       65.1 Bayesian R-squared    0.612  0.0311
#> 3 Bayes factors for linear models       65.1 Bayesian R-squared    0.612  0.0311
#> 4 Bayes factors for linear models       65.1 Bayesian R-squared    0.612  0.0311
#> 5 Bayes factors for linear models       65.1 Bayesian R-squared    0.612  0.0311
#> 6 Bayes factors for linear models       65.1 Bayesian R-squared    0.612  0.0311
#>   conf.level conf.low conf.high conf.method n.obs expression
#>        <dbl>    <dbl>     <dbl> <chr>       <int> <list>    
#> 1       0.95    0.544     0.667 HDI           150 <language>
#> 2       0.95    0.544     0.667 HDI           150 <language>
#> 3       0.95    0.544     0.667 HDI           150 <language>
#> 4       0.95    0.544     0.667 HDI           150 <language>
#> 5       0.95    0.544     0.667 HDI           150 <language>
#> 6       0.95    0.544     0.667 HDI           150 <language>
```

## Contingency table analyses

``` r

if (identical(Sys.getenv("NOT_CRAN"), "true")) {
  #### -------------------- association test ------------------------ ####

  # ------------------------ frequentist ---------------------------------

  # unpaired

  set.seed(123)
  contingency_table(
    data   = mtcars,
    x      = am,
    y      = vs,
    paired = FALSE
  )

  # paired

  paired_data <- tibble(
    response_before = structure(c(1L, 2L, 1L, 2L), levels = c("no", "yes"), class = "factor"),
    response_after = structure(c(1L, 1L, 2L, 2L), levels = c("no", "yes"), class = "factor"),
    Freq = c(65L, 25L, 5L, 5L)
  )

  set.seed(123)
  contingency_table(
    data   = paired_data,
    x      = response_before,
    y      = response_after,
    paired = TRUE,
    counts = Freq
  )

  # ------------------------ Bayesian -------------------------------------

  # unpaired

  set.seed(123)
  contingency_table(
    data = mtcars,
    x = am,
    y = vs,
    paired = FALSE,
    type = "bayes"
  )

  # paired

  set.seed(123)
  contingency_table(
    data = paired_data,
    x = response_before,
    y = response_after,
    paired = TRUE,
    counts = Freq,
    type = "bayes"
  )

  #### -------------------- goodness-of-fit test -------------------- ####

  # ------------------------ frequentist ---------------------------------

  set.seed(123)
  contingency_table(
    data   = as.data.frame(HairEyeColor),
    x      = Eye,
    counts = Freq
  )

  # ------------------------ Bayesian -------------------------------------

  set.seed(123)
  contingency_table(
    data   = as.data.frame(HairEyeColor),
    x      = Eye,
    counts = Freq,
    ratio  = c(0.2, 0.2, 0.3, 0.3),
    type   = "bayes"
  )
}
#> # A tibble: 1 × 4
#>      bf10 prior.scale method                                      expression
#>     <dbl>       <dbl> <chr>                                       <list>    
#> 1 4.17e55           1 Bayesian one-way contingency table analysis <language>
```

## Correlation analyses

``` r

# for reproducibility
set.seed(123)

# ----------------------- parametric -----------------------

corr_test(mtcars, wt, mpg, type = "parametric")
#> # A tibble: 1 × 14
#>   parameter1 parameter2 effectsize          estimate conf.level conf.low
#>   <chr>      <chr>      <chr>                  <dbl>      <dbl>    <dbl>
#> 1 wt         mpg        Pearson correlation   -0.868       0.95   -0.934
#>   conf.high statistic df.error  p.value method              n.obs conf.method
#>       <dbl>     <dbl>    <int>    <dbl> <chr>               <int> <chr>      
#> 1    -0.744     -9.56       30 1.29e-10 Pearson correlation    32 normal     
#>   expression
#>   <list>    
#> 1 <language>

# ----------------------- non-parametric -------------------

corr_test(mtcars, wt, mpg, type = "nonparametric")
#> # A tibble: 1 × 13
#>   parameter1 parameter2 effectsize           estimate conf.level conf.low
#>   <chr>      <chr>      <chr>                   <dbl>      <dbl>    <dbl>
#> 1 wt         mpg        Spearman correlation   -0.886       0.95   -0.945
#>   conf.high statistic  p.value method               n.obs conf.method expression
#>       <dbl>     <dbl>    <dbl> <chr>                <int> <chr>       <list>    
#> 1    -0.774    10292. 1.49e-11 Spearman correlation    32 normal      <language>

# ----------------------- robust ---------------------------

corr_test(mtcars, wt, mpg, type = "robust")
#> # A tibble: 1 × 14
#>   parameter1 parameter2 effectsize                     estimate conf.level
#>   <chr>      <chr>      <chr>                             <dbl>      <dbl>
#> 1 wt         mpg        Winsorized Pearson correlation   -0.864       0.95
#>   conf.low conf.high statistic df.error  p.value method                        
#>      <dbl>     <dbl>     <dbl>    <int>    <dbl> <chr>                         
#> 1   -0.932    -0.738     -9.41       30 1.84e-10 Winsorized Pearson correlation
#>   n.obs conf.method expression
#>   <int> <chr>       <list>    
#> 1    32 normal      <language>

# ----------------------- Bayesian -------------------------

corr_test(mtcars, wt, mpg, type = "bayes")
#> # A tibble: 1 × 17
#>   parameter1 parameter2 effectsize                   estimate conf.level
#>   <chr>      <chr>      <chr>                           <dbl>      <dbl>
#> 1 wt         mpg        Bayesian Pearson correlation   -0.843       0.95
#>   conf.low conf.high    pd rope.percentage prior.distribution prior.location
#>      <dbl>     <dbl> <dbl>           <dbl> <chr>                       <dbl>
#> 1   -0.934    -0.734     1               0 beta                         1.41
#>   prior.scale      bf10 method                       n.obs conf.method
#>         <dbl>     <dbl> <chr>                        <int> <chr>      
#> 1        1.41 56223033. Bayesian Pearson correlation    32 HDI        
#>   expression
#>   <list>    
#> 1 <language>
```

## Meta-analysis

``` r

library(metaplus)

# renaming columns to `{statsExpressions}` conventions
df <- dplyr::rename(mag, estimate = yi, std.error = sei)

# ----------------------- parametric ---------------------------------------

set.seed(123)
meta_analysis(df, type = "parametric")
#> # A tibble: 1 × 14
#>   term    effectsize                     estimate std.error conf.level conf.low
#>   <chr>   <chr>                             <dbl>     <dbl>      <dbl>    <dbl>
#> 1 Overall meta-analytic summary estimate   -0.767     0.212       0.95    -1.18
#>   conf.high statistic  p.value weight method                        conf.method
#>       <dbl>     <dbl>    <dbl>  <dbl> <chr>                         <chr>      
#> 1    -0.351     -3.62 0.000295     NA Meta-analysis using 'metafor' Wald       
#>   n.obs expression
#>   <int> <list>    
#> 1    16 <language>

# ----------------------- robust --------------------------------------------

set.seed(123)
meta_analysis(df, type = "robust")
#> # A tibble: 1 × 14
#>   term    effectsize                     estimate std.error conf.low conf.high
#>   <chr>   <chr>                             <dbl>     <dbl>    <dbl>     <dbl>
#> 1 Overall meta-analytic summary estimate   -0.746     0.233    -1.26    -0.344
#>   statistic  p.value weight conf.level method                               
#>       <dbl>    <dbl>  <dbl>      <dbl> <chr>                                
#> 1     -3.20 0.000501     NA       0.95 Robust meta-analysis using 'metaplus'
#>   conf.method n.obs expression
#>   <chr>       <int> <list>    
#> 1 Wald           16 <language>

# ----------------------- Bayesian ---------------------------------------

# suppress warnings about divergent transitions after warmup
set.seed(123)
suppressWarnings(meta_analysis(df, type = "bayes"))
#> # A tibble: 2 × 20
#>   term    effectsize                       estimate std.error conf.level
#>   <chr>   <chr>                               <dbl>     <dbl>      <dbl>
#> 1 Overall meta-analytic posterior estimate   -0.643     0.220       0.95
#> 2 tau     meta-analytic posterior estimate    0.484     0.182       0.95
#>   conf.low conf.high weight  bf10  rhat   ess component prior.distribution
#>      <dbl>     <dbl>  <dbl> <dbl> <dbl> <dbl> <chr>     <chr>             
#> 1   -1.11     -0.242     NA  53.0     1 3507  meta      Student's t       
#> 2    0.205     0.909     NA  53.0     1 3460. meta      Inverse gamma     
#>   prior.location prior.scale method                                 conf.method
#>            <dbl>       <dbl> <chr>                                  <chr>      
#> 1              0       0.707 Bayesian meta-analysis using 'metaBMA' ETI        
#> 2              1       0.15  Bayesian meta-analysis using 'metaBMA' ETI        
#>   log_e_bf10 n.obs expression
#>        <dbl> <int> <list>    
#> 1       3.97    16 <language>
#> 2       3.97    16 <language>
```

## Centrality description

``` r

# for reproducibility
set.seed(123)

# ----------------------- parametric -----------------------

centrality_description(iris, Species, Sepal.Length, type = "parametric")
#> # A tibble: 3 × 14
#>   Species    Sepal.Length std.dev   iqr conf.low conf.high   min   max skewness
#>   <fct>             <dbl>   <dbl> <dbl>    <dbl>     <dbl> <dbl> <dbl>    <dbl>
#> 1 setosa             5.01   0.352 0.400     4.90      5.10   4.3   5.8    0.120
#> 2 versicolor         5.94   0.516 0.7       5.80      6.07   4.9   7      0.105
#> 3 virginica          6.59   0.636 0.750     6.39      6.79   4.9   7.9    0.118
#>   kurtosis n.obs missing.obs expression n.expression          
#>      <dbl> <int>       <int> <list>     <chr>                 
#> 1  -0.253     50           0 <language> "setosa\n(n = 50)"    
#> 2  -0.533     50           0 <language> "versicolor\n(n = 50)"
#> 3   0.0329    50           0 <language> "virginica\n(n = 50)"

# ----------------------- non-parametric -------------------

centrality_description(mtcars, am, wt, type = "nonparametric")
#> # A tibble: 2 × 14
#>      am    wt   mad   iqr conf.low conf.high   min   max skewness kurtosis n.obs
#>   <dbl> <dbl> <dbl> <dbl>    <dbl>     <dbl> <dbl> <dbl>    <dbl>    <dbl> <int>
#> 1     0  3.52 0.452 0.41      3.44      3.84  2.46  5.42    1.15     1.06     19
#> 2     1  2.32 0.682 0.942     1.94      2.78  1.51  3.57    0.269   -0.654    13
#>   missing.obs expression n.expression 
#>         <int> <list>     <chr>        
#> 1           0 <language> "0\n(n = 19)"
#> 2           0 <language> "1\n(n = 13)"

# ----------------------- robust ---------------------------

centrality_description(ToothGrowth, supp, len, type = "robust")
#> # A tibble: 2 × 14
#>   supp    len std.dev   iqr conf.low conf.high   min   max skewness kurtosis
#>   <fct> <dbl>   <dbl> <dbl>    <dbl>     <dbl> <dbl> <dbl>    <dbl>    <dbl>
#> 1 OJ     21.7    6.61  10.9     17.5      23.5   8.2  30.9   -0.580   -0.831
#> 2 VC     16.6    8.27  12.5     13.6      19.6   4.2  33.9    0.306   -0.700
#>   n.obs missing.obs expression n.expression  
#>   <int>       <int> <list>     <chr>         
#> 1    30           0 <language> "OJ\n(n = 30)"
#> 2    30           0 <language> "VC\n(n = 30)"

# ----------------------- Bayesian -------------------------

centrality_description(sleep, group, extra, type = "bayes")
#> # A tibble: 2 × 13
#>   group  extra   iqr conf.low conf.high   min   max skewness kurtosis n.obs
#>   <fct>  <dbl> <dbl>    <dbl>     <dbl> <dbl> <dbl>    <dbl>    <dbl> <int>
#> 1 1     0.0579  2.8   -1.39        3.66  -1.6   3.7    0.581   -0.630    10
#> 2 2     0.973   3.82   0.0728      4.88  -0.1   5.5    0.386   -1.42     10
#>   missing.obs expression n.expression 
#>         <int> <list>     <chr>        
#> 1           0 <language> "1\n(n = 10)"
#> 2           0 <language> "2\n(n = 10)"
```

## Pairwise comparisons for one-way design

### Between-subjects design

``` r

# ----------------------- parametric -----------------------

# if `var.equal = TRUE`, then Student's *t*-test will be run
pairwise_comparisons(
  data            = ggplot2::msleep,
  x               = vore,
  y               = brainwt,
  type            = "parametric",
  var.equal       = TRUE,
  paired          = FALSE,
  p.adjust.method = "bonferroni"
)
#> # A tibble: 6 × 6
#>   group1  group2  p.value p.adjust.method test        expression
#>   <chr>   <chr>     <dbl> <chr>           <chr>       <list>    
#> 1 carni   herbi     1     Bonferroni      Student's t <language>
#> 2 carni   insecti   1     Bonferroni      Student's t <language>
#> 3 carni   omni      1     Bonferroni      Student's t <language>
#> 4 herbi   insecti   1     Bonferroni      Student's t <language>
#> 5 herbi   omni      0.979 Bonferroni      Student's t <language>
#> 6 insecti omni      1     Bonferroni      Student's t <language>

# if `var.equal = FALSE`, then Games-Howell test will be run
pairwise_comparisons(
  data            = ggplot2::msleep,
  x               = vore,
  y               = brainwt,
  type            = "parametric",
  var.equal       = FALSE,
  paired          = FALSE,
  p.adjust.method = "bonferroni"
)
#> # A tibble: 6 × 9
#>   group1  group2  statistic p.value alternative distribution p.adjust.method
#>   <chr>   <chr>       <dbl>   <dbl> <chr>       <chr>        <chr>          
#> 1 carni   herbi        2.17       1 two.sided   q            Bonferroni     
#> 2 carni   insecti     -2.17       1 two.sided   q            Bonferroni     
#> 3 carni   omni         1.10       1 two.sided   q            Bonferroni     
#> 4 herbi   insecti     -2.41       1 two.sided   q            Bonferroni     
#> 5 herbi   omni        -1.87       1 two.sided   q            Bonferroni     
#> 6 insecti omni         2.19       1 two.sided   q            Bonferroni     
#>   test         expression
#>   <chr>        <list>    
#> 1 Games-Howell <language>
#> 2 Games-Howell <language>
#> 3 Games-Howell <language>
#> 4 Games-Howell <language>
#> 5 Games-Howell <language>
#> 6 Games-Howell <language>

# ----------------------- non-parametric -------------------

pairwise_comparisons(
  data            = ggplot2::msleep,
  x               = vore,
  y               = brainwt,
  type            = "nonparametric",
  paired          = FALSE,
  p.adjust.method = "none"
)
#> # A tibble: 6 × 9
#>   group1  group2  statistic p.value alternative distribution p.adjust.method
#>   <chr>   <chr>       <dbl>   <dbl> <chr>       <chr>        <chr>          
#> 1 carni   herbi       0.582  0.561  two.sided   z            None           
#> 2 carni   insecti     1.88   0.0595 two.sided   z            None           
#> 3 carni   omni        1.14   0.254  two.sided   z            None           
#> 4 herbi   insecti     1.63   0.102  two.sided   z            None           
#> 5 herbi   omni        0.717  0.474  two.sided   z            None           
#> 6 insecti omni        1.14   0.254  two.sided   z            None           
#>   test  expression
#>   <chr> <list>    
#> 1 Dunn  <language>
#> 2 Dunn  <language>
#> 3 Dunn  <language>
#> 4 Dunn  <language>
#> 5 Dunn  <language>
#> 6 Dunn  <language>

# ----------------------- robust ---------------------------

pairwise_comparisons(
  data            = ggplot2::msleep,
  x               = vore,
  y               = brainwt,
  type            = "robust",
  paired          = FALSE,
  p.adjust.method = "fdr"
)
#> # A tibble: 6 × 10
#>   group1  group2  estimate conf.level conf.low conf.high p.value p.adjust.method
#>   <chr>   <chr>      <dbl>      <dbl>    <dbl>     <dbl>   <dbl> <chr>          
#> 1 carni   herbi   -0.0323        0.95  -0.248     0.184    0.790 FDR            
#> 2 carni   insecti  0.0451        0.95  -0.0484    0.139    0.552 FDR            
#> 3 carni   omni     0.00520       0.95  -0.114     0.124    0.898 FDR            
#> 4 herbi   insecti  0.0774        0.95  -0.133     0.288    0.552 FDR            
#> 5 herbi   omni     0.0375        0.95  -0.182     0.257    0.790 FDR            
#> 6 insecti omni    -0.0399        0.95  -0.142     0.0625   0.552 FDR            
#>   test                 expression
#>   <chr>                <list>    
#> 1 Yuen's trimmed means <language>
#> 2 Yuen's trimmed means <language>
#> 3 Yuen's trimmed means <language>
#> 4 Yuen's trimmed means <language>
#> 5 Yuen's trimmed means <language>
#> 6 Yuen's trimmed means <language>

# ----------------------- Bayesian -------------------------

pairwise_comparisons(
  data   = ggplot2::msleep,
  x      = vore,
  y      = brainwt,
  type   = "bayes",
  paired = FALSE
)
#> # A tibble: 6 × 18
#>   group1  group2  term       effectsize      estimate conf.level conf.low
#>   <chr>   <chr>   <chr>      <chr>              <dbl>      <dbl>    <dbl>
#> 1 carni   herbi   Difference Bayesian t-test  -0.384        0.95  -1.34  
#> 2 carni   insecti Difference Bayesian t-test   0.0342       0.95  -0.0433
#> 3 carni   omni    Difference Bayesian t-test  -0.0442       0.95  -0.240 
#> 4 herbi   insecti Difference Bayesian t-test   0.377        0.95  -0.775 
#> 5 herbi   omni    Difference Bayesian t-test   0.364        0.95  -0.318 
#> 6 insecti omni    Difference Bayesian t-test  -0.0720       0.95  -0.341 
#>   conf.high    pd prior.distribution prior.location prior.scale  bf10
#>       <dbl> <dbl> <chr>                       <dbl>       <dbl> <dbl>
#> 1     0.494 0.805 cauchy                          0       0.707 0.540
#> 2     0.131 0.821 cauchy                          0       0.707 0.718
#> 3     0.140 0.676 cauchy                          0       0.707 0.427
#> 4     1.67  0.751 cauchy                          0       0.707 0.540
#> 5     1.12  0.852 cauchy                          0       0.707 0.571
#> 6     0.149 0.747 cauchy                          0       0.707 0.545
#>   conf.method log_e_bf10 n.obs expression test       
#>   <chr>            <dbl> <int> <list>     <chr>      
#> 1 ETI             -0.617    29 <language> Student's t
#> 2 ETI             -0.332    14 <language> Student's t
#> 3 ETI             -0.851    26 <language> Student's t
#> 4 ETI             -0.616    25 <language> Student's t
#> 5 ETI             -0.560    37 <language> Student's t
#> 6 ETI             -0.606    22 <language> Student's t
```

### Within-subjects design

``` r

# ----------------------- parametric -----------------------

pairwise_comparisons(
  data            = bugs_long,
  x               = condition,
  y               = desire,
  subject.id      = subject,
  type            = "parametric",
  paired          = TRUE,
  p.adjust.method = "BH"
)
#> # A tibble: 6 × 6
#>   group1 group2  p.value p.adjust.method test        expression
#>   <chr>  <chr>     <dbl> <chr>           <chr>       <list>    
#> 1 HDHF   HDLF   1.06e- 3 FDR             Student's t <language>
#> 2 HDHF   LDHF   7.02e- 2 FDR             Student's t <language>
#> 3 HDHF   LDLF   3.95e-12 FDR             Student's t <language>
#> 4 HDLF   LDHF   6.74e- 2 FDR             Student's t <language>
#> 5 HDLF   LDLF   1.99e- 3 FDR             Student's t <language>
#> 6 LDHF   LDLF   6.66e- 9 FDR             Student's t <language>

# ----------------------- non-parametric -------------------

pairwise_comparisons(
  data            = bugs_long,
  x               = condition,
  y               = desire,
  subject.id      = subject,
  type            = "nonparametric",
  paired          = TRUE,
  p.adjust.method = "BY"
)
#> # A tibble: 6 × 9
#>   group1 group2 statistic  p.value alternative distribution p.adjust.method
#>   <chr>  <chr>      <dbl>    <dbl> <chr>       <chr>        <chr>          
#> 1 HDHF   HDLF        4.78 1.44e- 5 two.sided   t            BY             
#> 2 HDHF   LDHF        2.44 4.47e- 2 two.sided   t            BY             
#> 3 HDHF   LDLF        8.01 5.45e-13 two.sided   t            BY             
#> 4 HDLF   LDHF        2.34 4.96e- 2 two.sided   t            BY             
#> 5 HDLF   LDLF        3.23 5.05e- 3 two.sided   t            BY             
#> 6 LDHF   LDLF        5.57 4.64e- 7 two.sided   t            BY             
#>   test           expression
#>   <chr>          <list>    
#> 1 Durbin-Conover <language>
#> 2 Durbin-Conover <language>
#> 3 Durbin-Conover <language>
#> 4 Durbin-Conover <language>
#> 5 Durbin-Conover <language>
#> 6 Durbin-Conover <language>

# ----------------------- robust ---------------------------

pairwise_comparisons(
  data            = bugs_long,
  x               = condition,
  y               = desire,
  subject.id      = subject,
  type            = "robust",
  paired          = TRUE,
  p.adjust.method = "hommel"
)
#> # A tibble: 6 × 11
#>   group1 group2 estimate conf.level conf.low conf.high     p.value  p.crit
#>   <chr>  <chr>     <dbl>      <dbl>    <dbl>     <dbl>       <dbl>   <dbl>
#> 1 HDHF   HDLF      1.03        0.95   0.140      1.92  0.00999     0.0127 
#> 2 HDHF   LDHF      0.454       0.95  -0.104      1.01  0.0520      0.025  
#> 3 HDHF   LDLF      1.95        0.95   1.09       2.82  0.000000564 0.00851
#> 4 HDLF   LDHF     -0.676       0.95  -1.61       0.256 0.0520      0.05   
#> 5 HDLF   LDLF      0.889       0.95   0.0244     1.75  0.0203      0.0169 
#> 6 LDHF   LDLF      1.35        0.95   0.560      2.14  0.000102    0.0102 
#>   p.adjust.method test                 expression
#>   <chr>           <chr>                <list>    
#> 1 Hommel          Yuen's trimmed means <language>
#> 2 Hommel          Yuen's trimmed means <language>
#> 3 Hommel          Yuen's trimmed means <language>
#> 4 Hommel          Yuen's trimmed means <language>
#> 5 Hommel          Yuen's trimmed means <language>
#> 6 Hommel          Yuen's trimmed means <language>

# ----------------------- Bayesian -------------------------

pairwise_comparisons(
  data       = bugs_long,
  x          = condition,
  y          = desire,
  subject.id = subject,
  type       = "bayes",
  paired     = TRUE,
  bf.prior   = 0.77
)
#> # A tibble: 6 × 18
#>   group1 group2 term       effectsize      estimate conf.level conf.low
#>   <chr>  <chr>  <chr>      <chr>              <dbl>      <dbl>    <dbl>
#> 1 HDHF   HDLF   Difference Bayesian t-test    1.11        0.95   0.485 
#> 2 HDHF   LDHF   Difference Bayesian t-test    0.447       0.95  -0.0576
#> 3 HDHF   LDLF   Difference Bayesian t-test    2.13        0.95   1.64  
#> 4 HDLF   LDHF   Difference Bayesian t-test   -0.655       0.95  -1.32  
#> 5 HDLF   LDLF   Difference Bayesian t-test    0.981       0.95   0.400 
#> 6 LDHF   LDLF   Difference Bayesian t-test    1.66        0.95   1.15  
#>   conf.high    pd prior.distribution prior.location prior.scale     bf10
#>       <dbl> <dbl> <chr>                       <dbl>       <dbl>    <dbl>
#> 1    1.74   1     cauchy                          0        0.77 3.95e+ 1
#> 2    0.954  0.960 cauchy                          0        0.77 5.42e- 1
#> 3    2.62   1     cauchy                          0        0.77 1.22e+10
#> 4    0.0379 0.968 cauchy                          0        0.77 6.50e- 1
#> 5    1.58   0.999 cauchy                          0        0.77 1.72e+ 1
#> 6    2.16   1     cauchy                          0        0.77 4.78e+ 6
#>   conf.method log_e_bf10 n.obs expression test       
#>   <chr>            <dbl> <int> <list>     <chr>      
#> 1 ETI              3.68     88 <language> Student's t
#> 2 ETI             -0.612    88 <language> Student's t
#> 3 ETI             23.2      88 <language> Student's t
#> 4 ETI             -0.430    88 <language> Student's t
#> 5 ETI              2.84     88 <language> Student's t
#> 6 ETI             15.4      88 <language> Student's t
```

## Suggestions

If you find any bugs or have any suggestions/remarks, please file an
issue on GitHub:
<https://github.com/IndrajeetPatil/statsExpressions/issues>
