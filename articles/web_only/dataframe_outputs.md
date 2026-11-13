# Data frame outputs

## One-sample test output

\
`# for reproducibility`\
[`set.seed`](https://rdrr.io/r/base/Random.html)`(``123``)`\
\
`# ----------------------- parametric -----------------------`\
\
[`one_sample_test`](https://www.indrapatil.com/statsExpressions/reference/one_sample_test.md)`(``mtcars``, ``wt``, test.value ``=`` ``3``)`\
`#> ``# A tibble: 1 × 15`\
`#>      ``mu`` ``statistic`` ``df.error`` ``p.value`` ``method``            ``alternative`` ``effectsize`\
`#>   ``<dbl>``     ``<dbl>``    ``<dbl>``   ``<dbl>`` ``<chr>``             ``<chr>``       ``<chr>``     `\
`#> ``1``     3      1.26       31   ``0.``218 One Sample t-test two.sided   Hedges' g `\
`#>   ``estimate`` ``conf.level`` ``conf.low`` ``conf.high`` ``conf.method`` ``conf.distribution`` ``n.obs`\
`#>      ``<dbl>``      ``<dbl>``    ``<dbl>``     ``<dbl>`` ``<chr>``       ``<chr>``             ``<int>`\
`#> ``1``    ``0.``217       ``0.``95   -``0.``127``     ``0.``557 ncp         t                    32`\
`#>   ``expression`\
`#>   ``<list>``    `\
`#> ``1`` ``<language>`\
\
`# biased (Cohen's d) effect size`\
[`one_sample_test`](https://www.indrapatil.com/statsExpressions/reference/one_sample_test.md)`(``mtcars``, ``wt``, test.value ``=`` ``3``, effsize.type ``=`` ``"d"``)`\
`#> ``# A tibble: 1 × 15`\
`#>      ``mu`` ``statistic`` ``df.error`` ``p.value`` ``method``            ``alternative`` ``effectsize`\
`#>   ``<dbl>``     ``<dbl>``    ``<dbl>``   ``<dbl>`` ``<chr>``             ``<chr>``       ``<chr>``     `\
`#> ``1``     3      1.26       31   ``0.``218 One Sample t-test two.sided   Cohen's d `\
`#>   ``estimate`` ``conf.level`` ``conf.low`` ``conf.high`` ``conf.method`` ``conf.distribution`` ``n.obs`\
`#>      ``<dbl>``      ``<dbl>``    ``<dbl>``     ``<dbl>`` ``<chr>``       ``<chr>``             ``<int>`\
`#> ``1``    ``0.``222       ``0.``95   -``0.``131``     ``0.``571 ncp         t                    32`\
`#>   ``expression`\
`#>   ``<list>``    `\
`#> ``1`` ``<language>`\
\
`# ----------------------- non-parametric -------------------`\
\
[`one_sample_test`](https://www.indrapatil.com/statsExpressions/reference/one_sample_test.md)`(``mtcars``, ``wt``, test.value ``=`` ``3``, type ``=`` ``"nonparametric"``)`\
`#> ``# A tibble: 1 × 12`\
`#>   ``statistic`` ``p.value`` ``method``                    ``alternative`` ``effectsize``       `\
`#>       ``<dbl>``   ``<dbl>`` ``<chr>``                     ``<chr>``       ``<chr>``            `\
`#> ``1``       319   ``0.``308 Wilcoxon signed rank test two.sided   r (rank biserial)`\
`#>   ``estimate`` ``conf.level`` ``conf.low`` ``conf.high`` ``conf.method`` ``n.obs`` ``expression`\
`#>      ``<dbl>``      ``<dbl>``    ``<dbl>``     ``<dbl>`` ``<chr>``       ``<int>`` ``<list>``    `\
`#> ``1``    ``0.``208       ``0.``95   -``0.``184``     ``0.``543 normal         32 ``<language>`\
\
`# ----------------------- robust ---------------------------`\
\
[`one_sample_test`](https://www.indrapatil.com/statsExpressions/reference/one_sample_test.md)`(``mtcars``, ``wt``, test.value ``=`` ``3``, type ``=`` ``"robust"``)`\
`#> ``# A tibble: 1 × 10`\
`#>   ``statistic`` ``p.value`` ``n.obs`` ``method``                                 ``effectsize``  `\
`#>       ``<dbl>``   ``<dbl>`` ``<int>`` ``<chr>``                                  ``<chr>``       `\
`#> ``1``      1.18   ``0.``275    32 Bootstrap-t method for one-sample test Trimmed mean`\
`#>   ``estimate`` ``conf.level`` ``conf.low`` ``conf.high`` ``expression`\
`#>      ``<dbl>``      ``<dbl>``    ``<dbl>``     ``<dbl>`` ``<list>``    `\
`#> ``1``     3.20       ``0.``95     2.85      3.54 ``<language>`\
\
`# ----------------------- Bayesian -------------------------`\
\
[`one_sample_test`](https://www.indrapatil.com/statsExpressions/reference/one_sample_test.md)`(``mtcars``, ``wt``, test.value ``=`` ``3``, type ``=`` ``"bayes"``)`\
`#> ``# A tibble: 1 × 16`\
`#>   ``term``       ``effectsize``      ``estimate`` ``conf.level`` ``conf.low`` ``conf.high``    ``pd`\
`#>   ``<chr>``      ``<chr>``              ``<dbl>``      ``<dbl>``    ``<dbl>``     ``<dbl>`` ``<dbl>`\
`#> ``1`` Difference Bayesian t-test    ``0.``195       ``0.``95   -``0.``165``     ``0.``555  ``0.``86`\
`#>   ``prior.distribution`` ``prior.location`` ``prior.scale``  ``bf10`` ``method``         `\
`#>   ``<chr>``                       ``<dbl>``       ``<dbl>`` ``<dbl>`` ``<chr>``          `\
`#> ``1`` cauchy                          ``0``       ``0.``707 ``0.``387 Bayesian t-test`\
`#>   ``conf.method`` ``log_e_bf10`` ``n.obs`` ``expression`\
`#>   ``<chr>``            ``<dbl>`` ``<int>`` ``<list>``    `\
`#> ``1`` ETI             -``0.``950``    32 ``<language>`

## Two-sample test output

\
`# ----------------------- within-subjects -------------------------------------`\
\
`# data`\
`df`` ``<-`` ``dplyr``::`[`filter`](https://dplyr.tidyverse.org/reference/filter.html)`(``bugs_long``, ``condition`` `[`%in%`](https://rdrr.io/r/base/match.html)` `[`c`](https://rdrr.io/r/base/c.html)`(``"LDLF"``, ``"LDHF"``)``)`\
\
`# for reproducibility`\
[`set.seed`](https://rdrr.io/r/base/Random.html)`(``123``)`\
\
`# ----------------------- parametric ---------------------------------------`\
\
[`two_sample_test`](https://www.indrapatil.com/statsExpressions/reference/two_sample_test.md)`(`\
`  ``df``,`\
`  ``condition``,`\
`  ``desire``,`\
`  subject.id ``=`` ``subject``,`\
`  paired ``=`` ``TRUE``,`\
`  type ``=`` ``"parametric"`\
`)`\
`#> ``# A tibble: 1 × 16`\
`#>   ``term``   ``group``     ``statistic`` ``df.error``       ``p.value`` ``method``        ``alternative`\
`#>   ``<chr>``  ``<chr>``         ``<dbl>``    ``<dbl>``         ``<dbl>`` ``<chr>``         ``<chr>``      `\
`#> ``1`` desire condition      6.65       90 ``0.000``000``00``2``22`` Paired t-test two.sided  `\
`#>   ``effectsize`` ``estimate`` ``conf.level`` ``conf.low`` ``conf.high`` ``conf.method`\
`#>   ``<chr>``         ``<dbl>``      ``<dbl>``    ``<dbl>``     ``<dbl>`` ``<chr>``      `\
`#> ``1`` Hedges' g     ``0.``691       ``0.``95    ``0.``462     ``0.``917 ncp        `\
`#>   ``conf.distribution`` ``n.obs`` ``expression`\
`#>   ``<chr>``             ``<int>`` ``<list>``    `\
`#> ``1`` t                    91 ``<language>`\
\
`# ----------------------- non-parametric -----------------------------------`\
\
[`two_sample_test`](https://www.indrapatil.com/statsExpressions/reference/two_sample_test.md)`(`\
`  ``df``,`\
`  ``condition``,`\
`  ``desire``,`\
`  subject.id ``=`` ``subject``,`\
`  paired ``=`` ``TRUE``,`\
`  type ``=`` ``"nonparametric"`\
`)`\
`#> ``# A tibble: 1 × 14`\
`#>   ``parameter1`` ``parameter2`` ``statistic``      ``p.value`` ``method``                   `\
`#>   ``<chr>``      ``<chr>``          ``<dbl>``        ``<dbl>`` ``<chr>``                    `\
`#> ``1`` desire     condition      ``2``25``0``. ``0.000``000``0``24``1`` Wilcoxon signed rank test`\
`#>   ``alternative`` ``effectsize``        ``estimate`` ``conf.level`` ``conf.low`` ``conf.high`\
`#>   ``<chr>``       ``<chr>``                ``<dbl>``      ``<dbl>``    ``<dbl>``     ``<dbl>`\
`#> ``1`` two.sided   r (rank biserial)    ``0.``761       ``0.``95    ``0.``642     ``0.``844`\
`#>   ``conf.method`` ``n.obs`` ``expression`\
`#>   ``<chr>``       ``<int>`` ``<list>``    `\
`#> ``1`` normal         91 ``<language>`\
\
`# ----------------------- robust --------------------------------------------`\
\
[`two_sample_test`](https://www.indrapatil.com/statsExpressions/reference/two_sample_test.md)`(`\
`  ``df``,`\
`  ``condition``,`\
`  ``desire``,`\
`  subject.id ``=`` ``subject``,`\
`  paired ``=`` ``TRUE``,`\
`  type ``=`` ``"robust"`\
`)`\
`#> ``# A tibble: 1 × 15`\
`#>   ``statistic`` ``df.error``      ``p.value`\
`#>       ``<dbl>``    ``<dbl>``        ``<dbl>`\
`#> ``1``      6.46       54 ``0.000``000``0``31``3`\
`#>   ``method``                                            `\
`#>   ``<chr>``                                             `\
`#> ``1`` Yuen's test on trimmed means for dependent samples`\
`#>   ``effectsize``                                              ``estimate`` ``conf.level`\
`#>   ``<chr>``                                                      ``<dbl>``      ``<dbl>`\
`#> ``1`` Algina-Keselman-Penfield robust standardized difference    ``0.``533       ``0.``95`\
`#>   ``conf.low`` ``conf.high``    ``mu`` ``small`` ``medium`` ``large`` ``n.obs`` ``expression`\
`#>      ``<dbl>``     ``<dbl>`` ``<dbl>`` ``<dbl>``  ``<dbl>`` ``<dbl>`` ``<int>`` ``<list>``    `\
`#> ``1``    ``0.``369     ``0.``707     ``0``   ``0.``1    ``0.``3   ``0.``5    91 ``<language>`\
\
`# ----------------------- Bayesian ---------------------------------------`\
\
[`two_sample_test`](https://www.indrapatil.com/statsExpressions/reference/two_sample_test.md)`(`\
`  ``df``,`\
`  ``condition``,`\
`  ``desire``,`\
`  subject.id ``=`` ``subject``,`\
`  paired ``=`` ``TRUE``,`\
`  type ``=`` ``"bayes"`\
`)`\
`#> ``# A tibble: 1 × 16`\
`#>   ``term``       ``effectsize``      ``estimate`` ``conf.level`` ``conf.low`` ``conf.high``    ``pd`\
`#>   ``<chr>``      ``<chr>``              ``<dbl>``      ``<dbl>``    ``<dbl>``     ``<dbl>`` ``<dbl>`\
`#> ``1`` Difference Bayesian t-test     1.63       ``0.``95     1.13      2.11     1`\
`#>   ``prior.distribution`` ``prior.location`` ``prior.scale``     ``bf10`` ``method``         `\
`#>   ``<chr>``                       ``<dbl>``       ``<dbl>``    ``<dbl>`` ``<chr>``          `\
`#> ``1`` cauchy                          ``0``       ``0.``707 4``76``2``370``. Bayesian t-test`\
`#>   ``conf.method`` ``log_e_bf10`` ``n.obs`` ``expression`\
`#>   ``<chr>``            ``<dbl>`` ``<int>`` ``<list>``    `\
`#> ``1`` ETI               15.4    91 ``<language>`\
\
`# ----------------------- between-subjects -------------------------------------`\
\
`# for reproducibility`\
[`set.seed`](https://rdrr.io/r/base/Random.html)`(``123``)`\
\
`# ----------------------- parametric ---------------------------------------`\
\
`# unequal variance`\
[`two_sample_test`](https://www.indrapatil.com/statsExpressions/reference/two_sample_test.md)`(``ToothGrowth``, ``supp``, ``len``, type ``=`` ``"parametric"``)`\
`#> ``# A tibble: 1 × 18`\
`#>   ``parameter1`` ``parameter2`` ``mean.parameter1`` ``mean.parameter2`` ``statistic`` ``df.error`\
`#>   ``<chr>``      ``<chr>``                ``<dbl>``           ``<dbl>``     ``<dbl>``    ``<dbl>`\
`#> ``1`` len        supp                  20.7            17.0      1.92     55.3`\
`#>   ``p.value`` ``method``                  ``alternative`` ``effectsize`` ``estimate`` ``conf.level`\
`#>     ``<dbl>`` ``<chr>``                   ``<chr>``       ``<chr>``         ``<dbl>``      ``<dbl>`\
`#> ``1``  ``0.0``60``6`` Welch Two Sample t-test two.sided   Hedges' g     ``0.``488       ``0.``95`\
`#>   ``conf.low`` ``conf.high`` ``conf.method`` ``conf.distribution`` ``n.obs`` ``expression`\
`#>      ``<dbl>``     ``<dbl>`` ``<chr>``       ``<chr>``             ``<int>`` ``<list>``    `\
`#> ``1``  -``0.0``21``7``     ``0.``993 ncp         t                    60 ``<language>`\
\
`# equal variance`\
[`two_sample_test`](https://www.indrapatil.com/statsExpressions/reference/two_sample_test.md)`(``ToothGrowth``, ``supp``, ``len``, type ``=`` ``"parametric"``, var.equal ``=`` ``TRUE``)`\
`#> ``# A tibble: 1 × 18`\
`#>   ``parameter1`` ``parameter2`` ``mean.parameter1`` ``mean.parameter2`` ``statistic`` ``df.error`\
`#>   ``<chr>``      ``<chr>``                ``<dbl>``           ``<dbl>``     ``<dbl>``    ``<dbl>`\
`#> ``1`` len        supp                  20.7            17.0      1.92       58`\
`#>   ``p.value`` ``method``            ``alternative`` ``effectsize`` ``estimate`` ``conf.level`` ``conf.low`\
`#>     ``<dbl>`` ``<chr>``             ``<chr>``       ``<chr>``         ``<dbl>``      ``<dbl>``    ``<dbl>`\
`#> ``1``  ``0.0``60``4`` Two Sample t-test two.sided   Hedges' g     ``0.``488       ``0.``95  -``0.0``21``7`\
`#>   ``conf.high`` ``conf.method`` ``conf.distribution`` ``n.obs`` ``expression`\
`#>       ``<dbl>`` ``<chr>``       ``<chr>``             ``<int>`` ``<list>``    `\
`#> ``1``     ``0.``993 ncp         t                    60 ``<language>`\
\
`# biased (Cohen's d) effect size`\
[`two_sample_test`](https://www.indrapatil.com/statsExpressions/reference/two_sample_test.md)`(``ToothGrowth``, ``supp``, ``len``, type ``=`` ``"parametric"``, effsize.type ``=`` ``"d"``)`\
`#> ``# A tibble: 1 × 18`\
`#>   ``parameter1`` ``parameter2`` ``mean.parameter1`` ``mean.parameter2`` ``statistic`` ``df.error`\
`#>   ``<chr>``      ``<chr>``                ``<dbl>``           ``<dbl>``     ``<dbl>``    ``<dbl>`\
`#> ``1`` len        supp                  20.7            17.0      1.92     55.3`\
`#>   ``p.value`` ``method``                  ``alternative`` ``effectsize`` ``estimate`` ``conf.level`\
`#>     ``<dbl>`` ``<chr>``                   ``<chr>``       ``<chr>``         ``<dbl>``      ``<dbl>`\
`#> ``1``  ``0.0``60``6`` Welch Two Sample t-test two.sided   Cohen's d     ``0.``495       ``0.``95`\
`#>   ``conf.low`` ``conf.high`` ``conf.method`` ``conf.distribution`` ``n.obs`` ``expression`\
`#>      ``<dbl>``     ``<dbl>`` ``<chr>``       ``<chr>``             ``<int>`` ``<list>``    `\
`#> ``1``  -``0.0``22``0``      1.01 ncp         t                    60 ``<language>`\
\
`# ----------------------- non-parametric -----------------------------------`\
\
[`two_sample_test`](https://www.indrapatil.com/statsExpressions/reference/two_sample_test.md)`(``ToothGrowth``, ``supp``, ``len``, type ``=`` ``"nonparametric"``)`\
`#> ``# A tibble: 1 × 14`\
`#>   ``parameter1`` ``parameter2`` ``statistic`` ``p.value`` ``method``                 ``alternative`\
`#>   ``<chr>``      ``<chr>``          ``<dbl>``   ``<dbl>`` ``<chr>``                  ``<chr>``      `\
`#> ``1`` len        supp            576.  ``0.0``64``5`` Wilcoxon rank sum test two.sided  `\
`#>   ``effectsize``        ``estimate`` ``conf.level`` ``conf.low`` ``conf.high`` ``conf.method`` ``n.obs`\
`#>   ``<chr>``                ``<dbl>``      ``<dbl>``    ``<dbl>``     ``<dbl>`` ``<chr>``       ``<int>`\
`#> ``1`` r (rank biserial)    ``0.``279       ``0.``95 -``0.00``8``12``     ``0.``523 normal         60`\
`#>   ``expression`\
`#>   ``<list>``    `\
`#> ``1`` ``<language>`\
\
`# ----------------------- robust --------------------------------------------`\
\
[`two_sample_test`](https://www.indrapatil.com/statsExpressions/reference/two_sample_test.md)`(``ToothGrowth``, ``supp``, ``len``, type ``=`` ``"robust"``)`\
`#> ``# A tibble: 1 × 11`\
`#>   ``statistic`` ``df.error`` ``p.value`\
`#>       ``<dbl>``    ``<dbl>``   ``<dbl>`\
`#> ``1``      2.29     33.5  ``0.0``28``6`\
`#>   ``method``                                              `\
`#>   ``<chr>``                                               `\
`#> ``1`` Yuen's test on trimmed means for independent samples`\
`#>   ``effectsize``                                              ``estimate`` ``conf.level`\
`#>   ``<chr>``                                                      ``<dbl>``      ``<dbl>`\
`#> ``1`` Algina-Keselman-Penfield robust standardized difference    ``0.``683       ``0.``95`\
`#>   ``conf.low`` ``conf.high`` ``n.obs`` ``expression`\
`#>      ``<dbl>``     ``<dbl>`` ``<int>`` ``<list>``    `\
`#> ``1`` -``0.00``7``36``      2.36    60 ``<language>`\
\
`# ----------------------- Bayesian ---------------------------------------`\
\
[`two_sample_test`](https://www.indrapatil.com/statsExpressions/reference/two_sample_test.md)`(``ToothGrowth``, ``supp``, ``len``, type ``=`` ``"bayes"``)`\
`#> ``# A tibble: 1 × 16`\
`#>   ``term``       ``effectsize``      ``estimate`` ``conf.level`` ``conf.low`` ``conf.high``    ``pd`\
`#>   ``<chr>``      ``<chr>``              ``<dbl>``      ``<dbl>``    ``<dbl>``     ``<dbl>`` ``<dbl>`\
`#> ``1`` Difference Bayesian t-test     3.16       ``0.``95   -``0.``338``      6.78 ``0.``961`\
`#>   ``prior.distribution`` ``prior.location`` ``prior.scale``  ``bf10`` ``method``         `\
`#>   ``<chr>``                       ``<dbl>``       ``<dbl>`` ``<dbl>`` ``<chr>``          `\
`#> ``1`` cauchy                          ``0``       ``0.``707  1.20 Bayesian t-test`\
`#>   ``conf.method`` ``log_e_bf10`` ``n.obs`` ``expression`\
`#>   ``<chr>``            ``<dbl>`` ``<int>`` ``<list>``    `\
`#> ``1`` ETI              ``0.``181    60 ``<language>`

## One-way ANOVAs

\
`# for reproducibility`\
[`set.seed`](https://rdrr.io/r/base/Random.html)`(``123``)`\
\
`# ----------------------- parametric -------------------------------------`\
\
`# between-subjects`\
[`oneway_anova`](https://www.indrapatil.com/statsExpressions/reference/oneway_anova.md)`(`\
`  data ``=`` ``mtcars``,`\
`  x ``=`` ``cyl``,`\
`  y ``=`` ``wt`\
`)`\
`#> ``# A tibble: 1 × 14`\
`#>   ``statistic``    ``df`` ``df.error``   ``p.value`\
`#>       ``<dbl>`` ``<dbl>``    ``<dbl>``     ``<dbl>`\
`#> ``1``      20.2     2     19.0 ``0.000``0``19``6`\
`#>   ``method``                                                   ``effectsize`` ``estimate`\
`#>   ``<chr>``                                                    ``<chr>``         ``<dbl>`\
`#> ``1`` One-way analysis of means (not assuming equal variances) Omega2        ``0.``637`\
`#>   ``conf.level`` ``conf.low`` ``conf.high`` ``conf.method`` ``conf.distribution`` ``n.obs`` ``expression`\
`#>        ``<dbl>``    ``<dbl>``     ``<dbl>`` ``<chr>``       ``<chr>``             ``<int>`` ``<list>``    `\
`#> ``1``       ``0.``95    ``0.``370         1 ncp         F                    32 ``<language>`\
\
`# biased (partial eta-squared) effect size`\
[`oneway_anova`](https://www.indrapatil.com/statsExpressions/reference/oneway_anova.md)`(`\
`  data ``=`` ``mtcars``,`\
`  x ``=`` ``cyl``,`\
`  y ``=`` ``wt``,`\
`  effsize.type ``=`` ``"eta"`\
`)`\
`#> ``# A tibble: 1 × 14`\
`#>   ``statistic``    ``df`` ``df.error``   ``p.value`\
`#>       ``<dbl>`` ``<dbl>``    ``<dbl>``     ``<dbl>`\
`#> ``1``      20.2     2     19.0 ``0.000``0``19``6`\
`#>   ``method``                                                   ``effectsize`` ``estimate`\
`#>   ``<chr>``                                                    ``<chr>``         ``<dbl>`\
`#> ``1`` One-way analysis of means (not assuming equal variances) Eta2          ``0.``681`\
`#>   ``conf.level`` ``conf.low`` ``conf.high`` ``conf.method`` ``conf.distribution`` ``n.obs`` ``expression`\
`#>        ``<dbl>``    ``<dbl>``     ``<dbl>`` ``<chr>``       ``<chr>``             ``<int>`` ``<list>``    `\
`#> ``1``       ``0.``95    ``0.``437         1 ncp         F                    32 ``<language>`\
\
`# within-subjects design`\
[`oneway_anova`](https://www.indrapatil.com/statsExpressions/reference/oneway_anova.md)`(`\
`  data ``=`` ``bugs_long``,`\
`  x ``=`` ``condition``,`\
`  y ``=`` ``desire``,`\
`  subject.id ``=`` ``subject``,`\
`  paired ``=`` ``TRUE`\
`)`\
`#> ``# A tibble: 1 × 18`\
`#>   ``term``      ``sumsq`` ``sum.squares.error``    ``df`` ``df.error`` ``meansq`` ``statistic``  ``p.value`\
`#>   ``<chr>``     ``<dbl>``             ``<dbl>`` ``<dbl>``    ``<dbl>``  ``<dbl>``     ``<dbl>``    ``<dbl>`\
`#> ``1`` condition  233.              984.  2.63     229.   4.30      20.6 8.27``e``-11`\
`#>   ``method``                                              ``effectsize``       ``estimate`\
`#>   ``<chr>``                                               ``<chr>``               ``<dbl>`\
`#> ``1`` ANOVA estimation for factorial designs using 'afex' Omega2 (partial)   ``0.0``78``3`\
`#>   ``conf.level`` ``conf.low`` ``conf.high`` ``conf.method`` ``conf.distribution`` ``n.obs`` ``expression`\
`#>        ``<dbl>``    ``<dbl>``     ``<dbl>`` ``<chr>``       ``<chr>``             ``<int>`` ``<list>``    `\
`#> ``1``       ``0.``95   ``0.0``28``0``         1 ncp         F                    88 ``<language>`\
\
`# ----------------------- non-parametric ----------------------------------`\
\
`# between-subjects`\
[`oneway_anova`](https://www.indrapatil.com/statsExpressions/reference/oneway_anova.md)`(`\
`  data ``=`` ``mtcars``,`\
`  x ``=`` ``cyl``,`\
`  y ``=`` ``wt``,`\
`  type ``=`` ``"np"`\
`)`\
`#> ``# A tibble: 1 × 15`\
`#>   ``parameter1`` ``parameter2`` ``statistic`` ``df.error``   ``p.value`\
`#>   ``<chr>``      ``<chr>``          ``<dbl>``    ``<int>``     ``<dbl>`\
`#> ``1`` wt         cyl             22.8        2 ``0.000``0``11``2`\
`#>   ``method``                       ``effectsize``      ``estimate`` ``conf.level`` ``conf.low`\
`#>   ``<chr>``                        ``<chr>``              ``<dbl>``      ``<dbl>``    ``<dbl>`\
`#> ``1`` Kruskal-Wallis rank sum test Epsilon2 (rank)    ``0.``736       ``0.``95    ``0.``624`\
`#>   ``conf.high`` ``conf.method``          ``conf.iterations`` ``n.obs`` ``expression`\
`#>       ``<dbl>`` ``<chr>``                          ``<int>`` ``<int>`` ``<list>``    `\
`#> ``1``         1 percentile bootstrap             100    32 ``<language>`\
\
`# within-subjects design`\
[`oneway_anova`](https://www.indrapatil.com/statsExpressions/reference/oneway_anova.md)`(`\
`  data ``=`` ``bugs_long``,`\
`  x ``=`` ``condition``,`\
`  y ``=`` ``desire``,`\
`  subject.id ``=`` ``subject``,`\
`  paired ``=`` ``TRUE``,`\
`  type ``=`` ``"np"`\
`)`\
`#> ``# A tibble: 1 × 15`\
`#>   ``parameter1`` ``parameter2`` ``statistic`` ``df.error``  ``p.value`` ``method``                `\
`#>   ``<chr>``      ``<chr>``          ``<dbl>``    ``<dbl>``    ``<dbl>`` ``<chr>``                 `\
`#> ``1`` desire     condition       55.8        3 4.56``e``-12`` Friedman rank sum test`\
`#>   ``effectsize``  ``estimate`` ``conf.level`` ``conf.low`` ``conf.high`` ``conf.method``         `\
`#>   ``<chr>``          ``<dbl>``      ``<dbl>``    ``<dbl>``     ``<dbl>`` ``<chr>``               `\
`#> ``1`` Kendall's W    ``0.``211       ``0.``95    ``0.``154         1 percentile bootstrap`\
`#>   ``conf.iterations`` ``n.obs`` ``expression`\
`#>             ``<int>`` ``<int>`` ``<list>``    `\
`#> ``1``             100    88 ``<language>`\
\
`# ----------------------- robust -------------------------------------`\
\
`# between-subjects`\
[`oneway_anova`](https://www.indrapatil.com/statsExpressions/reference/oneway_anova.md)`(`\
`  data ``=`` ``mtcars``,`\
`  x ``=`` ``cyl``,`\
`  y ``=`` ``wt``,`\
`  type ``=`` ``"r"`\
`)`\
`#> ``# A tibble: 1 × 12`\
`#>   ``statistic``    ``df`` ``df.error`` ``p.value`\
`#>       ``<dbl>`` ``<dbl>``    ``<dbl>``   ``<dbl>`\
`#> ``1``      12.7     2     12.2 ``0.00``1``02`\
`#>   ``method``                                           `\
`#>   ``<chr>``                                            `\
`#> ``1`` A heteroscedastic one-way ANOVA for trimmed means`\
`#>   ``effectsize``                         ``estimate`` ``conf.level`` ``conf.low`` ``conf.high`\
`#>   ``<chr>``                                 ``<dbl>``      ``<dbl>``    ``<dbl>``     ``<dbl>`\
`#> ``1`` Explanatory measure of effect size     1.02       ``0.``95    ``0.``824      1.62`\
`#>   ``n.obs`` ``expression`\
`#>   ``<int>`` ``<list>``    `\
`#> ``1``    32 ``<language>`\
\
`# within-subjects design`\
[`oneway_anova`](https://www.indrapatil.com/statsExpressions/reference/oneway_anova.md)`(`\
`  data ``=`` ``bugs_long``,`\
`  x ``=`` ``condition``,`\
`  y ``=`` ``desire``,`\
`  subject.id ``=`` ``subject``,`\
`  paired ``=`` ``TRUE``,`\
`  type ``=`` ``"r"`\
`)`\
`#> ``# A tibble: 1 × 12`\
`#>   ``statistic``    ``df`` ``df.error``  ``p.value`\
`#>       ``<dbl>`` ``<dbl>``    ``<dbl>``    ``<dbl>`\
`#> ``1``      21.0  2.73     145. 1.15``e``-10`\
`#>   ``method``                                                             `\
`#>   ``<chr>``                                                              `\
`#> ``1`` A heteroscedastic one-way repeated measures ANOVA for trimmed means`\
`#>   ``effectsize``                                                      ``estimate`\
`#>   ``<chr>``                                                              ``<dbl>`\
`#> ``1`` Algina-Keselman-Penfield robust standardized difference average    ``0.``325`\
`#>   ``conf.level`` ``conf.low`` ``conf.high`` ``n.obs`` ``expression`\
`#>        ``<dbl>``    ``<dbl>``     ``<dbl>`` ``<int>`` ``<list>``    `\
`#> ``1``       ``0.``95    ``0.``110     ``0.``572    88 ``<language>`\
\
`# ----------------------- Bayesian -------------------------------------`\
\
`# between-subjects`\
[`oneway_anova`](https://www.indrapatil.com/statsExpressions/reference/oneway_anova.md)`(`\
`  data ``=`` ``mtcars``,`\
`  x ``=`` ``cyl``,`\
`  y ``=`` ``wt``,`\
`  type ``=`` ``"bayes"`\
`)`\
`#> ``# A tibble: 6 × 17`\
`#>   ``term``     ``pd`` ``prior.distribution`` ``prior.location`` ``prior.scale``   ``bf10`\
`#>   ``<chr>`` ``<dbl>`` ``<chr>``                       ``<dbl>``       ``<dbl>``  ``<dbl>`\
`#> ``1`` mu    1     cauchy                          ``0``       ``0.``707 ``20``9``68``.`\
`#> ``2`` cyl-4 1     cauchy                          ``0``       ``0.``707 ``20``9``68``.`\
`#> ``3`` cyl-6 ``0.``542 cauchy                          ``0``       ``0.``707 ``20``9``68``.`\
`#> ``4`` cyl-8 1     cauchy                          ``0``       ``0.``707 ``20``9``68``.`\
`#> ``5`` sig2  1     cauchy                          ``0``       ``0.``707 ``20``9``68``.`\
`#> ``6`` g_cyl 1     cauchy                          ``0``       ``0.``707 ``20``9``68``.`\
`#>   ``method``                          ``log_e_bf10`` ``effectsize``         ``estimate`` ``std.dev`\
`#>   ``<chr>``                                ``<dbl>`` ``<chr>``                 ``<dbl>``   ``<dbl>`\
`#> ``1`` Bayes factors for linear models       9.95 Bayesian R-squared    ``0.``581  ``0.0``81``5`\
`#> ``2`` Bayes factors for linear models       9.95 Bayesian R-squared    ``0.``581  ``0.0``81``5`\
`#> ``3`` Bayes factors for linear models       9.95 Bayesian R-squared    ``0.``581  ``0.0``81``5`\
`#> ``4`` Bayes factors for linear models       9.95 Bayesian R-squared    ``0.``581  ``0.0``81``5`\
`#> ``5`` Bayes factors for linear models       9.95 Bayesian R-squared    ``0.``581  ``0.0``81``5`\
`#> ``6`` Bayes factors for linear models       9.95 Bayesian R-squared    ``0.``581  ``0.0``81``5`\
`#>   ``conf.level`` ``conf.low`` ``conf.high`` ``conf.method`` ``n.obs`` ``expression`\
`#>        ``<dbl>``    ``<dbl>``     ``<dbl>`` ``<chr>``       ``<int>`` ``<list>``    `\
`#> ``1``       ``0.``95    ``0.``373     ``0.``702 HDI            32 ``<language>`\
`#> ``2``       ``0.``95    ``0.``373     ``0.``702 HDI            32 ``<language>`\
`#> ``3``       ``0.``95    ``0.``373     ``0.``702 HDI            32 ``<language>`\
`#> ``4``       ``0.``95    ``0.``373     ``0.``702 HDI            32 ``<language>`\
`#> ``5``       ``0.``95    ``0.``373     ``0.``702 HDI            32 ``<language>`\
`#> ``6``       ``0.``95    ``0.``373     ``0.``702 HDI            32 ``<language>`\
\
`# within-subjects design`\
[`oneway_anova`](https://www.indrapatil.com/statsExpressions/reference/oneway_anova.md)`(`\
`  data ``=`` ``bugs_long``,`\
`  x ``=`` ``condition``,`\
`  y ``=`` ``desire``,`\
`  subject.id ``=`` ``subject``,`\
`  paired ``=`` ``TRUE``,`\
`  type ``=`` ``"bayes"`\
`)`\
`#> ``# A tibble: 8 × 19`\
`#>   ``term``              ``pd`` ``prior.distribution`` ``prior.location`` ``prior.scale`` ``effect`\
`#>   ``<chr>``          ``<dbl>`` ``<chr>``                       ``<dbl>``       ``<dbl>`` ``<chr>`` `\
`#> ``1`` mu             1     cauchy                          ``0``       ``0.``707 fixed `\
`#> ``2`` condition-HDHF 1     cauchy                          ``0``       ``0.``707 fixed `\
`#> ``3`` condition-HDLF ``0.``868 cauchy                          ``0``       ``0.``707 fixed `\
`#> ``4`` condition-LDHF ``0.``994 cauchy                          ``0``       ``0.``707 fixed `\
`#> ``5`` condition-LDLF 1     cauchy                          ``0``       ``0.``707 fixed `\
`#> ``6`` sig2           1     cauchy                          ``0``       1     fixed `\
`#> ``7`` g_condition    1     cauchy                          ``0``       1     fixed `\
`#> ``8`` g_.rowid       1     cauchy                          ``0``       1     fixed `\
`#>          ``bf10`` ``method``                          ``log_e_bf10`` ``effectsize``        `\
`#>         ``<dbl>`` ``<chr>``                                ``<dbl>`` ``<chr>``             `\
`#> ``1`` ``1``36``0``982``789``. Bayes factors for linear models       21.0 Bayesian R-squared`\
`#> ``2`` ``1``36``0``982``789``. Bayes factors for linear models       21.0 Bayesian R-squared`\
`#> ``3`` ``1``36``0``982``789``. Bayes factors for linear models       21.0 Bayesian R-squared`\
`#> ``4`` ``1``36``0``982``789``. Bayes factors for linear models       21.0 Bayesian R-squared`\
`#> ``5`` ``1``36``0``982``789``. Bayes factors for linear models       21.0 Bayesian R-squared`\
`#> ``6`` ``1``36``0``982``789``. Bayes factors for linear models       21.0 Bayesian R-squared`\
`#> ``7`` ``1``36``0``982``789``. Bayes factors for linear models       21.0 Bayesian R-squared`\
`#> ``8`` ``1``36``0``982``789``. Bayes factors for linear models       21.0 Bayesian R-squared`\
`#>   ``estimate`` ``std.dev`` ``conf.level`` ``conf.low`` ``conf.high`` ``conf.method`` ``component``   ``n.obs`\
`#>      ``<dbl>``   ``<dbl>``      ``<dbl>``    ``<dbl>``     ``<dbl>`` ``<chr>``       ``<chr>``       ``<int>`\
`#> ``1``    ``0.``531  ``0.0``32``3``       ``0.``95    ``0.``461     ``0.``587 HDI         conditional    88`\
`#> ``2``    ``0.``531  ``0.0``32``3``       ``0.``95    ``0.``461     ``0.``587 HDI         conditional    88`\
`#> ``3``    ``0.``531  ``0.0``32``3``       ``0.``95    ``0.``461     ``0.``587 HDI         conditional    88`\
`#> ``4``    ``0.``531  ``0.0``32``3``       ``0.``95    ``0.``461     ``0.``587 HDI         conditional    88`\
`#> ``5``    ``0.``531  ``0.0``32``3``       ``0.``95    ``0.``461     ``0.``587 HDI         conditional    88`\
`#> ``6``    ``0.``531  ``0.0``32``3``       ``0.``95    ``0.``461     ``0.``587 HDI         conditional    88`\
`#> ``7``    ``0.``531  ``0.0``32``3``       ``0.``95    ``0.``461     ``0.``587 HDI         conditional    88`\
`#> ``8``    ``0.``531  ``0.0``32``3``       ``0.``95    ``0.``461     ``0.``587 HDI         conditional    88`\
`#>   ``expression`\
`#>   ``<list>``    `\
`#> ``1`` ``<language>`\
`#> ``2`` ``<language>`\
`#> ``3`` ``<language>`\
`#> ``4`` ``<language>`\
`#> ``5`` ``<language>`\
`#> ``6`` ``<language>`\
`#> ``7`` ``<language>`\
`#> ``8`` ``<language>`

## Contingency table analysis output

\
`#### -------------------- association test ------------------------ ####`\
\
`# ------------------------ frequentist ---------------------------------`\
\
`# unpaired`\
\
[`set.seed`](https://rdrr.io/r/base/Random.html)`(``123``)`\
[`contingency_table`](https://www.indrapatil.com/statsExpressions/reference/contingency_table.md)`(`\
`  data ``=`` ``mtcars``,`\
`  x ``=`` ``am``,`\
`  y ``=`` ``vs``,`\
`  paired ``=`` ``FALSE`\
`)`\
`#> ``# A tibble: 1 × 13`\
`#>   ``statistic``    ``df`` ``p.value`` ``method``                     ``effectsize``        ``estimate`\
`#>       ``<dbl>`` ``<int>``   ``<dbl>`` ``<chr>``                      ``<chr>``                ``<dbl>`\
`#> ``1``     ``0.``907     1   ``0.``341 Pearson's Chi-squared test Cramer's V (adj.)        ``0`\
`#>   ``conf.level`` ``conf.low`` ``conf.high`` ``conf.method`` ``conf.distribution`` ``n.obs`` ``expression`\
`#>        ``<dbl>``    ``<dbl>``     ``<dbl>`` ``<chr>``       ``<chr>``             ``<int>`` ``<list>``    `\
`#> ``1``       ``0.``95        ``0``     ``0.``490 ncp         chisq                32 ``<language>`\
\
`# paired`\
\
`paired_data`` ``<-`` ``dplyr``::`[`tibble`](https://tibble.tidyverse.org/reference/tibble.html)`(`\
`  response_before ``=`` `[`factor`](https://rdrr.io/r/base/factor.html)`(`[`c`](https://rdrr.io/r/base/c.html)`(``"no"``, ``"yes"``, ``"no"``, ``"yes"``)``)``,`\
`  response_after ``=`` `[`factor`](https://rdrr.io/r/base/factor.html)`(`[`c`](https://rdrr.io/r/base/c.html)`(``"no"``, ``"no"``, ``"yes"``, ``"yes"``)``)``,`\
`  Freq ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``65L``, ``25L``, ``5L``, ``5L``)`\
`)`\
\
[`set.seed`](https://rdrr.io/r/base/Random.html)`(``123``)`\
[`contingency_table`](https://www.indrapatil.com/statsExpressions/reference/contingency_table.md)`(`\
`  data ``=`` ``paired_data``,`\
`  x ``=`` ``response_before``,`\
`  y ``=`` ``response_after``,`\
`  paired ``=`` ``TRUE``,`\
`  counts ``=`` ``Freq`\
`)`\
`#> ``# A tibble: 1 × 12`\
`#>   ``statistic``    ``df``  ``p.value`` ``method``                     ``effectsize`` ``estimate`\
`#>       ``<dbl>`` ``<dbl>``    ``<dbl>`` ``<chr>``                      ``<chr>``         ``<dbl>`\
`#> ``1``      13.3     1 ``0.000``261`` McNemar's Chi-squared test Cohen's g     ``0.``333`\
`#>   ``conf.level`` ``conf.low`` ``conf.high`` ``conf.method`` ``n.obs`` ``expression`\
`#>        ``<dbl>``    ``<dbl>``     ``<dbl>`` ``<chr>``       ``<int>`` ``<list>``    `\
`#> ``1``       ``0.``95    ``0.``164     ``0.``427 binomial      100 ``<language>`\
\
`# ------------------------ Bayesian -------------------------------------`\
\
`# unpaired`\
\
[`set.seed`](https://rdrr.io/r/base/Random.html)`(``123``)`\
[`contingency_table`](https://www.indrapatil.com/statsExpressions/reference/contingency_table.md)`(`\
`  data ``=`` ``mtcars``,`\
`  x ``=`` ``am``,`\
`  y ``=`` ``vs``,`\
`  paired ``=`` ``FALSE``,`\
`  type ``=`` ``"bayes"`\
`)`\
`#> ``# A tibble: 1 × 15`\
`#>   ``term``  ``conf.level`` ``effectsize`` ``estimate`` ``conf.low`` ``conf.high`\
`#>   ``<chr>``      ``<dbl>`` ``<chr>``         ``<dbl>``    ``<dbl>``     ``<dbl>`\
`#> ``1`` Ratio       ``0.``95 Cramers_v         ``0``        ``0``     ``0.``421`\
`#>   ``prior.distribution``      ``prior.location`` ``prior.scale``  ``bf10`\
`#>   ``<chr>``                            ``<dbl>``       ``<dbl>`` ``<dbl>`\
`#> ``1`` independent multinomial              ``0``           1 ``0.``643`\
`#>   ``method``                              ``conf.method`` ``log_e_bf10`` ``n.obs`` ``expression`\
`#>   ``<chr>``                               ``<chr>``            ``<dbl>`` ``<int>`` ``<list>``    `\
`#> ``1`` Bayesian contingency table analysis ETI             -``0.``442``    32 ``<language>`\
\
`# paired`\
\
[`set.seed`](https://rdrr.io/r/base/Random.html)`(``123``)`\
[`contingency_table`](https://www.indrapatil.com/statsExpressions/reference/contingency_table.md)`(`\
`  data ``=`` ``paired_data``,`\
`  x ``=`` ``response_before``,`\
`  y ``=`` ``response_after``,`\
`  paired ``=`` ``TRUE``,`\
`  counts ``=`` ``Freq``,`\
`  type ``=`` ``"bayes"`\
`)`\
`#> ``# A tibble: 1 × 15`\
`#>   ``term``  ``conf.level`` ``effectsize`` ``estimate`` ``conf.low`` ``conf.high`\
`#>   ``<chr>``      ``<dbl>`` ``<chr>``         ``<dbl>``    ``<dbl>``     ``<dbl>`\
`#> ``1`` Ratio       ``0.``95 Cramers_v     ``0.``111        ``0``     ``0.``340`\
`#>   ``prior.distribution``      ``prior.location`` ``prior.scale``  ``bf10`\
`#>   ``<chr>``                            ``<dbl>``       ``<dbl>`` ``<dbl>`\
`#> ``1`` independent multinomial              ``0``           1 ``0.``461`\
`#>   ``method``                              ``conf.method`` ``log_e_bf10`` ``n.obs`` ``expression`\
`#>   ``<chr>``                               ``<chr>``            ``<dbl>`` ``<int>`` ``<list>``    `\
`#> ``1`` Bayesian contingency table analysis ETI             -``0.``775``   100 ``<language>`\
\
`#### -------------------- goodness-of-fit test -------------------- ####`\
\
`# ------------------------ frequentist ---------------------------------`\
\
[`set.seed`](https://rdrr.io/r/base/Random.html)`(``123``)`\
[`contingency_table`](https://www.indrapatil.com/statsExpressions/reference/contingency_table.md)`(`\
`  data ``=`` `[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html)`(``HairEyeColor``)``,`\
`  x ``=`` ``Eye``,`\
`  counts ``=`` ``Freq`\
`)`\
`#> ``# A tibble: 1 × 13`\
`#>   ``statistic``    ``df``  ``p.value`` ``method``                                   ``effectsize`` `\
`#>       ``<dbl>`` ``<dbl>``    ``<dbl>`` ``<chr>``                                    ``<chr>``      `\
`#> ``1``      133.     3 9.65``e``-29`` Chi-squared test for given probabilities Pearson's C`\
`#>   ``estimate`` ``conf.level`` ``conf.low`` ``conf.high`` ``conf.method`` ``conf.distribution`` ``n.obs`\
`#>      ``<dbl>``      ``<dbl>``    ``<dbl>``     ``<dbl>`` ``<chr>``       ``<chr>``             ``<int>`\
`#> ``1``    ``0.``429       ``0.``95    ``0.``364     ``0.``483 ncp         chisq               592`\
`#>   ``expression`\
`#>   ``<list>``    `\
`#> ``1`` ``<language>`\
\
`# ------------------------ Bayesian -------------------------------------`\
\
[`set.seed`](https://rdrr.io/r/base/Random.html)`(``123``)`\
[`contingency_table`](https://www.indrapatil.com/statsExpressions/reference/contingency_table.md)`(`\
`  data ``=`` `[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html)`(``HairEyeColor``)``,`\
`  x ``=`` ``Eye``,`\
`  counts ``=`` ``Freq``,`\
`  ratio ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``0.2``, ``0.2``, ``0.3``, ``0.3``)``,`\
`  type ``=`` ``"bayes"`\
`)`\
`#> ``# A tibble: 1 × 4`\
`#>      ``bf10`` ``prior.scale`` ``method``                                      ``expression`\
`#>     ``<dbl>``       ``<dbl>`` ``<chr>``                                       ``<list>``    `\
`#> ``1`` 4.17``e``55           1 Bayesian one-way contingency table analysis ``<language>`

## Correlation analysis output

\
`# for reproducibility`\
[`set.seed`](https://rdrr.io/r/base/Random.html)`(``123``)`\
\
`# ----------------------- parametric -----------------------`\
\
[`corr_test`](https://www.indrapatil.com/statsExpressions/reference/corr_test.md)`(``mtcars``, ``wt``, ``mpg``, type ``=`` ``"parametric"``)`\
`#> ``# A tibble: 1 × 14`\
`#>   ``parameter1`` ``parameter2`` ``effectsize``          ``estimate`` ``conf.level`` ``conf.low`\
`#>   ``<chr>``      ``<chr>``      ``<chr>``                  ``<dbl>``      ``<dbl>``    ``<dbl>`\
`#> ``1`` wt         mpg        Pearson correlation   -``0.``868``       ``0.``95   -``0.``934`\
`#>   ``conf.high`` ``statistic`` ``df.error``  ``p.value`` ``method``              ``n.obs`` ``conf.method`\
`#>       ``<dbl>``     ``<dbl>``    ``<int>``    ``<dbl>`` ``<chr>``               ``<int>`` ``<chr>``      `\
`#> ``1``    -``0.``744``     -``9.56``       30 1.29``e``-10`` Pearson correlation    32 normal     `\
`#>   ``expression`\
`#>   ``<list>``    `\
`#> ``1`` ``<language>`\
\
`# ----------------------- non-parametric -------------------`\
\
[`corr_test`](https://www.indrapatil.com/statsExpressions/reference/corr_test.md)`(``mtcars``, ``wt``, ``mpg``, type ``=`` ``"nonparametric"``)`\
`#> ``# A tibble: 1 × 13`\
`#>   ``parameter1`` ``parameter2`` ``effectsize``           ``estimate`` ``conf.level`` ``conf.low`\
`#>   ``<chr>``      ``<chr>``      ``<chr>``                   ``<dbl>``      ``<dbl>``    ``<dbl>`\
`#> ``1`` wt         mpg        Spearman correlation   -``0.``886``       ``0.``95   -``0.``945`\
`#>   ``conf.high`` ``statistic``  ``p.value`` ``method``               ``n.obs`` ``conf.method`` ``expression`\
`#>       ``<dbl>``     ``<dbl>``    ``<dbl>`` ``<chr>``                ``<int>`` ``<chr>``       ``<list>``    `\
`#> ``1``    -``0.``774``    ``10``2``92``. 1.49``e``-11`` Spearman correlation    32 normal      ``<language>`\
\
`# ----------------------- robust ---------------------------`\
\
[`corr_test`](https://www.indrapatil.com/statsExpressions/reference/corr_test.md)`(``mtcars``, ``wt``, ``mpg``, type ``=`` ``"robust"``)`\
`#> ``# A tibble: 1 × 14`\
`#>   ``parameter1`` ``parameter2`` ``effectsize``                     ``estimate`` ``conf.level`\
`#>   ``<chr>``      ``<chr>``      ``<chr>``                             ``<dbl>``      ``<dbl>`\
`#> ``1`` wt         mpg        Winsorized Pearson correlation   -``0.``864``       ``0.``95`\
`#>   ``conf.low`` ``conf.high`` ``statistic`` ``df.error``  ``p.value`` ``method``                        `\
`#>      ``<dbl>``     ``<dbl>``     ``<dbl>``    ``<int>``    ``<dbl>`` ``<chr>``                         `\
`#> ``1``   -``0.``932``    -``0.``738``     -``9.41``       30 1.84``e``-10`` Winsorized Pearson correlation`\
`#>   ``n.obs`` ``conf.method`` ``expression`\
`#>   ``<int>`` ``<chr>``       ``<list>``    `\
`#> ``1``    32 normal      ``<language>`\
\
`# ----------------------- Bayesian -------------------------`\
\
[`corr_test`](https://www.indrapatil.com/statsExpressions/reference/corr_test.md)`(``mtcars``, ``wt``, ``mpg``, type ``=`` ``"bayes"``)`\
`#> ``# A tibble: 1 × 17`\
`#>   ``parameter1`` ``parameter2`` ``effectsize``                   ``estimate`` ``conf.level`\
`#>   ``<chr>``      ``<chr>``      ``<chr>``                           ``<dbl>``      ``<dbl>`\
`#> ``1`` wt         mpg        Bayesian Pearson correlation   -``0.``843``       ``0.``95`\
`#>   ``conf.low`` ``conf.high``    ``pd`` ``rope.percentage`` ``prior.distribution`` ``prior.location`\
`#>      ``<dbl>``     ``<dbl>`` ``<dbl>``           ``<dbl>`` ``<chr>``                       ``<dbl>`\
`#> ``1``   -``0.``934``    -``0.``734``     1               ``0`` beta                         1.41`\
`#>   ``prior.scale``      ``bf10`` ``method``                       ``n.obs`` ``conf.method`\
`#>         ``<dbl>``     ``<dbl>`` ``<chr>``                        ``<int>`` ``<chr>``      `\
`#> ``1``        1.41 56``2``23``033``. Bayesian Pearson correlation    32 HDI        `\
`#>   ``expression`\
`#>   ``<list>``    `\
`#> ``1`` ``<language>`

## Random-effects meta-analysis output

\
`` # let's use `mag` dataset from `{metaplus}` ``\
[`data`](https://rdrr.io/r/utils/data.html)`(``mag``, package ``=`` ``"metaplus"``)`\
`dat`` ``<-`` ``dplyr``::`[`rename`](https://dplyr.tidyverse.org/reference/rename.html)`(``mag``, estimate ``=`` ``yi``, std.error ``=`` ``sei``)`\
\
`# ----------------------- parametric ---------------------------------------`\
\
[`set.seed`](https://rdrr.io/r/base/Random.html)`(``123``)`\
[`meta_analysis`](https://www.indrapatil.com/statsExpressions/reference/meta_analysis.md)`(``dat``)`\
`#> ``# A tibble: 1 × 14`\
`#>   ``term``    ``effectsize``                     ``estimate`` ``std.error`` ``conf.level`` ``conf.low`\
`#>   ``<chr>``   ``<chr>``                             ``<dbl>``     ``<dbl>``      ``<dbl>``    ``<dbl>`\
`#> ``1`` Overall meta-analytic summary estimate   -``0.``767``     ``0.``212       ``0.``95    -``1.18`\
`#>   ``conf.high`` ``statistic``  ``p.value`` ``weight`` ``method``                        ``conf.method`\
`#>       ``<dbl>``     ``<dbl>``    ``<dbl>``  ``<dbl>`` ``<chr>``                         ``<chr>``      `\
`#> ``1``    -``0.``351``     -``3.62`` ``0.000``295``     ``NA`` Meta-analysis using 'metafor' Wald       `\
`#>   ``n.obs`` ``expression`\
`#>   ``<int>`` ``<list>``    `\
`#> ``1``    16 ``<language>`\
\
`# ----------------------- robust --------------------------------------------`\
\
[`set.seed`](https://rdrr.io/r/base/Random.html)`(``123``)`\
[`meta_analysis`](https://www.indrapatil.com/statsExpressions/reference/meta_analysis.md)`(``dat``, type ``=`` ``"robust"``, random ``=`` ``"normal"``)`\
`#> ``# A tibble: 1 × 14`\
`#>   ``term``    ``effectsize``                     ``estimate`` ``std.error`` ``conf.low`` ``conf.high`\
`#>   ``<chr>``   ``<chr>``                             ``<dbl>``     ``<dbl>``    ``<dbl>``     ``<dbl>`\
`#> ``1`` Overall meta-analytic summary estimate   -``0.``746``     ``0.``234    -``1.26``    -``0.``343`\
`#>   ``statistic``  ``p.value`` ``weight`` ``conf.level`` ``method``                               `\
`#>       ``<dbl>``    ``<dbl>``  ``<dbl>``      ``<dbl>`` ``<chr>``                                `\
`#> ``1``     -``3.20`` ``0.000``501``     ``NA``       ``0.``95 Robust meta-analysis using 'metaplus'`\
`#>   ``conf.method`` ``n.obs`` ``expression`\
`#>   ``<chr>``       ``<int>`` ``<list>``    `\
`#> ``1`` Wald           16 ``<language>`\
\
`# ----------------------- Bayesian ------------------------------------------`\
\
`# suppress warnings about divergent transitions after warmup`\
[`set.seed`](https://rdrr.io/r/base/Random.html)`(``123``)`\
[`suppressWarnings`](https://rdrr.io/r/base/warning.html)`(`[`meta_analysis`](https://www.indrapatil.com/statsExpressions/reference/meta_analysis.md)`(``dat``, type ``=`` ``"bayes"``)``)`\
`#> ``# A tibble: 2 × 20`\
`#>   ``term``    ``effectsize``                       ``estimate`` ``std.error`` ``conf.level`\
`#>   ``<chr>``   ``<chr>``                               ``<dbl>``     ``<dbl>``      ``<dbl>`\
`#> ``1`` Overall meta-analytic posterior estimate   -``0.``643``     ``0.``220       ``0.``95`\
`#> ``2`` tau     meta-analytic posterior estimate    ``0.``484     ``0.``182       ``0.``95`\
`#>   ``conf.low`` ``conf.high`` ``weight``  ``bf10``  ``rhat``   ``ess`` ``component`` ``prior.distribution`\
`#>      ``<dbl>``     ``<dbl>``  ``<dbl>`` ``<dbl>`` ``<dbl>`` ``<dbl>`` ``<chr>``     ``<chr>``             `\
`#> ``1``   -``1.11``     -``0.``242``     ``NA``  53.0     1 ``3``50``7``  meta      Student's t       `\
`#> ``2``    ``0.``205     ``0.``909     ``NA``  53.0     1 ``3``46``0``. meta      Inverse gamma     `\
`#>   ``prior.location`` ``prior.scale`` ``method``                                 ``conf.method`\
`#>            ``<dbl>``       ``<dbl>`` ``<chr>``                                  ``<chr>``      `\
`#> ``1``              ``0``       ``0.``707 Bayesian meta-analysis using 'metaBMA' ETI        `\
`#> ``2``              1       ``0.``15  Bayesian meta-analysis using 'metaBMA' ETI        `\
`#>   ``log_e_bf10`` ``n.obs`` ``expression`\
`#>        ``<dbl>`` ``<int>`` ``<list>``    `\
`#> ``1``       3.97    16 ``<language>`\
`#> ``2``       3.97    16 ``<language>`

## Centrality description

\
`# for reproducibility`\
[`set.seed`](https://rdrr.io/r/base/Random.html)`(``123``)`\
\
`# ----------------------- parametric -----------------------`\
\
[`centrality_description`](https://www.indrapatil.com/statsExpressions/reference/centrality_description.md)`(``iris``, ``Species``, ``Sepal.Length``, type ``=`` ``"parametric"``)`\
`#> ``# A tibble: 3 × 14`\
`#>   ``Species``    ``Sepal.Length`` ``std.dev``   ``iqr`` ``conf.low`` ``conf.high``   ``min``   ``max`` ``skewness`\
`#>   ``<fct>``             ``<dbl>``   ``<dbl>`` ``<dbl>``    ``<dbl>``     ``<dbl>`` ``<dbl>`` ``<dbl>``    ``<dbl>`\
`#> ``1`` setosa             5.01   ``0.``352 ``0.``400     4.90      5.10   4.3   5.8    ``0.``120`\
`#> ``2`` versicolor         5.94   ``0.``516 ``0.``7       5.80      6.07   4.9   7      ``0.``105`\
`#> ``3`` virginica          6.59   ``0.``636 ``0.``750     6.39      6.79   4.9   7.9    ``0.``118`\
`#>   ``kurtosis`` ``n.obs`` ``missing.obs`` ``expression`` ``n.expression``          `\
`#>      ``<dbl>`` ``<int>``       ``<int>`` ``<list>``     ``<chr>``                 `\
`#> ``1``  -``0.``253``     50           ``0`` ``<language>`` ``"``setosa\n(n = 50)``"``    `\
`#> ``2``  -``0.``533``     50           ``0`` ``<language>`` ``"``versicolor\n(n = 50)``"`\
`#> ``3``   ``0.0``32``9``    50           ``0`` ``<language>`` ``"``virginica\n(n = 50)``"`\
\
`# ----------------------- non-parametric -------------------`\
\
[`centrality_description`](https://www.indrapatil.com/statsExpressions/reference/centrality_description.md)`(``mtcars``, ``am``, ``wt``, type ``=`` ``"nonparametric"``)`\
`#> ``# A tibble: 2 × 14`\
`#>      ``am``    ``wt``   ``mad``   ``iqr`` ``conf.low`` ``conf.high``   ``min``   ``max`` ``skewness`` ``kurtosis`` ``n.obs`\
`#>   ``<dbl>`` ``<dbl>`` ``<dbl>`` ``<dbl>``    ``<dbl>``     ``<dbl>`` ``<dbl>`` ``<dbl>``    ``<dbl>``    ``<dbl>`` ``<int>`\
`#> ``1``     ``0``  3.52 ``0.``452 ``0.``41      3.44      3.84  2.46  5.42    1.15     1.06     19`\
`#> ``2``     1  2.32 ``0.``682 ``0.``942     1.94      2.78  1.51  3.57    ``0.``269   -``0.``654``    13`\
`#>   ``missing.obs`` ``expression`` ``n.expression`` `\
`#>         ``<int>`` ``<list>``     ``<chr>``        `\
`#> ``1``           ``0`` ``<language>`` ``"``0\n(n = 19)``"`\
`#> ``2``           ``0`` ``<language>`` ``"``1\n(n = 13)``"`\
\
`# ----------------------- robust ---------------------------`\
\
[`centrality_description`](https://www.indrapatil.com/statsExpressions/reference/centrality_description.md)`(``ToothGrowth``, ``supp``, ``len``, type ``=`` ``"robust"``)`\
`#> ``# A tibble: 2 × 14`\
`#>   ``supp``    ``len`` ``std.dev``   ``iqr`` ``conf.low`` ``conf.high``   ``min``   ``max`` ``skewness`` ``kurtosis`\
`#>   ``<fct>`` ``<dbl>``   ``<dbl>`` ``<dbl>``    ``<dbl>``     ``<dbl>`` ``<dbl>`` ``<dbl>``    ``<dbl>``    ``<dbl>`\
`#> ``1`` OJ     21.7    6.61  10.9     17.5      23.5   8.2  30.9   -``0.``580``   -``0.``831`\
`#> ``2`` VC     16.6    8.27  12.5     13.6      19.6   4.2  33.9    ``0.``306   -``0.``700`\
`#>   ``n.obs`` ``missing.obs`` ``expression`` ``n.expression``  `\
`#>   ``<int>``       ``<int>`` ``<list>``     ``<chr>``         `\
`#> ``1``    30           ``0`` ``<language>`` ``"``OJ\n(n = 30)``"`\
`#> ``2``    30           ``0`` ``<language>`` ``"``VC\n(n = 30)``"`\
\
`# ----------------------- Bayesian -------------------------`\
\
[`centrality_description`](https://www.indrapatil.com/statsExpressions/reference/centrality_description.md)`(``sleep``, ``group``, ``extra``, type ``=`` ``"bayes"``)`\
`#> ``# A tibble: 2 × 13`\
`#>   ``group``  ``extra``   ``iqr`` ``conf.low`` ``conf.high``   ``min``   ``max`` ``skewness`` ``kurtosis`` ``n.obs`\
`#>   ``<fct>``  ``<dbl>`` ``<dbl>``    ``<dbl>``     ``<dbl>`` ``<dbl>`` ``<dbl>``    ``<dbl>``    ``<dbl>`` ``<int>`\
`#> ``1`` 1     ``0.0``57``9``  2.8   -``1.39``        3.66  -``1.6``   3.7    ``0.``581   -``0.``630``    10`\
`#> ``2`` 2     ``0.``973   3.82   ``0.0``72``8``      4.88  -``0.``1``   5.5    ``0.``386   -``1.42``     10`\
`#>   ``missing.obs`` ``expression`` ``n.expression`` `\
`#>         ``<int>`` ``<list>``     ``<chr>``        `\
`#> ``1``           ``0`` ``<language>`` ``"``1\n(n = 10)``"`\
`#> ``2``           ``0`` ``<language>`` ``"``2\n(n = 10)``"`

## Pairwise comparisons for one-way design

\
`# for reproducibility`\
[`set.seed`](https://rdrr.io/r/base/Random.html)`(``123``)`\
\
`#------------------- between-subjects design ----------------------------`\
\
`# parametric`\
`` # if `var.equal = TRUE`, then Student's t-test will be run ``\
[`pairwise_comparisons`](https://www.indrapatil.com/statsExpressions/reference/pairwise_comparisons.md)`(`\
`  data ``=`` ``mtcars``,`\
`  x ``=`` ``cyl``,`\
`  y ``=`` ``wt``,`\
`  type ``=`` ``"parametric"``,`\
`  var.equal ``=`` ``TRUE``,`\
`  paired ``=`` ``FALSE``,`\
`  p.adjust.method ``=`` ``"none"`\
`)`\
`#> ``# A tibble: 3 × 6`\
`#>   ``group1`` ``group2``     ``p.value`` ``p.adjust.method`` ``test``        ``expression`\
`#>   ``<chr>``  ``<chr>``        ``<dbl>`` ``<chr>``           ``<chr>``       ``<list>``    `\
`#> ``1`` 4      6      ``0.0``10``6``      None            Student's t ``<language>`\
`#> ``2`` 4      8      ``0.000``000``207 None            Student's t ``<language>`\
`#> ``3`` 6      8      ``0.00``5``16``     None            Student's t ``<language>`\
\
`` # if `var.equal = FALSE`, then Games-Howell test will be run ``\
[`pairwise_comparisons`](https://www.indrapatil.com/statsExpressions/reference/pairwise_comparisons.md)`(`\
`  data ``=`` ``mtcars``,`\
`  x ``=`` ``cyl``,`\
`  y ``=`` ``wt``,`\
`  type ``=`` ``"parametric"``,`\
`  var.equal ``=`` ``FALSE``,`\
`  paired ``=`` ``FALSE``,`\
`  p.adjust.method ``=`` ``"bonferroni"`\
`)`\
`#> ``# A tibble: 3 × 9`\
`#>   ``group1`` ``group2`` ``statistic``   ``p.value`` ``alternative`` ``distribution`` ``p.adjust.method`\
`#>   ``<chr>``  ``<chr>``      ``<dbl>``     ``<dbl>`` ``<chr>``       ``<chr>``        ``<chr>``          `\
`#> ``1`` 4      6           5.39 ``0.0``12``5``    two.sided   q            Bonferroni     `\
`#> ``2`` 4      8           9.11 ``0.000``0``12``4 two.sided   q            Bonferroni     `\
`#> ``3`` 6      8           5.12 ``0.0``14``8``    two.sided   q            Bonferroni     `\
`#>   ``test``         ``expression`\
`#>   ``<chr>``        ``<list>``    `\
`#> ``1`` Games-Howell ``<language>`\
`#> ``2`` Games-Howell ``<language>`\
`#> ``3`` Games-Howell ``<language>`\
\
`# non-parametric (Dunn test)`\
[`pairwise_comparisons`](https://www.indrapatil.com/statsExpressions/reference/pairwise_comparisons.md)`(`\
`  data ``=`` ``mtcars``,`\
`  x ``=`` ``cyl``,`\
`  y ``=`` ``wt``,`\
`  type ``=`` ``"nonparametric"``,`\
`  paired ``=`` ``FALSE``,`\
`  p.adjust.method ``=`` ``"none"`\
`)`\
`#> ``# A tibble: 3 × 9`\
`#>   ``group1`` ``group2`` ``statistic``    ``p.value`` ``alternative`` ``distribution`` ``p.adjust.method`\
`#>   ``<chr>``  ``<chr>``      ``<dbl>``      ``<dbl>`` ``<chr>``       ``<chr>``        ``<chr>``          `\
`#> ``1`` 4      6           1.84 ``0.0``66``3``     two.sided   z            None           `\
`#> ``2`` 4      8           4.76 ``0.000``00``1``98 two.sided   z            None           `\
`#> ``3`` 6      8           2.22 ``0.0``26``3``     two.sided   z            None           `\
`#>   ``test``  ``expression`\
`#>   ``<chr>`` ``<list>``    `\
`#> ``1`` Dunn  ``<language>`\
`#> ``2`` Dunn  ``<language>`\
`#> ``3`` Dunn  ``<language>`\
\
`# robust (Yuen's trimmed means t-test)`\
[`pairwise_comparisons`](https://www.indrapatil.com/statsExpressions/reference/pairwise_comparisons.md)`(`\
`  data ``=`` ``mtcars``,`\
`  x ``=`` ``cyl``,`\
`  y ``=`` ``wt``,`\
`  type ``=`` ``"robust"``,`\
`  paired ``=`` ``FALSE``,`\
`  p.adjust.method ``=`` ``"fdr"`\
`)`\
`#> ``# A tibble: 3 × 10`\
`#>   ``group1`` ``group2`` ``estimate`` ``conf.level`` ``conf.low`` ``conf.high``  ``p.value`` ``p.adjust.method`\
`#>   ``<chr>``  ``<chr>``     ``<dbl>``      ``<dbl>``    ``<dbl>``     ``<dbl>``    ``<dbl>`` ``<chr>``          `\
`#> ``1`` 4      6        -``0.``909``       ``0.``95    -``1.64``    -``0.``173`` ``0.00``8``72``  FDR            `\
`#> ``2`` 4      8        -``1.62``        ``0.``95    -``2.50``    -``0.``746`` ``0.000``549`` FDR            `\
`#> ``3`` 6      8        -``0.``713``       ``0.``95    -``1.58``     ``0.``155 ``0.0``43``8``   FDR            `\
`#>   ``test``                 ``expression`\
`#>   ``<chr>``                ``<list>``    `\
`#> ``1`` Yuen's trimmed means ``<language>`\
`#> ``2`` Yuen's trimmed means ``<language>`\
`#> ``3`` Yuen's trimmed means ``<language>`\
\
`# Bayes Factor (Student's t-test)`\
[`pairwise_comparisons`](https://www.indrapatil.com/statsExpressions/reference/pairwise_comparisons.md)`(`\
`  data ``=`` ``mtcars``,`\
`  x ``=`` ``cyl``,`\
`  y ``=`` ``wt``,`\
`  type ``=`` ``"bayes"``,`\
`  paired ``=`` ``FALSE`\
`)`\
`#> ``# A tibble: 3 × 18`\
`#>   ``group1`` ``group2`` ``term``       ``effectsize``      ``estimate`` ``conf.level`` ``conf.low`\
`#>   ``<chr>``  ``<chr>``  ``<chr>``      ``<chr>``              ``<dbl>``      ``<dbl>``    ``<dbl>`\
`#> ``1`` 4      6      Difference Bayesian t-test   -``0.``686``       ``0.``95    -``1.22`\
`#> ``2`` 4      8      Difference Bayesian t-test   -``1.63``        ``0.``95    -``2.21`\
`#> ``3`` 6      8      Difference Bayesian t-test   -``0.``715``       ``0.``95    -``1.36`\
`#>   ``conf.high``    ``pd`` ``prior.distribution`` ``prior.location`` ``prior.scale``    ``bf10`\
`#>       ``<dbl>`` ``<dbl>`` ``<chr>``                       ``<dbl>``       ``<dbl>``   ``<dbl>`\
`#> ``1``   -``0.``157``  ``0.``992 cauchy                          ``0``       ``0.``707   11.4 `\
`#> ``2``   -``1.01``   1     cauchy                          ``0``       ``0.``707 ``5``22``2``.  `\
`#> ``3``   -``0.0``91``0`` ``0.``987 cauchy                          ``0``       ``0.``707    5.36`\
`#>   ``conf.method`` ``log_e_bf10`` ``n.obs`` ``expression`` ``test``       `\
`#>   ``<chr>``            ``<dbl>`` ``<int>`` ``<list>``     ``<chr>``      `\
`#> ``1`` ETI               2.44    18 ``<language>`` Student's t`\
`#> ``2`` ETI               8.56    25 ``<language>`` Student's t`\
`#> ``3`` ETI               1.68    21 ``<language>`` Student's t`\
\
`#------------------- within-subjects design ----------------------------`\
\
`# parametric (Student's t-test)`\
[`pairwise_comparisons`](https://www.indrapatil.com/statsExpressions/reference/pairwise_comparisons.md)`(`\
`  data ``=`` ``bugs_long``,`\
`  x ``=`` ``condition``,`\
`  y ``=`` ``desire``,`\
`  subject.id ``=`` ``subject``,`\
`  type ``=`` ``"parametric"``,`\
`  paired ``=`` ``TRUE``,`\
`  p.adjust.method ``=`` ``"BH"`\
`)`\
`#> ``# A tibble: 6 × 6`\
`#>   ``group1`` ``group2``  ``p.value`` ``p.adjust.method`` ``test``        ``expression`\
`#>   ``<chr>``  ``<chr>``     ``<dbl>`` ``<chr>``           ``<chr>``       ``<list>``    `\
`#> ``1`` HDHF   HDLF   1.06``e``- 3`` FDR             Student's t ``<language>`\
`#> ``2`` HDHF   LDHF   7.02``e``- 2`` FDR             Student's t ``<language>`\
`#> ``3`` HDHF   LDLF   3.95``e``-12`` FDR             Student's t ``<language>`\
`#> ``4`` HDLF   LDHF   6.74``e``- 2`` FDR             Student's t ``<language>`\
`#> ``5`` HDLF   LDLF   1.99``e``- 3`` FDR             Student's t ``<language>`\
`#> ``6`` LDHF   LDLF   6.66``e``- 9`` FDR             Student's t ``<language>`\
\
`# non-parametric (Durbin-Conover test)`\
[`pairwise_comparisons`](https://www.indrapatil.com/statsExpressions/reference/pairwise_comparisons.md)`(`\
`  data ``=`` ``bugs_long``,`\
`  x ``=`` ``condition``,`\
`  y ``=`` ``desire``,`\
`  subject.id ``=`` ``subject``,`\
`  type ``=`` ``"nonparametric"``,`\
`  paired ``=`` ``TRUE``,`\
`  p.adjust.method ``=`` ``"BY"`\
`)`\
`#> ``# A tibble: 6 × 9`\
`#>   ``group1`` ``group2`` ``statistic``  ``p.value`` ``alternative`` ``distribution`` ``p.adjust.method`\
`#>   ``<chr>``  ``<chr>``      ``<dbl>``    ``<dbl>`` ``<chr>``       ``<chr>``        ``<chr>``          `\
`#> ``1`` HDHF   HDLF        4.78 1.44``e``- 5`` two.sided   t            BY             `\
`#> ``2`` HDHF   LDHF        2.44 4.47``e``- 2`` two.sided   t            BY             `\
`#> ``3`` HDHF   LDLF        8.01 5.45``e``-13`` two.sided   t            BY             `\
`#> ``4`` HDLF   LDHF        2.34 4.96``e``- 2`` two.sided   t            BY             `\
`#> ``5`` HDLF   LDLF        3.23 5.05``e``- 3`` two.sided   t            BY             `\
`#> ``6`` LDHF   LDLF        5.57 4.64``e``- 7`` two.sided   t            BY             `\
`#>   ``test``           ``expression`\
`#>   ``<chr>``          ``<list>``    `\
`#> ``1`` Durbin-Conover ``<language>`\
`#> ``2`` Durbin-Conover ``<language>`\
`#> ``3`` Durbin-Conover ``<language>`\
`#> ``4`` Durbin-Conover ``<language>`\
`#> ``5`` Durbin-Conover ``<language>`\
`#> ``6`` Durbin-Conover ``<language>`\
\
`# robust (Yuen's trimmed means t-test)`\
[`pairwise_comparisons`](https://www.indrapatil.com/statsExpressions/reference/pairwise_comparisons.md)`(`\
`  data ``=`` ``bugs_long``,`\
`  x ``=`` ``condition``,`\
`  y ``=`` ``desire``,`\
`  subject.id ``=`` ``subject``,`\
`  type ``=`` ``"robust"``,`\
`  paired ``=`` ``TRUE``,`\
`  p.adjust.method ``=`` ``"hommel"`\
`)`\
`#> ``# A tibble: 6 × 11`\
`#>   ``group1`` ``group2`` ``estimate`` ``conf.level`` ``conf.low`` ``conf.high``     ``p.value``  ``p.crit`\
`#>   ``<chr>``  ``<chr>``     ``<dbl>``      ``<dbl>``    ``<dbl>``     ``<dbl>``       ``<dbl>``   ``<dbl>`\
`#> ``1`` HDHF   HDLF      1.03        ``0.``95   ``0.``140      1.92  ``0.00``9``99``     ``0.0``12``7`` `\
`#> ``2`` HDHF   LDHF      ``0.``454       ``0.``95  -``0.``104``      1.01  ``0.0``52``0``      ``0.0``25  `\
`#> ``3`` HDHF   LDLF      1.95        ``0.``95   1.09       2.82  ``0.000``000``564 ``0.00``8``51`\
`#> ``4`` HDLF   LDHF     -``0.``676``       ``0.``95  -``1.61``       ``0.``256 ``0.0``52``0``      ``0.0``5   `\
`#> ``5`` HDLF   LDLF      ``0.``889       ``0.``95   ``0.0``24``4``     1.75  ``0.0``20``3``      ``0.0``16``9`` `\
`#> ``6`` LDHF   LDLF      1.35        ``0.``95   ``0.``560      2.14  ``0.000``102``    ``0.0``10``2`` `\
`#>   ``p.adjust.method`` ``test``                 ``expression`\
`#>   ``<chr>``           ``<chr>``                ``<list>``    `\
`#> ``1`` Hommel          Yuen's trimmed means ``<language>`\
`#> ``2`` Hommel          Yuen's trimmed means ``<language>`\
`#> ``3`` Hommel          Yuen's trimmed means ``<language>`\
`#> ``4`` Hommel          Yuen's trimmed means ``<language>`\
`#> ``5`` Hommel          Yuen's trimmed means ``<language>`\
`#> ``6`` Hommel          Yuen's trimmed means ``<language>`\
\
`# Bayes Factor (Student's t-test)`\
[`pairwise_comparisons`](https://www.indrapatil.com/statsExpressions/reference/pairwise_comparisons.md)`(`\
`  data ``=`` ``bugs_long``,`\
`  x ``=`` ``condition``,`\
`  y ``=`` ``desire``,`\
`  subject.id ``=`` ``subject``,`\
`  type ``=`` ``"bayes"``,`\
`  paired ``=`` ``TRUE`\
`)`\
`#> ``# A tibble: 6 × 18`\
`#>   ``group1`` ``group2`` ``term``       ``effectsize``      ``estimate`` ``conf.level`` ``conf.low`\
`#>   ``<chr>``  ``<chr>``  ``<chr>``      ``<chr>``              ``<dbl>``      ``<dbl>``    ``<dbl>`\
`#> ``1`` HDHF   HDLF   Difference Bayesian t-test    1.10        ``0.``95   ``0.``491 `\
`#> ``2`` HDHF   LDHF   Difference Bayesian t-test    ``0.``455       ``0.``95  -``0.0``48``3`\
`#> ``3`` HDHF   LDLF   Difference Bayesian t-test    2.13        ``0.``95   1.62  `\
`#> ``4`` HDLF   LDHF   Difference Bayesian t-test   -``0.``661``       ``0.``95  -``1.32``  `\
`#> ``5`` HDLF   LDLF   Difference Bayesian t-test    ``0.``991       ``0.``95   ``0.``369 `\
`#> ``6`` LDHF   LDLF   Difference Bayesian t-test    1.65        ``0.``95   1.14  `\
`#>   ``conf.high``    ``pd`` ``prior.distribution`` ``prior.location`` ``prior.scale``     ``bf10`\
`#>       ``<dbl>`` ``<dbl>`` ``<chr>``                       ``<dbl>``       ``<dbl>``    ``<dbl>`\
`#> ``1``    1.76   1.000 cauchy                          ``0``       ``0.``707 4.16``e``+ 1`\
`#> ``2``    ``0.``955  ``0.``962 cauchy                          ``0``       ``0.``707 5.83``e``- 1`\
`#> ``3``    2.63   1     cauchy                          ``0``       ``0.``707 1.20``e``+10`\
`#> ``4``    ``0.0``31``5`` ``0.``97  cauchy                          ``0``       ``0.``707 6.98``e``- 1`\
`#> ``5``    1.57   ``0.``999 cauchy                          ``0``       ``0.``707 1.81``e``+ 1`\
`#> ``6``    2.15   1     cauchy                          ``0``       ``0.``707 4.81``e``+ 6`\
`#>   ``conf.method`` ``log_e_bf10`` ``n.obs`` ``expression`` ``test``       `\
`#>   ``<chr>``            ``<dbl>`` ``<int>`` ``<list>``     ``<chr>``      `\
`#> ``1`` ETI              3.73     88 ``<language>`` Student's t`\
`#> ``2`` ETI             -``0.``539``    88 ``<language>`` Student's t`\
`#> ``3`` ETI             23.2      88 ``<language>`` Student's t`\
`#> ``4`` ETI             -``0.``359``    88 ``<language>`` Student's t`\
`#> ``5`` ETI              2.90     88 ``<language>`` Student's t`\
`#> ``6`` ETI             15.4      88 ``<language>`` Student's t`

## Suggestions

If you find any bugs or have any suggestions/remarks, please file an
issue on GitHub:
<https://github.com/IndrajeetPatil/statsExpressions/issues>

## Citation

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
