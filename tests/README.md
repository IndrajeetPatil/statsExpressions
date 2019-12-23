Tests and Coverage
================
23 December, 2019 20:15:33

  - [Coverage](#coverage)
  - [Unit Tests](#unit-tests)

This output is created by
[covrpage](https://github.com/metrumresearchgroup/covrpage).

## Coverage

Coverage summary is created using the
[covr](https://github.com/r-lib/covr) package.

| Object                                                                                     | Coverage (%) |
| :----------------------------------------------------------------------------------------- | :----------: |
| statsExpressions                                                                           |    99.41     |
| [R/helpers\_anova\_expressions.R](../R/helpers_anova_expressions.R)                        |    96.67     |
| [R/helpers\_bf\_tests.R](../R/helpers_bf_tests.R)                                          |    100.00    |
| [R/helpers\_contingency\_tabs\_expressions.R](../R/helpers_contingency_tabs_expressions.R) |    100.00    |
| [R/helpers\_corr\_test\_expressions.R](../R/helpers_corr_test_expressions.R)               |    100.00    |
| [R/helpers\_effsize\_ci.R](../R/helpers_effsize_ci.R)                                      |    100.00    |
| [R/helpers\_messages.R](../R/helpers_messages.R)                                           |    100.00    |
| [R/helpers\_miscellaneous.R](../R/helpers_miscellaneous.R)                                 |    100.00    |
| [R/helpers\_t\_onesample\_expressions.R](../R/helpers_t_onesample_expressions.R)           |    100.00    |
| [R/helpers\_t\_test\_expressions.R](../R/helpers_t_test_expressions.R)                     |    100.00    |

<br>

## Unit Tests

Unit Test summary is created using the
[testthat](https://github.com/r-lib/testthat) package.

| file                                                                          |   n |  time | error | failed | skipped | warning | icon |
| :---------------------------------------------------------------------------- | --: | ----: | ----: | -----: | ------: | ------: | :--- |
| [test-effsize\_helpers.R](testthat/test-effsize_helpers.R)                    | 147 | 11.54 |     0 |      0 |       0 |       0 |      |
| [test-expr\_anova\_bayes.R](testthat/test-expr_anova_bayes.R)                 |   3 |  3.04 |     0 |      0 |       0 |       0 |      |
| [test-expr\_anova\_nonparametric.R](testthat/test-expr_anova_nonparametric.R) |   4 |  1.36 |     0 |      0 |       0 |       0 |      |
| [test-expr\_anova\_parametric.R](testthat/test-expr_anova_parametric.R)       |  11 |  2.52 |     0 |      0 |       1 |       0 | \+   |
| [test-expr\_anova\_robust.R](testthat/test-expr_anova_robust.R)               |   4 |  2.73 |     0 |      0 |       0 |       0 |      |
| [test-expr\_contingency\_tab.R](testthat/test-expr_contingency_tab.R)         |  13 |  1.19 |     0 |      0 |       0 |       0 |      |
| [test-expr\_corr\_test.R](testthat/test-expr_corr_test.R)                     |   4 |  0.11 |     0 |      0 |       0 |       0 |      |
| [test-expr\_t\_bayes.R](testthat/test-expr_t_bayes.R)                         |   4 |  0.14 |     0 |      0 |       0 |       0 |      |
| [test-expr\_t\_nonparametric.R](testthat/test-expr_t_nonparametric.R)         |   3 |  0.22 |     0 |      0 |       0 |       0 |      |
| [test-expr\_t\_onesample.R](testthat/test-expr_t_onesample.R)                 |   7 |  0.20 |     0 |      0 |       0 |       0 |      |
| [test-expr\_t\_parametric.R](testthat/test-expr_t_parametric.R)               |   4 |  0.10 |     0 |      0 |       0 |       0 |      |
| [test-expr\_t\_robust.R](testthat/test-expr_t_robust.R)                       |   4 |  4.10 |     0 |      0 |       0 |       0 |      |
| [test-expr\_templates.R](testthat/test-expr_templates.R)                      |   5 |  0.03 |     0 |      0 |       0 |       0 |      |
| [test-helpers\_bf\_tests.R](testthat/test-helpers_bf_tests.R)                 |  62 |  6.27 |     0 |      0 |       0 |       0 |      |
| [test-subtitle\_misc\_helpers.R](testthat/test-subtitle_misc_helpers.R)       |   4 |  0.06 |     0 |      0 |       0 |       0 |      |
| [test-switch\_statements.R](testthat/test-switch_statements.R)                |  21 |  0.01 |     0 |      0 |       0 |       0 |      |

<details open>

<summary> Show Detailed Test Results </summary>

| file                                                                               | context                                    |                                 test                                  | status  |  n |  time | icon |
| :--------------------------------------------------------------------------------- | :----------------------------------------- | :-------------------------------------------------------------------: | :------ | -: | ----: | :--- |
| [test-effsize\_helpers.R](testthat/test-effsize_helpers.R#L11)                     | effsize\_t\_parametric                     |   effsize works for Cohen’s d and Hedge’s g (between - without NA)    | PASS    | 29 |  0.06 |      |
| [test-effsize\_helpers.R](testthat/test-effsize_helpers.R#L187_L190)               | effsize\_t\_parametric                     |     effsize works for Cohen’s d and Hedge’s g (between - with NA)     | PASS    | 24 |  0.03 |      |
| [test-effsize\_helpers.R](testthat/test-effsize_helpers.R#L339_L345)               | effsize\_t\_parametric                     |    effsize works for Cohen’s d and Hedge’s g (within - without NA)    | PASS    | 26 |  0.03 |      |
| [test-effsize\_helpers.R](testthat/test-effsize_helpers.R#L549_L558)               | effsize\_t\_parametric                     |     effsize works for Cohen’s d and Hedge’s g (within - with NA)      | PASS    | 23 |  0.19 |      |
| [test-effsize\_helpers.R](testthat/test-effsize_helpers.R#L775)                    | effsize\_t\_parametric                     |                   effsize works for one sample test                   | PASS    | 14 |  0.02 |      |
| [test-effsize\_helpers.R](testthat/test-effsize_helpers.R#L833)                    | t1way\_ci                                  |                            t1way\_ci works                            | PASS    | 10 | 10.33 |      |
| [test-effsize\_helpers.R](testthat/test-effsize_helpers.R#L882)                    | test\_yuend\_ci                            |       Yuen’s test on trimmed means for dependent samples works        | PASS    | 12 |  0.61 |      |
| [test-effsize\_helpers.R](testthat/test-effsize_helpers.R#L939)                    | robcor\_ci                                 |                           robcor\_ci works                            | PASS    |  9 |  0.27 |      |
| [test-expr\_anova\_bayes.R](testthat/test-expr_anova_bayes.R#L39)                  | expr\_anova\_bayes                         |              expr\_anova\_bayes works (between-subjects)              | PASS    |  1 |  0.04 |      |
| [test-expr\_anova\_bayes.R](testthat/test-expr_anova_bayes.R#L79)                  | expr\_anova\_bayes                         |              expr\_anova\_bayes works (within-subjects)               | PASS    |  1 |  0.28 |      |
| [test-expr\_anova\_bayes.R](testthat/test-expr_anova_bayes.R#L119)                 | expr\_anova\_bayes                         |         expr\_anova\_bayes works (within-subjects) - with NA          | PASS    |  1 |  2.72 |      |
| [test-expr\_anova\_nonparametric.R](testthat/test-expr_anova_nonparametric.R#L56)  | expr\_anova\_nonparametric                 |             between-subjects - data with and without NAs              | PASS    |  2 |  0.24 |      |
| [test-expr\_anova\_nonparametric.R](testthat/test-expr_anova_nonparametric.R#L163) | expr\_anova\_nonparametric                 |              within-subjects - data with and without NAs              | PASS    |  2 |  1.12 |      |
| [test-expr\_anova\_parametric.R](testthat/test-expr_anova_parametric.R#L58)        | expr\_anova\_parametric - between-subjects |             parametric anova subtitles work (without NAs)             | PASS    |  1 |  0.05 |      |
| [test-expr\_anova\_parametric.R](testthat/test-expr_anova_parametric.R#L119)       | expr\_anova\_parametric - between-subjects |              parametric anova subtitles work (with NAs)               | PASS    |  1 |  0.94 |      |
| [test-expr\_anova\_parametric.R](testthat/test-expr_anova_parametric.R#L177)       | expr\_anova\_parametric - between-subjects |         parametric anova subtitles with partial omega-squared         | PASS    |  1 |  0.12 |      |
| [test-expr\_anova\_parametric.R](testthat/test-expr_anova_parametric.R#L236)       | expr\_anova\_parametric - between-subjects | parametric anova subtitles with partial eta-squared and data with NAs | PASS    |  1 |  0.05 |      |
| [test-expr\_anova\_parametric.R](testthat/test-expr_anova_parametric.R#L347)       | expr\_anova\_parametric - between-subjects | parametric anova subtitles with partial eta-squared and data with NAs | PASS    |  2 |  0.16 |      |
| [test-expr\_anova\_parametric.R](testthat/test-expr_anova_parametric.R#L409)       | expr\_anova\_parametric - between-subjects |             parametric anova subtitles work (without NAs)             | PASS    |  1 |  0.53 |      |
| [test-expr\_anova\_parametric.R](testthat/test-expr_anova_parametric.R#L469)       | expr\_anova\_parametric - between-subjects |              parametric anova subtitles work (with NAs)               | PASS    |  2 |  0.26 |      |
| [test-expr\_anova\_parametric.R](testthat/test-expr_anova_parametric.R#L583)       | expr\_anova\_parametric - between-subjects |           parametric anova subtitles work (catch bad data)            | PASS    |  1 |  0.41 |      |
| [test-expr\_anova\_parametric.R](testthat/test-expr_anova_parametric.R#L592)       | expr\_anova\_parametric - between-subjects |               checking warning message when too few obs               | SKIPPED |  1 |  0.00 | \+   |
| [test-expr\_anova\_robust.R](testthat/test-expr_anova_robust.R#L58)                | expr\_anova\_robust                        |             expr\_anova\_robust works - between-subjects              | PASS    |  2 |  2.66 |      |
| [test-expr\_anova\_robust.R](testthat/test-expr_anova_robust.R#L155)               | expr\_anova\_robust                        |              expr\_anova\_robust works - within-subjects              | PASS    |  2 |  0.07 |      |
| [test-expr\_contingency\_tab.R](testthat/test-expr_contingency_tab.R#L60)          | expr\_contingency\_tab                     |            expr\_contingency\_tab works - data without NAs            | PASS    |  2 |  0.47 |      |
| [test-expr\_contingency\_tab.R](testthat/test-expr_contingency_tab.R#L163)         | expr\_contingency\_tab                     |             expr\_contingency\_tab works - data with NAs              | PASS    |  1 |  0.06 |      |
| [test-expr\_contingency\_tab.R](testthat/test-expr_contingency_tab.R#L243)         | expr\_contingency\_tab                     |     paired expr\_contingency\_tab works - counts data without NAs     | PASS    |  1 |  0.10 |      |
| [test-expr\_contingency\_tab.R](testthat/test-expr_contingency_tab.R#L336)         | expr\_contingency\_tab                     |            paired expr\_contingency\_tab works - with NAs             | PASS    |  1 |  0.09 |      |
| [test-expr\_contingency\_tab.R](testthat/test-expr_contingency_tab.R#L420)         | expr\_contingency\_tab                     |                          paired data 4-by-4                           | PASS    |  1 |  0.10 |      |
| [test-expr\_contingency\_tab.R](testthat/test-expr_contingency_tab.R#L478)         | expr\_contingency\_tab                     |      Goodness of Fit expr\_contingency\_tab works without counts      | PASS    |  2 |  0.21 |      |
| [test-expr\_contingency\_tab.R](testthat/test-expr_contingency_tab.R#L575)         | expr\_contingency\_tab                     |       Goodness of Fit expr\_contingency\_tab works with counts        | PASS    |  1 |  0.08 |      |
| [test-expr\_contingency\_tab.R](testthat/test-expr_contingency_tab.R#L629)         | expr\_contingency\_tab                     |             works with dataframes with NAs and with ratio             | PASS    |  1 |  0.05 |      |
| [test-expr\_contingency\_tab.R](testthat/test-expr_contingency_tab.R#L645_L648)    | expr\_contingency\_tab                     |                       works even in edge cases                        | PASS    |  3 |  0.03 |      |
| [test-expr\_corr\_test.R](testthat/test-expr_corr_test.R#L54)                      | expr\_corr\_test                           |                expr\_corr\_test works - nonparametric                 | PASS    |  1 |  0.03 |      |
| [test-expr\_corr\_test.R](testthat/test-expr_corr_test.R#L112)                     | expr\_corr\_test                           |                  expr\_corr\_test works - parametric                  | PASS    |  1 |  0.00 |      |
| [test-expr\_corr\_test.R](testthat/test-expr_corr_test.R#L170)                     | expr\_corr\_test                           |                    expr\_corr\_test works - robust                    | PASS    |  1 |  0.06 |      |
| [test-expr\_corr\_test.R](testthat/test-expr_corr_test.R#L212)                     | expr\_corr\_test                           |                    expr\_corr\_test works - bayes                     | PASS    |  1 |  0.02 |      |
| [test-expr\_t\_bayes.R](testthat/test-expr_t_bayes.R#L42)                          | expr\_t\_bayes                             |            expr\_t\_bayes works - between-subjects design             | PASS    |  1 |  0.03 |      |
| [test-expr\_t\_bayes.R](testthat/test-expr_t_bayes.R#L83)                          | expr\_t\_bayes                             |       expr\_t\_bayes works - between-subjects design - with NA        | PASS    |  1 |  0.03 |      |
| [test-expr\_t\_bayes.R](testthat/test-expr_t_bayes.R#L160)                         | expr\_t\_bayes                             |         expr\_t\_bayes\_paired works - within-subjects design         | PASS    |  1 |  0.04 |      |
| [test-expr\_t\_bayes.R](testthat/test-expr_t_bayes.R#L201)                         | expr\_t\_bayes                             |    expr\_t\_bayes\_paired works - within-subjects design - with NA    | PASS    |  1 |  0.04 |      |
| [test-expr\_t\_nonparametric.R](testthat/test-expr_t_nonparametric.R#L53)          | expr\_t\_nonparametric                     |        expr\_t\_nonparametric works - between-subjects design         | PASS    |  1 |  0.13 |      |
| [test-expr\_t\_nonparametric.R](testthat/test-expr_t_nonparametric.R#L99_L103)     | expr\_t\_nonparametric                     |         expr\_t\_nonparametric works - within-subjects design         | PASS    |  2 |  0.09 |      |
| [test-expr\_t\_onesample.R](testthat/test-expr_t_onesample.R#L199)                 | expr\_t\_onesample                         |                  expr\_t\_onesample parametric works                  | PASS    |  4 |  0.06 |      |
| [test-expr\_t\_onesample.R](testthat/test-expr_t_onesample.R#L256)                 | expr\_t\_onesample                         |                expr\_t\_onesample non-parametric works                | PASS    |  1 |  0.08 |      |
| [test-expr\_t\_onesample.R](testthat/test-expr_t_onesample.R#L306)                 | expr\_t\_onesample                         |                    expr\_t\_onesample robust works                    | PASS    |  1 |  0.03 |      |
| [test-expr\_t\_onesample.R](testthat/test-expr_t_onesample.R#L347)                 | expr\_t\_onesample                         |                 expr\_t\_onesample bayes factor works                 | PASS    |  1 |  0.03 |      |
| [test-expr\_t\_parametric.R](testthat/test-expr_t_parametric.R#L63)                | expr\_t\_parametric                        |        parametric t-test works (between-subjects without NAs)         | PASS    |  1 |  0.02 |      |
| [test-expr\_t\_parametric.R](testthat/test-expr_t_parametric.R#L127)               | expr\_t\_parametric                        |          parametric t-test works (between-subjects with NAs)          | PASS    |  1 |  0.02 |      |
| [test-expr\_t\_parametric.R](testthat/test-expr_t_parametric.R#L186)               | expr\_t\_parametric                        |         parametric t-test works (within-subjects without NAs)         | PASS    |  1 |  0.03 |      |
| [test-expr\_t\_parametric.R](testthat/test-expr_t_parametric.R#L243)               | expr\_t\_parametric                        |          parametric t-test works (within-subjects with NAs)           | PASS    |  1 |  0.03 |      |
| [test-expr\_t\_robust.R](testthat/test-expr_t_robust.R#L58)                        | expr\_t\_robust                            |            expr\_t\_robust - within-subjects - without NAs            | PASS    |  2 |  2.32 |      |
| [test-expr\_t\_robust.R](testthat/test-expr_t_robust.R#L134)                       | expr\_t\_robust                            |             expr\_t\_robust - within-subjects - with NAs              | PASS    |  1 |  1.15 |      |
| [test-expr\_t\_robust.R](testthat/test-expr_t_robust.R#L191)                       | expr\_t\_robust                            |           expr\_t\_robust - between-subjects - without NAs            | PASS    |  1 |  0.63 |      |
| [test-expr\_templates.R](testthat/test-expr_templates.R#L59)                       | expr\_templates                            |       checking if subtitle template works without any parameter       | PASS    |  1 |  0.00 |      |
| [test-expr\_templates.R](testthat/test-expr_templates.R#L121)                      | expr\_templates                            |      checking if subtitle template works with a single parameter      | PASS    |  2 |  0.02 |      |
| [test-expr\_templates.R](testthat/test-expr_templates.R#L204)                      | expr\_templates                            |        checking if subtitle template works with two parameters        | PASS    |  2 |  0.01 |      |
| [test-helpers\_bf\_tests.R](testthat/test-helpers_bf_tests.R#L20)                  | helpers\_bf\_tests                         |                      bayes factor (correlation)                       | PASS    |  5 |  0.02 |      |
| [test-helpers\_bf\_tests.R](testthat/test-helpers_bf_tests.R#L57)                  | helpers\_bf\_tests                         |               bayes factor (independent samples t-test)               | PASS    |  6 |  0.06 |      |
| [test-helpers\_bf\_tests.R](testthat/test-helpers_bf_tests.R#L105)                 | helpers\_bf\_tests                         |                     bayes factor (paired t-test)                      | PASS    |  7 |  0.06 |      |
| [test-helpers\_bf\_tests.R](testthat/test-helpers_bf_tests.R#L146)                 | helpers\_bf\_tests                         |                   bayes factor (one sample t-test)                    | PASS    |  7 |  0.03 |      |
| [test-helpers\_bf\_tests.R](testthat/test-helpers_bf_tests.R#L212)                 | helpers\_bf\_tests                         |                bayes factor (between-subjects - anova)                | PASS    |  9 |  0.14 |      |
| [test-helpers\_bf\_tests.R](testthat/test-helpers_bf_tests.R#L320)                 | helpers\_bf\_tests                         |                bayes factor (within-subjects - anova)                 | PASS    |  9 |  1.24 |      |
| [test-helpers\_bf\_tests.R](testthat/test-helpers_bf_tests.R#L385)                 | helpers\_bf\_tests                         |                    bayes factor (proportion test)                     | PASS    |  6 |  4.61 |      |
| [test-helpers\_bf\_tests.R](testthat/test-helpers_bf_tests.R#L479)                 | helpers\_bf\_tests                         |                    bayes factor (contingency tab)                     | PASS    | 10 |  0.09 |      |
| [test-helpers\_bf\_tests.R](testthat/test-helpers_bf_tests.R#L555)                 | helpers\_bf\_tests                         |                   bayes factor caption maker check                    | PASS    |  1 |  0.00 |      |
| [test-helpers\_bf\_tests.R](testthat/test-helpers_bf_tests.R#L585_L603)            | helpers\_bf\_tests                         |                   bayes factor caption maker check                    | PASS    |  2 |  0.02 |      |
| [test-subtitle\_misc\_helpers.R](testthat/test-subtitle_misc_helpers.R#L27)        | long\_to\_wide\_converter                  |                    long\_to\_wide\_converter works                    | PASS    |  4 |  0.06 |      |
| [test-switch\_statements.R](testthat/test-switch_statements.R#L10)                 | switch statements                          |                   switch for effct size type works                    | PASS    | 13 |  0.01 |      |
| [test-switch\_statements.R](testthat/test-switch_statements.R#L34)                 | switch statements                          |                      switch for stats type works                      | PASS    |  8 |  0.00 |      |

| Failed | Warning | Skipped |
| :----- | :------ | :------ |
| \!     | \-      | \+      |

</details>

<details>

<summary> Session Info </summary>

| Field    | Value                            |
| :------- | :------------------------------- |
| Version  | R version 3.6.1 (2019-07-05)     |
| Platform | x86\_64-w64-mingw32/x64 (64-bit) |
| Running  | Windows 10 x64 (build 16299)     |
| Language | English\_United States           |
| Timezone | Europe/Berlin                    |

| Package  | Version |
| :------- | :------ |
| testthat | 2.3.1   |
| covr     | 3.4.0   |
| covrpage | 0.0.70  |

</details>

<!--- Final Status : skipped/warning --->
