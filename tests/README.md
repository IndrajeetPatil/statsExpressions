Tests and Coverage
================
16 August, 2019 13:33:05

  - [Coverage](#coverage)
  - [Unit Tests](#unit-tests)

This output is created by
[covrpage](https://github.com/metrumresearchgroup/covrpage).

## Coverage

Coverage summary is created using the
[covr](https://github.com/r-lib/covr) package.

| Object                                                                                     | Coverage (%) |
| :----------------------------------------------------------------------------------------- | :----------: |
| statsExpressions                                                                           |     100      |
| [R/helpers\_anova\_expressions.R](../R/helpers_anova_expressions.R)                        |     100      |
| [R/helpers\_bf\_tests.R](../R/helpers_bf_tests.R)                                          |     100      |
| [R/helpers\_contingency\_tabs\_expressions.R](../R/helpers_contingency_tabs_expressions.R) |     100      |
| [R/helpers\_corr\_test\_expressions.R](../R/helpers_corr_test_expressions.R)               |     100      |
| [R/helpers\_effsize\_ci.R](../R/helpers_effsize_ci.R)                                      |     100      |
| [R/helpers\_messages.R](../R/helpers_messages.R)                                           |     100      |
| [R/helpers\_miscellaneous.R](../R/helpers_miscellaneous.R)                                 |     100      |
| [R/helpers\_t\_onesample\_expressions.R](../R/helpers_t_onesample_expressions.R)           |     100      |
| [R/helpers\_t\_test\_expressions.R](../R/helpers_t_test_expressions.R)                     |     100      |

<br>

## Unit Tests

Unit Test summary is created using the
[testthat](https://github.com/r-lib/testthat) package.

| file                                                                          |   n |  time | error | failed | skipped | warning |
| :---------------------------------------------------------------------------- | --: | ----: | ----: | -----: | ------: | ------: |
| [test-effsize\_helpers.R](testthat/test-effsize_helpers.R)                    | 170 | 16.07 |     0 |      0 |       0 |       0 |
| [test-expr\_anova\_bayes.R](testthat/test-expr_anova_bayes.R)                 |   3 |  2.29 |     0 |      0 |       0 |       0 |
| [test-expr\_anova\_nonparametric.R](testthat/test-expr_anova_nonparametric.R) |   4 |  0.81 |     0 |      0 |       0 |       0 |
| [test-expr\_anova\_parametric.R](testthat/test-expr_anova_parametric.R)       |  11 |  1.84 |     0 |      0 |       0 |       0 |
| [test-expr\_anova\_robust.R](testthat/test-expr_anova_robust.R)               |   4 |  2.58 |     0 |      0 |       0 |       0 |
| [test-expr\_contingency\_tab.R](testthat/test-expr_contingency_tab.R)         |  13 |  0.66 |     0 |      0 |       0 |       0 |
| [test-expr\_corr\_test.R](testthat/test-expr_corr_test.R)                     |   4 |  0.13 |     0 |      0 |       0 |       0 |
| [test-expr\_t\_bayes.R](testthat/test-expr_t_bayes.R)                         |   4 |  0.10 |     0 |      0 |       0 |       0 |
| [test-expr\_t\_nonparametric.R](testthat/test-expr_t_nonparametric.R)         |   3 |  0.98 |     0 |      0 |       0 |       0 |
| [test-expr\_t\_onesample.R](testthat/test-expr_t_onesample.R)                 |   7 |  1.15 |     0 |      0 |       0 |       0 |
| [test-expr\_t\_parametric.R](testthat/test-expr_t_parametric.R)               |   4 |  0.07 |     0 |      0 |       0 |       0 |
| [test-expr\_t\_robust.R](testthat/test-expr_t_robust.R)                       |   4 |  2.85 |     0 |      0 |       0 |       0 |
| [test-expr\_templates.R](testthat/test-expr_templates.R)                      |   5 |  0.02 |     0 |      0 |       0 |       0 |
| [test-helpers\_bf\_tests.R](testthat/test-helpers_bf_tests.R)                 |  62 |  4.23 |     0 |      0 |       0 |       0 |
| [test-subtitle\_misc\_helpers.R](testthat/test-subtitle_misc_helpers.R)       |   4 |  0.05 |     0 |      0 |       0 |       0 |
| [test-switch\_statements.R](testthat/test-switch_statements.R)                |  21 |  0.01 |     0 |      0 |       0 |       0 |

<details closed>

<summary> Show Detailed Test Results </summary>

| file                                                                               | context                                    | test                                                                  | status |  n |  time |
| :--------------------------------------------------------------------------------- | :----------------------------------------- | :-------------------------------------------------------------------- | :----- | -: | ----: |
| [test-effsize\_helpers.R](testthat/test-effsize_helpers.R#L11)                     | effsize\_t\_parametric                     | effsize works for Cohen’s d and Hedge’s g (between - without NA)      | PASS   | 29 |  0.03 |
| [test-effsize\_helpers.R](testthat/test-effsize_helpers.R#L181_L184)               | effsize\_t\_parametric                     | effsize works for Cohen’s d and Hedge’s g (between - with NA)         | PASS   | 24 |  0.03 |
| [test-effsize\_helpers.R](testthat/test-effsize_helpers.R#L329_L335)               | effsize\_t\_parametric                     | effsize works for Cohen’s d and Hedge’s g (within - without NA)       | PASS   | 26 |  0.03 |
| [test-effsize\_helpers.R](testthat/test-effsize_helpers.R#L534_L543)               | effsize\_t\_parametric                     | effsize works for Cohen’s d and Hedge’s g (within - with NA)          | PASS   | 23 |  0.07 |
| [test-effsize\_helpers.R](testthat/test-effsize_helpers.R#L756)                    | effsize\_t\_parametric                     | effsize works for one sample test                                     | PASS   | 14 |  0.01 |
| [test-effsize\_helpers.R](testthat/test-effsize_helpers.R#L827)                    | t1way\_ci                                  | t1way\_ci works                                                       | PASS   | 13 | 14.52 |
| [test-effsize\_helpers.R](testthat/test-effsize_helpers.R#L1006)                   | test\_yuend\_ci                            | Yuen’s test on trimmed means for dependent samples works              | PASS   | 18 |  1.09 |
| [test-effsize\_helpers.R](testthat/test-effsize_helpers.R#L1096)                   | robcor\_ci                                 | robcor\_ci works                                                      | PASS   | 23 |  0.29 |
| [test-expr\_anova\_bayes.R](testthat/test-expr_anova_bayes.R#L38)                  | expr\_anova\_bayes                         | expr\_anova\_bayes works (between-subjects)                           | PASS   |  1 |  0.04 |
| [test-expr\_anova\_bayes.R](testthat/test-expr_anova_bayes.R#L78)                  | expr\_anova\_bayes                         | expr\_anova\_bayes works (within-subjects)                            | PASS   |  1 |  0.23 |
| [test-expr\_anova\_bayes.R](testthat/test-expr_anova_bayes.R#L118)                 | expr\_anova\_bayes                         | expr\_anova\_bayes works (within-subjects) - with NA                  | PASS   |  1 |  2.02 |
| [test-expr\_anova\_nonparametric.R](testthat/test-expr_anova_nonparametric.R#L56)  | expr\_anova\_nonparametric                 | between-subjects - data with and without NAs                          | PASS   |  2 |  0.29 |
| [test-expr\_anova\_nonparametric.R](testthat/test-expr_anova_nonparametric.R#L161) | expr\_anova\_nonparametric                 | within-subjects - data with and without NAs                           | PASS   |  2 |  0.52 |
| [test-expr\_anova\_parametric.R](testthat/test-expr_anova_parametric.R#L57)        | expr\_anova\_parametric - between-subjects | parametric anova subtitles work (without NAs)                         | PASS   |  1 |  0.03 |
| [test-expr\_anova\_parametric.R](testthat/test-expr_anova_parametric.R#L130)       | expr\_anova\_parametric - between-subjects | parametric anova subtitles work (with NAs)                            | PASS   |  1 |  0.45 |
| [test-expr\_anova\_parametric.R](testthat/test-expr_anova_parametric.R#L189)       | expr\_anova\_parametric - between-subjects | parametric anova subtitles with partial omega-squared                 | PASS   |  1 |  0.10 |
| [test-expr\_anova\_parametric.R](testthat/test-expr_anova_parametric.R#L249)       | expr\_anova\_parametric - between-subjects | parametric anova subtitles with partial eta-squared and data with NAs | PASS   |  1 |  0.03 |
| [test-expr\_anova\_parametric.R](testthat/test-expr_anova_parametric.R#L360)       | expr\_anova\_parametric - between-subjects | parametric anova subtitles with partial eta-squared and data with NAs | PASS   |  2 |  0.12 |
| [test-expr\_anova\_parametric.R](testthat/test-expr_anova_parametric.R#L425)       | expr\_anova\_parametric - within-subjects  | parametric anova subtitles work (without NAs)                         | PASS   |  1 |  0.42 |
| [test-expr\_anova\_parametric.R](testthat/test-expr_anova_parametric.R#L485)       | expr\_anova\_parametric - within-subjects  | parametric anova subtitles work (with NAs)                            | PASS   |  2 |  0.21 |
| [test-expr\_anova\_parametric.R](testthat/test-expr_anova_parametric.R#L596)       | expr\_anova\_parametric - within-subjects  | parametric anova subtitles work (catch bad data)                      | PASS   |  1 |  0.39 |
| [test-expr\_anova\_parametric.R](testthat/test-expr_anova_parametric.R#L673_L703)  | expr\_anova\_parametric - within-subjects  | checking warning message when too few obs                             | PASS   |  1 |  0.09 |
| [test-expr\_anova\_robust.R](testthat/test-expr_anova_robust.R#L58)                | expr\_anova\_robust                        | expr\_anova\_robust works - between-subjects                          | PASS   |  2 |  2.53 |
| [test-expr\_anova\_robust.R](testthat/test-expr_anova_robust.R#L155)               | expr\_anova\_robust                        | expr\_anova\_robust works - within-subjects                           | PASS   |  2 |  0.05 |
| [test-expr\_contingency\_tab.R](testthat/test-expr_contingency_tab.R#L60)          | expr\_contingency\_tab                     | expr\_contingency\_tab works - data without NAs                       | PASS   |  2 |  0.28 |
| [test-expr\_contingency\_tab.R](testthat/test-expr_contingency_tab.R#L176)         | expr\_contingency\_tab                     | expr\_contingency\_tab works - data with NAs                          | PASS   |  1 |  0.03 |
| [test-expr\_contingency\_tab.R](testthat/test-expr_contingency_tab.R#L260)         | expr\_contingency\_tab\_paired             | paired expr\_contingency\_tab works - counts data without NAs         | PASS   |  1 |  0.05 |
| [test-expr\_contingency\_tab.R](testthat/test-expr_contingency_tab.R#L359)         | expr\_contingency\_tab\_paired             | paired expr\_contingency\_tab works - with NAs                        | PASS   |  1 |  0.05 |
| [test-expr\_contingency\_tab.R](testthat/test-expr_contingency_tab.R#L442)         | expr\_contingency\_tab\_paired             | paired data 4-by-4                                                    | PASS   |  1 |  0.06 |
| [test-expr\_contingency\_tab.R](testthat/test-expr_contingency_tab.R#L502)         | expr\_contingency\_tab\_gof                | Goodness of Fit expr\_contingency\_tab works without counts           | PASS   |  2 |  0.07 |
| [test-expr\_contingency\_tab.R](testthat/test-expr_contingency_tab.R#L599)         | expr\_contingency\_tab\_gof                | Goodness of Fit expr\_contingency\_tab works with counts              | PASS   |  1 |  0.06 |
| [test-expr\_contingency\_tab.R](testthat/test-expr_contingency_tab.R#L653)         | expr\_contingency\_tab\_gof                | works with dataframes with NAs and with ratio                         | PASS   |  1 |  0.03 |
| [test-expr\_contingency\_tab.R](testthat/test-expr_contingency_tab.R#L668_L671)    | expr\_contingency\_tab\_gof                | works even in edge cases                                              | PASS   |  3 |  0.03 |
| [test-expr\_corr\_test.R](testthat/test-expr_corr_test.R#L54)                      | expr\_corr\_test                           | expr\_corr\_test works - nonparametric                                | PASS   |  1 |  0.08 |
| [test-expr\_corr\_test.R](testthat/test-expr_corr_test.R#L112)                     | expr\_corr\_test                           | expr\_corr\_test works - parametric                                   | PASS   |  1 |  0.02 |
| [test-expr\_corr\_test.R](testthat/test-expr_corr_test.R#L169)                     | expr\_corr\_test                           | expr\_corr\_test works - robust                                       | PASS   |  1 |  0.01 |
| [test-expr\_corr\_test.R](testthat/test-expr_corr_test.R#L211)                     | expr\_corr\_test                           | expr\_corr\_test works - bayes                                        | PASS   |  1 |  0.02 |
| [test-expr\_t\_bayes.R](testthat/test-expr_t_bayes.R#L42)                          | expr\_t\_bayes                             | expr\_t\_bayes works - between-subjects design                        | PASS   |  1 |  0.02 |
| [test-expr\_t\_bayes.R](testthat/test-expr_t_bayes.R#L83)                          | expr\_t\_bayes                             | expr\_t\_bayes works - between-subjects design - with NA              | PASS   |  1 |  0.01 |
| [test-expr\_t\_bayes.R](testthat/test-expr_t_bayes.R#L160)                         | expr\_t\_bayes                             | expr\_t\_bayes\_paired works - within-subjects design                 | PASS   |  1 |  0.04 |
| [test-expr\_t\_bayes.R](testthat/test-expr_t_bayes.R#L199)                         | expr\_t\_bayes                             | expr\_t\_bayes\_paired works - within-subjects design - with NA       | PASS   |  1 |  0.03 |
| [test-expr\_t\_nonparametric.R](testthat/test-expr_t_nonparametric.R#L51)          | expr\_t\_nonparametric                     | expr\_t\_nonparametric works - between-subjects design                | PASS   |  1 |  0.72 |
| [test-expr\_t\_nonparametric.R](testthat/test-expr_t_nonparametric.R#L97_L101)     | expr\_t\_nonparametric                     | expr\_t\_nonparametric works - within-subjects design                 | PASS   |  2 |  0.26 |
| [test-expr\_t\_onesample.R](testthat/test-expr_t_onesample.R#L198)                 | expr\_t\_onesample                         | expr\_t\_onesample parametric works                                   | PASS   |  4 |  0.04 |
| [test-expr\_t\_onesample.R](testthat/test-expr_t_onesample.R#L255)                 | expr\_t\_onesample                         | expr\_t\_onesample non-parametric works                               | PASS   |  1 |  1.07 |
| [test-expr\_t\_onesample.R](testthat/test-expr_t_onesample.R#L305)                 | expr\_t\_onesample                         | expr\_t\_onesample robust works                                       | PASS   |  1 |  0.03 |
| [test-expr\_t\_onesample.R](testthat/test-expr_t_onesample.R#L346)                 | expr\_t\_onesample                         | expr\_t\_onesample bayes factor works                                 | PASS   |  1 |  0.01 |
| [test-expr\_t\_parametric.R](testthat/test-expr_t_parametric.R#L61)                | expr\_t\_parametric                        | parametric t-test works (between-subjects without NAs)                | PASS   |  1 |  0.02 |
| [test-expr\_t\_parametric.R](testthat/test-expr_t_parametric.R#L125)               | expr\_t\_parametric                        | parametric t-test works (between-subjects with NAs)                   | PASS   |  1 |  0.00 |
| [test-expr\_t\_parametric.R](testthat/test-expr_t_parametric.R#L183)               | expr\_t\_parametric                        | parametric t-test works (within-subjects without NAs)                 | PASS   |  1 |  0.03 |
| [test-expr\_t\_parametric.R](testthat/test-expr_t_parametric.R#L239)               | expr\_t\_parametric                        | parametric t-test works (within-subjects with NAs)                    | PASS   |  1 |  0.02 |
| [test-expr\_t\_robust.R](testthat/test-expr_t_robust.R#L54)                        | expr\_t\_robust                            | expr\_t\_robust - within-subjects - without NAs                       | PASS   |  2 |  1.57 |
| [test-expr\_t\_robust.R](testthat/test-expr_t_robust.R#L128)                       | expr\_t\_robust                            | expr\_t\_robust - within-subjects - with NAs                          | PASS   |  1 |  0.83 |
| [test-expr\_t\_robust.R](testthat/test-expr_t_robust.R#L183)                       | expr\_t\_robust                            | expr\_t\_robust - between-subjects - without NAs                      | PASS   |  1 |  0.45 |
| [test-expr\_templates.R](testthat/test-expr_templates.R#L56)                       | expr\_templates                            | checking if subtitle template works without any parameter             | PASS   |  1 |  0.00 |
| [test-expr\_templates.R](testthat/test-expr_templates.R#L116)                      | expr\_templates                            | checking if subtitle template works with a single parameter           | PASS   |  2 |  0.00 |
| [test-expr\_templates.R](testthat/test-expr_templates.R#L197)                      | expr\_templates                            | checking if subtitle template works with two parameters               | PASS   |  2 |  0.02 |
| [test-helpers\_bf\_tests.R](testthat/test-helpers_bf_tests.R#L20)                  | helpers\_bf\_tests                         | bayes factor (correlation)                                            | PASS   |  5 |  0.02 |
| [test-helpers\_bf\_tests.R](testthat/test-helpers_bf_tests.R#L57)                  | helpers\_bf\_tests                         | bayes factor (independent samples t-test)                             | PASS   |  6 |  0.03 |
| [test-helpers\_bf\_tests.R](testthat/test-helpers_bf_tests.R#L103)                 | helpers\_bf\_tests                         | bayes factor (paired t-test)                                          | PASS   |  7 |  0.04 |
| [test-helpers\_bf\_tests.R](testthat/test-helpers_bf_tests.R#L144)                 | helpers\_bf\_tests                         | bayes factor (one sample t-test)                                      | PASS   |  7 |  0.02 |
| [test-helpers\_bf\_tests.R](testthat/test-helpers_bf_tests.R#L210)                 | helpers\_bf\_tests                         | bayes factor (between-subjects - anova)                               | PASS   |  9 |  0.09 |
| [test-helpers\_bf\_tests.R](testthat/test-helpers_bf_tests.R#L318)                 | helpers\_bf\_tests                         | bayes factor (within-subjects - anova)                                | PASS   |  9 |  0.71 |
| [test-helpers\_bf\_tests.R](testthat/test-helpers_bf_tests.R#L383)                 | helpers\_bf\_tests                         | bayes factor (proportion test)                                        | PASS   |  6 |  3.25 |
| [test-helpers\_bf\_tests.R](testthat/test-helpers_bf_tests.R#L477)                 | helpers\_bf\_tests                         | bayes factor (contingency tab)                                        | PASS   | 10 |  0.07 |
| [test-helpers\_bf\_tests.R](testthat/test-helpers_bf_tests.R#L553)                 | helpers\_bf\_tests                         | bayes factor caption maker check                                      | PASS   |  1 |  0.00 |
| [test-helpers\_bf\_tests.R](testthat/test-helpers_bf_tests.R#L583_L601)            | helpers\_bf\_tests                         | bayes factor caption maker check                                      | PASS   |  2 |  0.00 |
| [test-subtitle\_misc\_helpers.R](testthat/test-subtitle_misc_helpers.R#L27)        | long\_to\_wide\_converter                  | long\_to\_wide\_converter works                                       | PASS   |  4 |  0.05 |
| [test-switch\_statements.R](testthat/test-switch_statements.R#L8)                  | switch statements                          | switch for effct size type works                                      | PASS   | 13 |  0.00 |
| [test-switch\_statements.R](testthat/test-switch_statements.R#L30)                 | switch statements                          | switch for stats type works                                           | PASS   |  8 |  0.01 |

</details>

<details>

<summary> Session Info </summary>

| Field    | Value                            |
| :------- | :------------------------------- |
| Version  | R version 3.6.1 (2019-07-05)     |
| Platform | x86\_64-w64-mingw32/x64 (64-bit) |
| Running  | Windows 10 x64 (build 16299)     |
| Language | English\_United States           |
| Timezone | America/New\_York                |

| Package  | Version |
| :------- | :------ |
| testthat | 2.2.1   |
| covr     | 3.3.0   |
| covrpage | 0.0.70  |

</details>

<!--- Final Status : pass --->
