# statsExpressions: Tidy Dataframes and Expressions with Statistical Details

The `{statsExpressions}` package has two key aims:

- to provide a consistent syntax to do statistical analysis with tidy
  data (in a pipe-friendly manner),

- to provide statistical expressions (pre-formatted in-text statistical
  results) for plotting functions.

It covers common statistical tests (*t*-tests, one-way ANOVA,
correlation analyses, contingency table analyses, and meta-analyses)
with parametric, nonparametric, robust, and Bayesian approaches,
depending on the test.

Statistical packages exhibit substantial diversity in terms of their
syntax and expected input and output data type. For example, some
functions expect vectors as inputs, while others expect data frames.
Depending on whether it is a repeated measures design or not, functions
from the same package might expect data to be in wide or long format.
Some functions can internally omit missing values, while other functions
error in their presence. Furthermore, the objects returned by test
functions might not have all required information (e.g., degrees of
freedom, significance, Bayes factor, etc.) accessible in a consistent
data type: depending on the function, details may be returned as a list,
a matrix, an array, or a data frame. This diversity makes it difficult
to access all needed information for hypothesis testing and estimation,
to use the results downstream in a workflow, and to switch from one
statistical approach to another.

This is where `{statsExpressions}` comes in: It can be thought of as a
unified portal through which most of the functionality in these
underlying packages can be accessed, with a simpler interface and no
requirement to change data format.

This package forms the statistical processing backend for the
[`ggstatsplot`](https://www.indrapatil.com/ggstatsplot/) package.

For more documentation, see the dedicated
[website](https://www.indrapatil.com/statsExpressions/).

## See also

Useful links:

- <https://www.indrapatil.com/statsExpressions/>

- <https://github.com/IndrajeetPatil/statsExpressions>

- Report bugs at
  <https://github.com/IndrajeetPatil/statsExpressions/issues>

## Author

**Maintainer**: Indrajeet Patil <patilindrajeet.science@gmail.com>
([ORCID](https://orcid.org/0000-0003-1995-6531)) \[copyright holder\]

Authors:

- Indrajeet Patil <patilindrajeet.science@gmail.com>
  ([ORCID](https://orcid.org/0000-0003-1995-6531)) \[copyright holder\]
