#' \code{statsExpressions}
#'
#' @title statsExpressions: Tidy Dataframes and Expressions with Statistical Details
#'
#' @description
#'
#' Statistical packages exhibit substantial diversity in terms of their syntax
#' and expected input type. This can make it difficult to switch from one
#' statistical approach to another. For example, some functions expect vectors
#' as inputs, while others expect dataframes. Depending on whether it is a
#' repeated measures design or not, different functions might expect data to be
#' in wide or long format. Some functions can internally omit missing values,
#' while other functions error in their presence. Furthermore, if someone wishes
#' to utilize the objects returned by these packages downstream in their
#' workflow, this is not straightforward either because even functions from the
#' same package can return a list, a matrix, an array, a dataframe, etc.,
#' depending on the function.
#'
#' This is where `statsExpressions` comes in: It can be thought of as a unified
#' portal through which most of the functionality in these underlying packages
#' can be accessed, with a simpler interface and no requirement to change data
#' format.
#'
#' This package forms the statistical processing backend for
#' [`ggstatsplot`](https://indrajeetpatil.github.io/ggstatsplot/) package.
#'
#' For more documentation, see the dedicated
#' \href{https://indrajeetpatil.github.io/statsExpressions/}{Website}.
#'
#' @docType package
#' @aliases statsExpressions statsExpressions-package
#' @name statsExpressions-package
"_PACKAGE"


## statsExpressions namespace: start
#' @import rlang
## statsExpressions namespace: end
NULL
