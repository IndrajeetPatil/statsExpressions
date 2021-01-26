# defining global variables and functions to appease R CMD Check

utils::globalVariables(
  names = c(
    ".",
    "Est",
    "ci.low",
    "ci.up",
    "effsize",
    "effectsize",
    "estimate",
    "rowid",
    "statistic",
    "std.error",
    "term",
    ".counts",
    "df",
    "df.error",
    "k.parameter",
    "effsize.text",
    "method",
    ".f",
    ".f.es",
    "bf10",
    "estimate.LB",
    "estimate.UB",
    "r2.component",
    "ci.width",
    ".fn",
    ".ns"
  ),
  package = "statsExpressions",
  add = FALSE
)
