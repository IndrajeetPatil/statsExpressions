# defining global variables and functions to appease R CMD Check

utils::globalVariables(
  names = c(
    ".",
    "Est",
    "ci.low",
    "ci.up",
    "df.denom",
    "df.num",
    "effect_size",
    "estimate",
    "int_df",
    "rowid",
    "statistic",
    "std.error",
    "term",
    ".counts",
    "df",
    "df.error"
  ),
  package = "statsExpressions",
  add = FALSE
)
