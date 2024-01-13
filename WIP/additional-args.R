#' @param .f.args,.f.es.args A **list** of additional arguments to be passed to
#'   internal functions `.f` and `.f.es`. These are used to carry out
#'   statistical tests and to compute effect sizes, respectively. To see what
#'   additional arguments are available, you can have a look at the
#'   documentation for the respective function. Note that the chosen internal
#'   functions (`.f` and `.f.es`) will themselves depend on the specified `type`
#'   argument. The defaults reflect arguments relevant for parametric tests
#'   because that's the default value for `type`. See `Details` section to see
#'   which internal functions are used. The function might fail if you provide
#'   an argument in a list which the underlying function does not take. This can
#'   happen when you forget that you changed `type` argument, but forget to
#'   change the `.f.args` and `.f.es.args` accordingly. Also, note that these
#'   arguments are useful to provide *additional arguments*. Therefore, you
#'   can's re-specify an argument you have already specified. For example, for
#'   robust tests, you can use `tr` argument to specify trimming level, but then
#'   you can't specify `tr` again inside list passed to `.f.args`
