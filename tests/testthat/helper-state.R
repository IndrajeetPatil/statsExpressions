testthat::set_state_inspector(function() {
  list(
    attached = search(),
    options = options(),
    envvars = Sys.getenv(),
    libpaths = .libPaths(),
    locale = Sys.getlocale(),
    cwd = getwd(),
    connections = nrow(getAllConnections())
  )
})
