testthat::set_state_inspector(function() {
  list(
    attached = search(),
    connections = getAllConnections(),
    cwd = getwd(),
    envvars = Sys.getenv(),
    libpaths = .libPaths(),
    locale = Sys.getlocale(),
    options = .Options,
    NULL
  )
})
