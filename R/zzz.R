.onLoad <- function(libname, pkgname) {
  op <- options()
  op.touchstone <- list(
    "touchstone.skip_install" = FALSE,
    "touchstone.n_iterations" = 20,
    "touchstone.dir" = "touchstone"
  )
  toset <- !(names(op.touchstone) %in% names(op))
  if (any(toset)) options(op.touchstone[toset])
  invisible()
}
