.onLoad <- function(libname, pkgname) {
  op <- options()
  op.touchstone <- list(
    "touchstone.skip_install" = FALSE,
    "touchstone.dir" = "touchstone",
    # how many times should inner loop be ran in benchmark_run_iteration
    "touchstone.n_iterations" = 1
  )
  toset <- !(names(op.touchstone) %in% names(op))
  if (any(toset)) options(op.touchstone[toset])
  invisible()
}
