.onLoad <- function(libname, pkgname) {
  op <- options()
  cache <- tibble::tibble(
    ref = character(), md5_hashes = list(), path_pkg = character()
  )
  op.touchstone <- list(
    "touchstone.skip_install" = FALSE,
    "touchstone.dir" = "touchstone",
    # how many times should inner loop be ran in benchmark_run_iteration
    "touchstone.n_iterations" = 1,
    "touchstone.hash_source_package" = cache
  )
  toset <- !(names(op.touchstone) %in% names(op))
  if (any(toset)) options(op.touchstone[toset])
  invisible()
}
