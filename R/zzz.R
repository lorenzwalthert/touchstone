.onLoad <- function(libname, pkgname) {
  op <- options()
  cache <- tibble::tibble(
    branch = character(), md5_hashes = list(), path_pkg = character()
  )

  op.touchstone <- list(
    "touchstone.skip_install" = FALSE,
    "touchstone.git_root" = find_git_root(),
    "touchstone.dir" = "touchstone",
    # how many times should inner loop be ran in benchmark_run_iteration
    "touchstone.n_iterations" = 1,
    "touchstone.hash_source_package" = cache
  )

  toset <- !(names(op.touchstone) %in% names(op))
  if (any(toset)) options(op.touchstone[toset])
  invisible()
}
