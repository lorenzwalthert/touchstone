#' Sources a script
#'
#' Basically [base::source()], but prepending the library path with a
#' touchstone library. For isolation, it's not allowed to install the
#' benchmarked package into the global library, as asserted with
#' [assert_no_global_installation()]. However, this implies that the package
#' is not available in the touchstone script outside of benchmark runs. To
#' remove this limitation, we prepend a touchstone library location that
#' contains the installed benchmarked package, and temporarily remove it during
#' benchmarking if another touchstone library is used.
#' @param path The script to run.
#' @param ref The ref that corresponds to the library that should be prepended
#'   to the library path.
#' @export
with_touchstone_lib <- function(path, ref = Sys.getenv("GITHUB_HEAD_REF")) {
  withr::local_libpaths(
    list(fs::path("touchstone", "lib", ref)),
    action = "prefix"
  )
  source(path, max.deparse.length = Inf)
}
