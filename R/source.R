#' Sources a script
#'
#' Basically [base::source()], but prepending the library path with a
#' touchstone library and running the script in a temp directory to avoid
#' git operations like checking out different branches to interfere with the
#' script execution (as running the script changes itself through git checkout).
#'
#' @param path The script to run. It must fulfill the requirements of a
#'   [touchstone_script].
#' @param ref The ref that corresponds to the library that should be prepended
#'   to the library path when the script at `path` is executed. Note that during
#'   a benchmark run with [benchmark_run_ref()], all touchstone library paths
#'   are removed and the library path corresponding to the argument `ref`
#'   in [benchmark_run_ref()] is added.
#' @section Why this function?
#' For isolation, it's not allowed to install the
#' benchmarked package into the global library, as asserted with
#' [assert_no_global_installation()]. However, this implies that the package
#' is not available in the touchstone script outside of benchmark runs. To
#' remove this limitation, we prepend a touchstone library location that
#' contains the installed benchmarked package, and temporarily remove it during
#' benchmarking if another touchstone library is used.
#' @export
with_touchstone_lib <- function(path, ref = Sys.getenv("GITHUB_HEAD_REF")) {
  lib <- libpath_touchstone(ref)
  fs::dir_create(lib)
  withr::local_libpaths(
    list(lib),
    action = "prefix"
  )
  tempfile <- fs::file_temp()
  fs::file_copy(path, tempfile)
  usethis::ui_done(glue::glue(
    "Copied touchstone script to tempdir to prevent branch checkouts to effect",
    "the script."
  ))
  source(tempfile, max.deparse.length = Inf)
}


#' The script for benchmarking
#'
#' The script that contains the code which executes the benchmark. It is
#' typically called with [with_touchstone_lib()].
#'
#' @name touchstone_script
#' @section Requirements:
#'
#' A touchstone script must:
#'
#' * install all versions of the benchmarked repository with [refs_install()].
#' * create benchmarks with one or more calls to [benchmark_run_ref()].
#' * produce the artifacts required in the GitHub workflow with
#'   [benchmarks_analyze()].
NULL
