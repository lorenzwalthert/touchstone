#' Sources a script
#'
#' Basically [base::source()], but prepending the library path with a
#' touchstone library and running the script in a temp directory to avoid
#' git operations like checking out different branches to interfere with the
#' script execution (as running the script changes itself through git checkout).
#'
#' @param path The script to run. It must fulfill the requirements of a
#'   [touchstone_script].
#' @param ref The branch that corresponds to the library that should be prepended
#'   to the library path when the script at `path` is executed, see 'Why this
#'   function?' below.
#' @section How to run this interactively?:
#' You can use [activate()] to setup the environment to interactively run your
#'  script, as there are some adjustments needed to mirror the Github Action
#'  environment.
#' In a GitHub Action workflow, the environment variables `GITHUB_BASE_REF` and
#' `GITHUB_HEAD_REF` denote the target and source branch of the pull request -
#' and these are default arguments in [benchmark_run_ref()] (and other functions
#' you probably want to call in your benchmarking script) to determinate the
#' branches to use.
#' @section Why this function?:
#' For isolation, \{touchstone\} does not allow the benchmarked package to be
#' installed in the global package library, but only in touchstone libraries, as
#' asserted with [assert_no_global_installation()]. However, this also implies
#' that the package is not available in the touchstone script outside of
#' benchmark runs (i.e. outside of [benchmark_run_ref()]. We sometimes still
#' want to call that package to prepare a benchmarking run though. To
#' allow this, we prepend a touchstone library location that
#' contains the installed benchmarked package for set-up tasks, and temporarily
#' remove it during benchmarking with [benchmark_run_ref()] so only one
#' touchstone library is on the library path at any time.
#' @return
#' The same as [base::source()], which inherits from [base::withVisible()], i.e.
#' a list with `value` and `visible` (invisibly).
#' @export
#' @examples
#' \dontrun{
#' # assuming you want to compare the branch main with the branch devel
#' if (rlang::is_installed("withr")) {
#'   withr::with_envvar(
#'     c("GITHUB_BASE_REF" = "main", "GITHUB_HEAD_REF" = "devel"),
#'     run_script("touchstone/script.R")
#'   )
#' }
#' }
run_script <- function(path = "touchstone/script.R",
                       ref = ref_get_or_fail("GITHUB_HEAD_REF")) {
  activate(ref, ref_get_or_fail("GITHUB_BASE_REF"))

  temp_file <- fs::file_temp()
  fs::file_copy(path, temp_file)

  cli::cli_alert_success(paste0(
    "Copied touchstone script to tempdir to prevent branch checkouts to effect",
    " the script."
  ))

  source(temp_file, max.deparse.length = Inf, local = TRUE)
}

#' Activate touchstone environment
#'
#' This sets environment variables, R options and library paths to work
#' interactively on the [touchstone_script].
#' @param head_ref Git ref to be used as the `GITHUB_HEAD_REF` ref (i.e. the
#'   branch with new changes) when running benchmarks. Defaults to the current
#'   branch.
#' @param base_ref Git ref for the `GITHUB_BASE_REF` (i.e. the branch you want
#'   to merge your changes into) when running benchmarks.
#'   Defaults to 'main' if the option `touchstone.default_base_ref`is not set.
#' @param env In which environment the temporary changes should be made.
#'   For use within functions.
#' @examples
#' \dontrun{
#' activate()
#' # You can now test parts of your touchstone script, e.g. touchstone/script.R
#' deactivate()
#' }
#' @export
activate <- function(head_ref = gert::git_branch(),
                     base_ref = getOption(
                       "touchstone.default_base_ref",
                       "main"
                     ),
                     env = parent.frame()) {
  suppressMessages({
    withr::local_envvar(
      GITHUB_BASE_REF = base_ref,
      GITHUB_HEAD_REF = head_ref,
      .local_envir = env
    )

    local_touchstone_libpath(head_ref, env = env)
    local_asset_dir(base_ref, head_ref, env = env)
  })

  if (identical(env, .GlobalEnv)) {
    cli::cli_alert_success(
      "Environment ready to interactivley execute your touchstone script."
    )
    cli::cli_alert_info(
      "Use {.fun touchstone::deactivate} to restore original environment."
    )
  }
}

#' Set Library Path
#'
#' Temporarily add a touchstone library to the path, so it can be found by
#' [.libPaths()] and friends. Can be used in [touchstone_script]
#'  to prepare benchmarks etc. If there are touchstone libraries on the path
#'  when this function is called, they will be removed.
#' @param ref Git ref to use, e.g. HEAD or BASE ref.
#' @param env Environment in which the change should be applied.
#' @seealso [run_script()]
local_touchstone_libpath <- function(ref, env = parent.frame()) {
  lib <- libpath_touchstone(ref)
  fs::dir_create(lib)
  current <- fs::path_real(.libPaths())
  print("current libpaths\n")
  print(current, sep = "\n")
  print("\n")

  current_is_touchstone <- purrr::map_lgl(current,
    fs::path_has_parent,
    parent = fs::path_real(dir_touchstone())
  )
  current <- current[!current_is_touchstone]
  print("short list\n")
  print(current)
  print("\n")
  out <- withr::local_libpaths(
    list(lib),
    action = "replace",
    .local_envir = env
  )
  print("new list:\n")
  print(.libPaths(), sep = "\n")
  print("\n")
  out
}

#' @describeIn activate Restore the original environment state.
#' @export
deactivate <- function(env = parent.frame()) {
  withr::deferred_clear(envir = env)
  cli::cli_alert_success("Original environment restored!")
}


#' The script for benchmarking
#'
#' The script that contains the code which executes the benchmark. It is
#' typically called with [run_script()].
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
