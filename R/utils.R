#' Touchstone managers
#'
#' Utilities to manage the touchstone database.
#' @name touchstone_managers
NULL

#' @describeIn touchstone_managers returns the directory where the touchstone database lives.
#' @aliases touchstone_managers
#' @return
#' Character vector of length one with the path to the touchstone directory (for
#' `dir_touchstone()`), path to the deleted files for (`touchstone_clear()`).
#' @export
dir_touchstone <- function() {
  getOption("touchstone.dir", "touchstone")
}

#' Get the ref from the environment variable or fail if not set
#'
#' This function is only exported because it is a default argument.
#' @param var The environment variable to retrieve.
#' @return
#' Returns a character vector of length one with the `ref` retrieved from the
#' environment variable `var`.
#' @export
ref_get_or_fail <- function(var) {
  retrieved <- Sys.getenv(var)
  if (!nzchar(retrieved)) {
    rlang::abort(paste0(
      "If you don't specify the argument `ref(s)`, you must set the environment ",
      "variable `", var, "` to tell {touchstone} ",
      "which branches you want to benchmark against each other, see ",
      "help(run_script, package = 'touchstone')."
    ))
  } else {
    retrieved
  }
}

path_touchstone_script <- function() {
  fs::path(dir_touchstone(), "script.R")
}

#' @describeIn touchstone_managers clears the touchstone database.
#' @aliases touchstone_managers
#' @param all Whether to clear the whole touchstone directory or just the
#'   records sub directory.
#' @export
touchstone_clear <- function(all = FALSE) {
  paths <- fs::path(dir_touchstone(), if (!all) c("records", "lib") else "")

  paths <- paths[fs::dir_exists(paths)]
  fs::dir_delete(paths)
}

#' Evaluate an expression
#'
#' @param text Character vector with code to evaluate.
#' @return
#' The input, parsed and evaluated.
#' @keywords internal
exprs_eval <- function(...) {
  eval(parse(text = unlist(rlang::list2(...))))
}

#' Samples `ref`
#'
#' A block is a permutation of all unique elements in `ref`. Then, we sample
#' `n` blocks. This is better than repeating one sample a certain number of
#' times because if compute resources steadily increase, the first sample will
#' always perform worse than the second, so the order within the blocks must be
#' random.
#' @keywords internal
#' @return
#' A tibble with columns `block` and `ref`.
ref_upsample <- function(ref, n = 20) {
  purrr::map_dfr(
    rlang::seq2(1, n),
    ~ tibble::tibble(block = .x, ref = sample(unique(ref)))
  )
}

ensure_dir <- function(...) {
  fs::dir_create(...)
}



schema_disk <- function() {
  c(
    elapsed = "numeric", iteration = "integer", ref = "character",
    block = "integer",
    name = "character"
  )
}


local_git_checkout <- function(branch,
                               path_pkg = ".",
                               envir = parent.frame()) {
  current_branch <- gert::git_branch(repo = path_pkg)
  withr::defer(
    gert::git_branch_checkout(current_branch, repo = path_pkg),
    envir = envir
  )
  if (!(branch %in% gert::git_branch_list(repo = path_pkg)$name)) {
    usethis::ui_stop("Branch {branch} does not exist, create it and add commits before you can switch on it.")
  }
  gert::git_branch_checkout(branch, repo = path_pkg)
  usethis::ui_done("Temporarily checked out branch {branch}.")
}


#' Temporarily remove all touchstone libraries from the path
#'
#' This is useful in conjunction with [run_script()].
#' @param path_pkg The path to the package that contains the touchstone library.
#' @param envir The environment that triggers the deferred action on
#'   destruction.
#' @details
#' * Add a touchstone library to the path with [run_script()] and
#'   run a script. The script hence may contain calls to libraries only installed
#'   in touchstone libraries.
#' * benchmark code with [benchmark_run_ref()]. At the start, remove all
#'   all touchstone libraries from path and add the touchstone library we need.
#'
#' Advantages: Keep benchmarked repo in touchstone library only.
#' @keywords internal
#' @return
#' The old library paths (invisibly).
local_without_touchstone_lib <- function(path_pkg = ".", envir = parent.frame()) {
  all <- normalizePath(.libPaths())
  is_touchstone <- fs::path_has_parent(
    all, normalizePath(fs::path_abs(dir_touchstone()), mustWork = FALSE)
  )
  all_but_touchstone <- all[!is_touchstone]
  withr::local_libpaths(all_but_touchstone, .local_envir = envir)
}


#' Make sure there is no installation of the package to benchmark in the global
#' package library
#' @param path_pkg The path to the package to check.
#' @return
#' `TRUE` invisibly or an error if there is a global installation.
#' @keywords internal
assert_no_global_installation <- function(path_pkg = ".") {
  local_without_touchstone_lib()
  check <- is_installed(path_pkg)
  if (check$installed) {
    usethis::ui_stop(paste0(
      "Package {check$name} can be found on a non-touchstone library path. ",
      "This should not be the case - as the package should be installed in ",
      "dedicated library paths for benchmarking."
    ))
  }
}


#' Check if a package is installed and unloading it
#' @return
#' A list with `name` of the package to check and `installed`, a boolean to
#' indicate if the package is installed or not.
#' @keywords internal
is_installed <- function(path_pkg = ".") {
  path_desc <- fs::path(path_pkg, "DESCRIPTION")
  pkg_name <- unname(read.dcf(path_desc)[, "Package"])
  list(
    name = pkg_name,
    installed = is_installed_impl(pkg_name)
  )
}

# When a package is removed from the library with [remove.packages()] and then
# loaded e.g. with [devtools::load_all()],
# [rlang::is_installed()] and similar returns `TRUE`,
# `pkg_name %in% installed.packages()` `FALSE` (but CRAN does not want to see
# `installed.packages()` used since it may be slow).
is_installed_impl <- function(pkg_name) {
  rlang::with_handlers(
    {
      find.package(pkg_name, lib.loc = .libPaths())
      TRUE
    },
    error = ~FALSE
  )
}

is_windows <- function() {
  identical(.Platform$OS.type, "windows")
}
