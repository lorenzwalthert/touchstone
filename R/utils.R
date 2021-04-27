#' Touchstone managers
#'
#' Utilities to manage the touchstone database.
#' @name touchstone_managers
NULL

#' @describeIn touchstone_managers returns the directory where the touchstone database lives.
#' @aliases touchstone_managers
#' @export
dir_touchstone <- function() {
  "touchstone"
}


path_touchstone_script <- function(root = ".") {
  fs::path(root, "touchstone", "script.R")
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
#' @keywords internal
exprs_eval <- function(...) {
  eval(parse(text = unlist(rlang::list2(...))))
}

ref_upsample <- function(ref, n = 20) {
  ref <- unique(ref)
  sample(rep(ref, length.out = n))
}

ensure_dir <- function(...) {
  fs::dir_create(...)
}



schema_disk <- function() {
  c(
    elapsed = "numeric", iteration = "integer", ref = "character",
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

#' Temporarily set the working directory to a temp directory
#' @keywords internal
local_tempdir_setwd <- function(.local_envir = parent.frame()) {
  withr::local_dir(
    withr::local_tempdir(.local_envir = .local_envir),
    .local_envir = .local_envir
  )
}


#' Temporarily remove all touchstone libraries from the path
#'
#' This is useful in conjunction with [with_touchstone_lib()].
#' @param path_pkg The path to the package that contains the touchstone library.
#' @param envir The environment that triggers the deferred action on
#'   destruction.
#' @details
#' * Add a touchstone library to the path with [with_touchstone_lib()] and
#'   run a script. The script hence may contain calls to libraries only installed
#'   in touchstone libraries.
#' * benchmark code with [benchmark_run_ref()]. At the start, remove all
#'   all touchstone libraries from path and add the touchstone library we need.
#'
#' Advantages: Keep benchmarked repo in touchstone library only.
#' @keywords internal
local_without_touchstone_lib <- function(path_pkg = ".", envir = parent.frame()) {
  all <- .libPaths()
  is_touchstone <- fs::path_has_parent(all, fs::path_abs(dir_touchstone()))
  all_but_touchstone <- all[!is_touchstone]
  withr::local_libpaths(all_but_touchstone, .local_envir = envir)
}


#' Make sure there is no installation of the package to benchmark in the global
#' package library
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
#' @keywords internal
is_installed <- function(path_pkg = ".") {
  path_desc <- fs::path(path_pkg, "DESCRIPTION")
  pkg_name <- unname(read.dcf(path_desc)[, "Package"])
  list(
    name = pkg_name,
    installed = pkg_name %in% rownames(utils::installed.packages())
  )
}
