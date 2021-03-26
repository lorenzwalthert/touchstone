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


#' @describeIn touchstone_managers clears the touchstone database.
#' @aliases touchstone_managers
#' @param all Whether to clear the whole touchstone directory or just the
#'   records sub directory.
#' @export
touchstone_clear <- function(all = FALSE) {
  paths <- fs::path(dir_touchstone(), if (!all) "records" else "")

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

#' Make sure there is no installation of the package to benchmark in the global
#' package library
#' @keywords internal
assert_no_global_installation <- function(path_pkg = ".") {
  path_desc <- fs::path(path_pkg, "DESCRIPTION")
  pkg_name <- unname(read.dcf(path_desc)[, "Package"])
  if (rlang::is_installed(pkg_name)) {
    usethis::ui_stop(paste0(
      "Package {pkg_name} can be found on library path. This should not be ",
      "the case - as the package is installed in dedicated library paths ",
      "during benchmarking."
    ))
  }
}
