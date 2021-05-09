#' Clean up
#'
#' Deletes [dir_touchstone()] when the local frame is destroyed.
#' @inheritParams withr::defer
#' @family testers
#' @keywords internal
local_clean_touchstone <- function(envir = parent.frame()) {
  withr::defer(touchstone_clear(all = TRUE), envir = envir)
}

path_temp_pkg <- function(name) {
  fs::path(tempdir(), digest::digest(Sys.time()), name)
}


#' Create a test package
#'
#' Creates a package in a temporary directory and sets the working directory
#' until the local frame is destroyed.
#'
#' This is primarily for testing.
#' @param path The path to the temporary package.
#' @param branches Branches to be created.
#' @param r_sample Character with code to write to `R/sampleR.`. This is helpful
#'   to validate if the installed package corresponds to source branch for
#'   testing. If `NULL`, nothing is written.
#' @inheritParams withr::defer
#' @family testers
local_package <- function(pkg_name = "testpkg",
                          branches = c("main", "devel"),
                          r_sample = NULL,
                          setwd = TRUE,
                          envir = parent.frame()) {
  path <- fs::path(tempfile(""), pkg_name)
  fs::dir_create(path)
  withr::local_options(
    usethis.quiet = TRUE,
    touchstone.n_iterations = 2,
    .local_envir = envir
  )
  usethis::create_package(path, open = FALSE)

  withr::local_dir(path, .local_envir = if (setwd) envir else parent.frame())
  gert::git_init()
  gert::git_config_set("user.name", "GitHub Actions")
  gert::git_config_set("user.email", "actions@github.com")
  gert::git_add("DESCRIPTION")
  if (!is.null(r_sample)) {
    writeLines(r_sample, fs::path("R", "sample.R"))
  }
  gert::git_add("R/")
  gert::git_commit("[init]")
  purrr::walk(branches, gert::git_branch_create)
  withr::defer(unlink(path), envir = envir)
  install_check <- is_installed(path)
  if (install_check$installed) {
    withr::defer(utils::remove.packages(install_check$name), envir = envir)
  }
  path
}
