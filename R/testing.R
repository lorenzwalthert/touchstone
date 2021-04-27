#' Clean up
#'
#' Deletes [dir_touchstone()] when the local frame is destroyed.
#' @inheritParams withr::defer
#' @family testers
#' @keywords internal
local_clean_touchstone <- function(envir = parent.frame()) {
  withr::defer(touchstone_clear(all = TRUE), envir = envir)
}

#' Combines a few options for a good testing environment
#'
#' @param envir The environment that determines when the deferred actions will
#'   manifest.
#' @param keywords internal
#' @details
#' Setup includes:
#'
#' * temporarily disables usethis communication.
#' * runs touchstone for only two iterations for speeding things up.
#' * remove the touchstone env with [local_clean_touchstone()].
#' @keywords internal
local_test_setup <- function(envir = parent.frame()) {
  withr::local_options(
    usethis.quiet = TRUE,
    touchstone.n_iterations = 2,
    .local_envir = envir
  )
  local_tempdir_setwd(envir)
  local_clean_touchstone(envir)
}

path_temp_pkg <- function(name) {
  fs::path(tempdir(), digest::digest(Sys.time()), name)
}


#' Create a test package
#'
#' Creates a package in a temporary directory until the local frame is
#' destroyed.
#'
#' This is primarily for testing.
#' @param path The path to the temporary package.
#' @param branches Branches to be created.
#' @param r_sample Character with code to write to `R/sampleR.`. This is helpful
#'   to validate if the installed package corresponds to source branch for
#'   testing. If `NULL`, nothing is written.
#' @inheritParams withr::defer
#' @family testers
local_package <- function(path = path_temp_pkg("testpkg"),
                          branches = c("main", "devel"),
                          r_sample = NULL,
                          envir = parent.frame()) {
  fs::dir_create(fs::path_dir(path))
  withr::with_options(
    list(usethis.quiet = TRUE),
    usethis::create_package(path, open = FALSE) #
  )
  gert::git_init(path)
  gert::git_config_set("user.name", "GitHub Actions", repo = path)
  gert::git_config_set("user.email", "actions@github.com", repo = path)
  gert::git_add("DESCRIPTION", repo = path)
  if (!is.null(r_sample)) {
    writeLines(r_sample, fs::path(path, "R", "sample.R"))
  }
  gert::git_add("R/", repo = path)
  gert::git_commit("[init]", repo = path)
  purrr::walk(branches, gert::git_branch_create, repo = path)
  withr::defer(unlink(path), envir = envir)
  install_check <- is_installed(path)
  if (install_check$installed) {
    withr::defer(utils::remove.packages(install_check$name), envir = envir)
  }
  path
}
