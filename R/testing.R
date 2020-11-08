#' Clean up
#'
#' Deletes [dir_touchstone()] when the local frame is destroyed.
#' @inheritParams withr::defer
#' @family testers
local_clean_touchstone <- function(envir = parent.frame()) {
  withr::defer(touchstone_clear(), envir = envir)
}

path_temp_pkg <- function(name) {
  fs::path(tempdir(), digest::digest(Sys.time()), name)
}


#' Create a test package
#'
#' Creates a package in a temporary directory until the local frame is
#' destroyed.
#' @param path The path to the temporary package.
#' @param branches Branches to be created.
#' @inheritParams withr::defer
#' @family testers
local_package <- function(path = path_temp_pkg("testpkg"),
                          branches = c("main", "devel"),
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
  gert::git_commit("[init]", repo = path)
  purrr::walk(branches, gert::git_branch_create, repo = path)
  withr::defer(unlink(path), envir = envir)
  path
}
