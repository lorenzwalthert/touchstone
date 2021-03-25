#' Checks out a source branch and install the package
#' @param path_pkg The path to the repository to install.
#' @param ref A reference to a git commit. Currently, only branch names are
#'   supported.
#' @param install_dependencies Passed to [remotes::install_local()]. Set to
#'   `FALSE` can help when ran locally without internet connection.
#' @keywords internal
benchmark_ref_install <- function(ref = "master",
                                  path_pkg = ".",
                                  install_dependencies = FALSE) {
  local_git_checkout(ref, path_pkg)
  if (getOption("touchstone.skip_install", FALSE)) {
    usethis::ui_info(
      "R option `touchstone.skip_install` is set, skipping installation."
    )
  } else {
    libpath <- c(
      .libPaths(),
      fs::dir_create(libpath_touchstone(ref))
    )
    withr::local_libpaths(libpath)
    remotes::install_local(path_pkg,
      upgrade = "never", quiet = TRUE,
      dependencies = install_dependencies
    )
    usethis::ui_done("Installed branch {ref} into {libpath}.")
  }
  libpath
}


libpath_touchstone <- function(ref) {
  fs::path("touchstone", "lib", ref)
}
