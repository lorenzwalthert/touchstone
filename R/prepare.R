#' Checks out a source branch and install the package
#'
#' @param path_pkg The path to the repository to install.
#' @param ref A reference to a git commit. Currently, only branch names are
#'   supported.
#' @param install_dependencies Passed to [remotes::install_local()]. Set to
#'   `FALSE` can help when ran locally without internet connection.
#' @return
#' A character vector with library paths.
#' @keywords internal
ref_install <- function(ref = "master",
                        path_pkg = ".",
                        install_dependencies = FALSE) {
  local_git_checkout(ref, path_pkg)
  if (getOption("touchstone.skip_install", FALSE)) {
    usethis::ui_info(
      "R option `touchstone.skip_install` is set, skipping installation."
    )
    NULL
  } else {
    libpath <- c(
      fs::dir_create(libpath_touchstone(ref)),
      .libPaths()
    )
    withr::local_libpaths(libpath)
    withr::local_options(warn = 2)
    remotes::install_local(path_pkg,
      upgrade = "never", quiet = TRUE,
      dependencies = install_dependencies
    )
    usethis::ui_done("Installed branch {ref} into {libpath[1]}.")
    libpath
  }
}

#' Install branches
#'
#' Installs each `ref` in a separate library for isolation.
#' @param refs The names of the branches in a character vector.
#' @param install_dependencies Passed to ``
#' @inheritParams ref_install
#' @return
#' The global and touchstone library paths in a character vector (invisibly).
#' @export
refs_install <- function(refs = c(
                           ref_get_or_fail("GITHUB_BASE_REF"),
                           ref_get_or_fail("GITHUB_HEAD_REF")
                         ),
                         path_pkg = ".",
                         install_dependencies = FALSE) {
  force(refs)
  assert_no_global_installation(path_pkg)
  usethis::ui_info("Start installing branches into separate libraries.")
  libpaths <- purrr::map(refs, ref_install,
    path_pkg = path_pkg,
    install_dependencies = install_dependencies
  ) %>%
    purrr::flatten_chr() %>%
    unique() %>%
    fs::path_abs() %>%
    as.character() %>%
    sort()
  assert_no_global_installation(path_pkg)
  usethis::ui_done("Completed installations.")
  invisible(libpaths)
}



libpath_touchstone <- function(ref) {
  fs::path(dir_touchstone(), "lib", ref)
}