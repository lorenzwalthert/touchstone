#' Checks out a source branch and install the package
#' @param repo The path to the repository to install.
#' @param ref A reference to a git commit. Currently, only branch names are
#'   supported.
#' @param install_dependencies Passed to [devtools::install()]. Set to `FALSE`
#'   can help when ran locally without internet connection.
#' @param install_quick Passed to [devtools::install()] as `quick`.
#' @inheritParams devtools::install
#' @keywords internal
benchmark_iteration_prepare <- function(ref = "master",
                                        path_pkg = ".",
                                        install_quick = TRUE,
                                        install_dependencies = FALSE) {
  gert::git_branch_checkout(ref, repo = path_pkg)
  usethis::ui_done("Switched to branch {ref}.")
  devtools::install(path_pkg,
    upgrade = "never", quiet = TRUE, quick = install_quick,
    dependencies = install_dependencies
  )
  usethis::ui_done("Installed branch {ref}.")
}
