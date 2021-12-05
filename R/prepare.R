#' Checks out a source branch and install the package
#'
#' @param path_pkg The path to the repository to install.
#' @param branch The name of the branch which should be installed.
#' @param install_dependencies Passed to [remotes::install_local()]. Set to
#'   `FALSE` can help when ran locally without internet connection.
#' @return
#' A character vector with library paths.
#' @keywords internal
branch_install <- function(branch = "main",
                           path_pkg = ".",
                           install_dependencies = FALSE) {
  local_git_checkout(branch, path_pkg)
  if (getOption("touchstone.skip_install", FALSE)) {
    cli::cli_alert_info(
      "R option {.envvar touchstone.skip_install} is set, skipping installation."
    )
    NULL
  } else {
    local_touchstone_libpath(branch)
    libpath <- .libPaths()
    install_local <- purrr::partial(remotes::install_local, path_pkg,
      upgrade = "never",
      dependencies = install_dependencies,
      force = !cache_up_to_date(branch, path_pkg)
    )
    withr::local_options(warn = 2)
    rlang::with_handlers(
      {
        install_missing_deps(path_pkg = path_pkg, quiet = TRUE)
        install_local(quiet = TRUE)
      },
      error = function(e) {
        install_missing_deps(path_pkg = path_pkg, quiet = FALSE)
        install_local(quiet = FALSE)
      }
    )
    cache_update(branch, path_pkg)
    cli::cli_alert_success("Installed branch {.val {branch}} into {.path {libpath[1]}}.")
    libpath
  }
}

#' Install branches
#'
#' Installs each `branch` in a separate library for isolation.
#' @param branches The names of the branches in a character vector.
#' @param install_dependencies Passed to ``
#' @inheritParams branch_install
#' @return
#' The global and touchstone library paths in a character vector (invisibly).
#' @export
branch_install <- function(branches = c(
                             branch_get_or_fail("GITHUB_BASE_REF"),
                             branch_get_or_fail("GITHUB_HEAD_REF")
                           ),
                           path_pkg = ".",
                           install_dependencies = FALSE) {
  force(branches)
  assert_no_global_installation(path_pkg)
  cli::cli_alert_info("Start installing branches into separate libraries.")
  libpaths <- purrr::map(branches, branch_install,
    path_pkg = path_pkg,
    install_dependencies = install_dependencies
  ) %>%
    purrr::flatten_chr() %>%
    unique() %>%
    fs::path_abs() %>%
    as.character() %>%
    sort()
  assert_no_global_installation(path_pkg)
  cli::cli_alert_success("Completed installations.")
  invisible(libpaths)
}



libpath_touchstone <- function(branch) {
  fs::path(dir_touchstone(), "lib", branch)
}

#' When did the package sources change last?
#' @inheritParams branch_install
#' @keywords internal
hash_pkg <- function(path_pkg) {
  withr::local_dir(path_pkg)
  list(
    tools::md5sum(c(
      if (fs::dir_exists("R")) fs::dir_ls("R"),
      if (fs::file_exists("DESCRIPTION")) "DESCRIPTION",
      if (fs::dir_exists("scr")) fs::dir_info("scr")
    ))
  )
}

#' Cache package sources within a session
#'
#' This is required to make sure [remotes::install_local()] installs again
#' when source code changed.
#' @inheritParams branch_install
#' @keywords internal
cache_up_to_date <- function(branch, path_pkg) {
  md5_hashes <- hash_pkg(path_pkg)
  cache <- cache_get()
  identical(md5_hashes, cache$md5_hashes[cache$branch == branch & cache$path_pkg == path_pkg])
}

#' @rdname cache_up_to_date
#' @keywords internal
cache_update <- function(branch, path_pkg) {
  md5_hashes <- hash_pkg(path_pkg)
  cache <- cache_get()
  stopifnot(sum(cache$branch[cache$path_pkg == path_pkg] == branch) <= 1)
  cache <- cache[(!(cache$branch == branch) & (cache$path_pkg == path_pkg)), ]
  cache <- vctrs::vec_rbind(
    cache, tibble::tibble(branch, md5_hashes, path_pkg)
  )
  options("touchstone.hash_source_package" = cache)
}


#' @rdname cache_up_to_date
#' @keywords internal
cache_get <- function() {
  getOption("touchstone.hash_source_package")
}

#' Install missing BASE dependencies
#'
#' If the HEAD branch removes dependencies (compared to BASE), installing the
#' package from the BASE branch will fail due to missing dependencies, as
#' dependencies were installed in the GitHub Action based on the `DESCRIPTION`
#' of the HEAD branch. The simplest way to
#' ensure all required dependencies are present (including specification of
#' remotes) is by simply installing them
#' into the respective {touchstone} library. Prepend the local touchstone
#' library to the library path with [local_touchstone_libpath()].
#' @keywords internal
install_missing_deps <- function(path_pkg, quiet = FALSE) {
  remotes::install_deps(pkgdir = path_pkg, upgrade = "always", quiet = quiet)
}
