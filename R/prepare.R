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
ref_install <- function(ref = "main",
                        path_pkg = ".",
                        install_dependencies = FALSE) {
  local_git_checkout(ref, path_pkg)
  if (getOption("touchstone.skip_install", FALSE)) {
    cli::cli_alert_info(
      "R option {.envvar touchstone.skip_install} is set, skipping installation."
    )
    NULL
  } else {
    local_touchstone_libpath(ref)
    libpath <- .libPaths()
    install_local <- purrr::partial(remotes::install_local, path_pkg,
      upgrade = "never",
      dependencies = install_dependencies,
      force = !cache_up_to_date(ref, path_pkg)
    )
    install_missing_deps(path_pkg = path_pkg)
    withr::local_options(warn = 2)
    rlang::with_handlers(
      install_local(quiet = TRUE),
      error = function(e) {
        install_local(quiet = FALSE)
      }
    )
    cache_update(ref, path_pkg)
    cli::cli_alert_success("Installed branch {.val {ref}} into {.path {libpath[1]}}.")
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
  cli::cli_alert_info("Start installing branches into separate libraries.")
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
  cli::cli_alert_success("Completed installations.")
  invisible(libpaths)
}



libpath_touchstone <- function(ref) {
  fs::path(dir_touchstone(), "lib", ref)
}

#' When did the package sources change last?
#' @inheritParams ref_install
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
#' @inheritParams ref_install
#' @keywords internal
cache_up_to_date <- function(ref, path_pkg) {
  md5_hashes <- hash_pkg(path_pkg)
  cache <- cache_get()
  identical(md5_hashes, cache$md5_hashes[cache$ref == ref & cache$path_pkg == path_pkg])
}

#' @rdname cache_up_to_date
#' @keywords internal
cache_update <- function(ref, path_pkg) {
  md5_hashes <- hash_pkg(path_pkg)
  cache <- cache_get()
  stopifnot(sum(cache$ref[cache$path_pkg == path_pkg] == ref) <= 1)
  cache <- cache[(!(cache$ref == ref) & (cache$path_pkg == path_pkg)), ]
  cache <- vctrs::vec_rbind(
    cache, tibble::tibble(ref, md5_hashes, path_pkg)
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
install_missing_deps <- function(path_pkg) {
  remotes::install_deps(pkgdir = path_pkg, upgrade = "always")
}
