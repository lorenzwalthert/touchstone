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
    remotes::install_local(path_pkg,
      upgrade = "never", quiet = TRUE,
      dependencies = install_dependencies,
      force = !cache_up_to_date(ref, path_pkg)
    )
    cache_update(ref, path_pkg)
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

#' When did the package sources change last?
#'
#' @keywords internal
hash_pkg <- function() {
  list(
    tools::md5sum(c(
      fs::dir_ls("R"),
      "DESCRIPTION",
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
  md5_hashes <- hash_pkg()
  cache <- cache_get()
  identical(md5_hashes, cache$md5_hashes[cache$ref == ref & cache$path_pkg == path_pkg])
}

#' @rdname cache_up_to_date
#' @keywords internal
cache_update <- function(ref, path_pkg) {
  md5_hashes <- hash_pkg()
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
