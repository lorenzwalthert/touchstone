#' Touchstone managers
#'
#' Utilities to manage the touchstone database.
#' @name touchstone_managers
NULL

#' @describeIn touchstone_managers returns the directory where the touchstone database lives.
#' @aliases touchstone_managers
#' @return
#' Character vector of length one with th path to the touchstone directory.
#' @export
dir_touchstone <- function() {
  getOption("touchstone.dir", "touchstone")
}

#' Get the ref from the environment variable or fail if not set
#'
#' This function is only exported because it is a default argument.
#' @param var The environment variable to retrieve.
#' @return
#' Returns a character vector of length one with the `ref` retrieved from the
#' environment variable `var`.
#' @export
ref_get_or_fail <- function(var) {
  retrieved <- Sys.getenv(var)
  if (!nzchar(retrieved)) {
    rlang::abort(paste0(
      "If you don't specify the argument `ref(s)`, you must set the environment ",
      "variable `", var, "` to tell {touchstone} ",
      "which branches you want to benchmark against each other, see ",
      "help(run_script, package = 'touchstone')."
    ))
  } else {
    retrieved
  }
}

path_touchstone_script <- function() {
  fs::path(dir_touchstone(), "script.R")
}

#' @describeIn touchstone_managers clears the touchstone database.
#' @aliases touchstone_managers
#' @param all Whether to clear the whole touchstone directory or just the
#'   records sub directory.
#' @return
#' The deleted paths (invisibly).
#' @export
touchstone_clear <- function(all = FALSE) {
  paths <- fs::path(dir_touchstone(), if (!all) c("records", "lib") else "")

  paths <- paths[fs::dir_exists(paths)]
  fs::dir_delete(paths)
}

#' Evaluate an expression for sideeffects
#'
#'
#' @param ... Character vector  of length 1 or expression with code to evaluate. This will be quoted using
#' [rlang::enexprs()], so you can use `!!`.
#' @param env Environment in which the expression will be evaluated.
#' @return The quoted input (invisibly).
#' @keywords internal
exprs_eval <- function(..., env = parent.frame()) {
  expr <- rlang::enexprs(...)[[1]]

  if (is.symbol(expr)) {
    expr <- rlang::eval_tidy(expr, env = env)
  }

  if (is.character(expr)) {
    expr <- rlang::parse_exprs(expr)
  }

  if (is.list(expr)) {
    purrr::map(expr, eval, envir = env)
  } else {
    eval(expr, envir = env)
  }

  invisible(expr)
}

#' Samples `ref`
#'
#' A block is a permutation of all unique elements in `ref`. Then, we sample
#' `n` blocks. This is better than repeating one sample a certain number of
#' times because if compute resources steadily increase, the first sample will
#' always perform worse than the second, so the order within the blocks must be
#' random.
#' @keywords internal
ref_upsample <- function(ref, n = 20) {
  purrr::map_dfr(
    rlang::seq2(1, n),
    ~ tibble::tibble(block = .x, ref = sample(unique(ref)))
  )
}

ensure_dir <- function(...) {
  fs::dir_create(...)
}



schema_disk <- function() {
  c(
    elapsed = "numeric", iteration = "integer", ref = "character",
    block = "integer",
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


#' Temporarily remove all touchstone libraries from the path
#'
#' This is useful in conjunction with [run_script()].
#' @param path_pkg The path to the package that contains the touchstone library.
#' @param envir The environment that triggers the deferred action on
#'   destruction.
#' @details
#' * Add a touchstone library to the path with [run_script()] and
#'   run a script. The script hence may contain calls to libraries only installed
#'   in touchstone libraries.
#' * benchmark code with [benchmark_run_ref()]. At the start, remove all
#'   all touchstone libraries from path and add the touchstone library we need.
#'
#' Advantages: Keep benchmarked repo in touchstone library only.
#' @keywords internal
local_without_touchstone_lib <- function(path_pkg = ".", envir = parent.frame()) {
  all <- normalizePath(.libPaths())
  is_touchstone <- fs::path_has_parent(
    all, normalizePath(fs::path_abs(dir_touchstone()), mustWork = FALSE)
  )
  all_but_touchstone <- all[!is_touchstone]
  withr::local_libpaths(all_but_touchstone, .local_envir = envir)
}


#' Make sure there is no installation of the package to benchmark in the global
#' package library
#' @keywords internal
assert_no_global_installation <- function(path_pkg = ".") {
  local_without_touchstone_lib()
  check <- is_installed(path_pkg)
  if (check$installed) {
    usethis::ui_stop(paste0(
      "Package {check$name} can be found on a non-touchstone library path. ",
      "This should not be the case - as the package should be installed in ",
      "dedicated library paths for benchmarking."
    ))
  }
}


#' Check if a package is installed and unloading it
#' @keywords internal
is_installed <- function(path_pkg = ".") {
  path_desc <- fs::path(path_pkg, "DESCRIPTION")
  pkg_name <- unname(read.dcf(path_desc)[, "Package"])
  list(
    name = pkg_name,
    installed = pkg_name %in% rownames(utils::installed.packages())
  )
}

is_windows <- function() {
  identical(.Platform$OS.type, "windows")
}

#' Pin asset directory
#'
#' @description Add directories that need to be available on both branches
#'    when running`script.R`. During [benchmark_run_ref] they will be placed
#'     in the same directory as `script.R`.
#' @param ... A number of directories, as strings in relation to the current
#'   working directory, that contain scripts you want to source in `script.R`.
#' @return The temp dir invisibly.
#' @examples
#' \dontrun{
#' # In script.R
#' pin_head_asssets(c("bench", "inst/scripts"))
#'
#' source("scripts/setup.R")
#'
#' touchstone::benchmark_run_ref(
#'   expr_before_benchmark = {
#'     !!setup
#'     source("bench/exprs.R")
#'   },
#'   run_me = some_exprs(),
#'   n = 6
#' )
#' }
#' @export
pin_head_asssets <- function(...) {
  temp_dir <- getOption("touchstone.dir_assets_head")

  if (is.null(temp_dir)) {
    usethis::ui_stop(c(
      "Temporary directory not found. ",
      paste0(
        "This function is only for use within 'script.R',",
        " which must be called with 'run_script'"
      )
    ))
  }

  dirs <- rlang::list2(...)

  valid_dirs <- dirs %>% purrr::map_lgl(fs::is_dir)

  if (!all(valid_dirs)) {
    usethis::ui_warn(c(
      "The following path(s) could not be found",
      " and will not be copied:",
      usethis::ui_path(unlist(dirs[!valid_dirs]))
    ))
  }

  dirs[valid_dirs] %>% purrr::map(~ fs::dir_copy(.x,
    fs::path_join(c(temp_dir, fs::path_file(.x))),
    overwrite = TRUE
  ))

  if (any(valid_dirs)) {
    usethis::ui_done(c(
      paste0(
        "Pinned the following asset directories ",
        "to make them available across branch checkouts:"
      ),
      usethis::ui_path(as.character(dirs[valid_dirs]))
    ))
  } else {
    usethis::ui_oops("No valid asset directories found.")
  }

  invisible(temp_dir)
}

#' Get path to asset
#'
#' @description This function is used to get the absolut path to a pinned asset
#' within `script.R`.
#' @inheritParams fs::path
#' @param ref Currently without function as assets can only be oinned from the
#' head branch. Introduced for API stability.
#' @return The absolute path to the asset.
#' @export
path_pinned_asset <- function(...,
                              ref = ref_get_or_fail("GITHUB_HEAD_REF")) {


  # to be consistent in API, we use `ref` as argument, with possible values for the branches, but internally with the
  # R option, we only have head and base, so we need to convert.
  if (ref == ref_get_or_fail("GITHUB_HEAD_REF")) {
    ref <- "head"
  } else if (ref == ref_get_or_fail("GITHUB_BASE_REF")) {
    ref <- "base"
  } else {
    usethis::ui_stop("Can only find pinned assets for head or base refs!")
  }

  asset_dir <- getOption(paste0("touchstone.dir_assets_", ref))

  if (is.null(asset_dir)) {
    usethis::ui_stop(c(
      "Temporary directory for ref {ref} not found.",
      paste0(
        "This function is only for use within 'script.R',",
        " which must be called with 'run_script'"
      )
    ))
  }

  path <- fs::path(asset_dir, ...)
  if (!fs::path_exists(path)) {
    usethis::ui_stop("Asset {fs::path(...)} not pinned at {ref}.")
  }

  path
}
