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
    cli::cli_abort(c(paste0(
      "If you don't specify the argument {.arg ref(s)}, you must set the environment ",
      "variable {.envvar {var}} to tell {.pkg touchstone} ",
      "which branches you want to benchmark against each other."
    ),
    "i" = "See {.code ?touchstone::run_script}."
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
    cli::cli_abort("Branch {.val {branch}} does not exist, create it and add commits before you can switch on it.")
  }
  gert::git_branch_checkout(branch, repo = path_pkg)
  cli::cli_alert_success("Temporarily checked out branch {.val {branch}}.")
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
    cli::cli_abort(c(
      "Package {.pkg {check$name}} can be found on a non-touchstone library path. ",
      "!" = paste0(
        "This should not be the case - as the package should be installed in ",
        "dedicated library paths for benchmarking."
      ),
      "*" = 'To uninstall use {.code remove.packages("{check$name}")}.'
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
    installed = rlang::is_installed(pkg_name)
  )
}

is_windows <- function() {
  identical(.Platform$OS.type, "windows")
}

#' Pin asset directory
#'
#' Pin files or directories that need to be available on both branches when
#' running the `touchstone_script`. During [benchmark_run_ref()] they will
#' available via [path_pinned_asset()].
#' @param ... Any number of directories or files, as strings, that you want to
#'   access in your [touchstone_script].
#' @param ref The branch the passed assets are copied from.
#' @inheritParams fs::path
#' @inheritParams fs::dir_copy
#' @details When passing nested directories or files within nested directories
#'   only the file/last directory will be copied. Directories will be copied
#'   recursively. See examples.
#' @return The asset directory invisibly.
#' @examples
#' \dontrun{
#' # In the touchstone script
#'
#' pin_assets(c("bench", "inst/setup.R", "some/nested/dir"))
#'
#' source(path_pinned_asset("setup.R"))
#' load(path_pinned_asset("dir/data.RData"))
#'
#' touchstone::benchmark_run_ref(
#'   expr_before_benchmark = {
#'     !!setup
#'     source(path_pinned_asset("bench/exprs.R"))
#'   },
#'   run_me = some_exprs(),
#'   n = 6
#' )
#' }
#' @export
pin_assets <- function(...,
                       ref = ref_get_or_fail("GITHUB_HEAD_REF"),
                       overwrite = TRUE) {
  asset_dir <- get_asset_dir(ref, "pin")

  local_git_checkout(ref)
  dirs <- rlang::list2(...)
  valid_dirs <- dirs %>% purrr::map_lgl(fs::file_exists)

  if (!all(valid_dirs)) {
    cli::cli_warn(paste0(
      "The following asset{?s} could not be found and will ",
      "not be copied: {.path {dirs[!valid_dirs]}}"
    ))
  }

  dirs[valid_dirs] %>% purrr::walk(
    ~ purrr::when(
      .x,
      fs::is_dir(.)[[1]] ~ fs::dir_copy(.x,
        fs::path_join(c(asset_dir, fs::path_file(.x))),
        overwrite = overwrite
      ),
      fs::is_file(.)[[1]] ~ fs::file_copy(.x,
        fs::path_join(c(asset_dir, fs::path_file(.x))),
        overwrite = overwrite
      )
    )
  )

  if (any(valid_dirs)) {
    cli::cli_alert_success(
      paste0(
        "Pinned the following asset{?s} ",
        "to make {?it/them} available across branch checkouts: ",
        "{.path {dirs[valid_dirs]}}"
      )
    )
  } else {
    cli::cli_abort("No valid assets found.")
  }

  invisible(asset_dir)
}

#' Get path to asset
#'
#' Get the path to a pinned asset within a [touchstone_script].
#' @inheritParams fs::path
#' @param ref The branch the passed asset was copied from.
#' @return The absolute path to the asset.
#' @seealso [pin_assets()]
#' @export
path_pinned_asset <- function(...,
                              ref = ref_get_or_fail("GITHUB_HEAD_REF")) {
  asset_dir <- get_asset_dir(ref)

  path <- fs::path(asset_dir, ...)
  if (!fs::file_exists(path)) {
    cli::cli_abort("Asset {.path {fs::path(...)}} not pinned at {.val {ref}}.")
  }

  path
}

get_asset_dir <- function(ref, verb = "find") {
  if (ref == ref_get_or_fail("GITHUB_HEAD_REF")) {
    ref <- "head"
  } else if (ref == ref_get_or_fail("GITHUB_BASE_REF")) {
    ref <- "base"
  } else {
    cli::cli_abort("Can only {verb} assets for head or base refs!")
  }

  asset_dir <- getOption(paste0("touchstone.dir_assets_", ref))

  if (is.null(asset_dir)) {
    cli::cli_abort(c(
      "Temporary directory not found. ",
      "i" = paste0(
        "This function is only for use within the {.code ?touchstone_script},",
        " which must be called with {.fun run_script}"
      )
    ))
  }

  asset_dir
}


#' @describeIn touchstone_managers returns the path to the file containing the pr comment.
#' @aliases touchstone_managers
#' @return
#' Character vector of length one with the path to the pr comment.
#' @export
#' @seealso [pr_comment]
path_pr_comment <- function() {
  fs::path(dir_touchstone(), "pr-comment/info.txt")
}
