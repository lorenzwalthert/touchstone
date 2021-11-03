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
    cli::cli_alert_info(c(paste0(
      "touchstone not activated. Most likely, you want to run ",
      "{.code touchstone::activate()} if you are working interactivley and ",
      "the below error should go away."
    )))
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
    {
      gert::git_branch_checkout(current_branch, repo = path_pkg)
      cli::cli_alert_success("Switched back to branch {.val {current_branch}}.")
    },
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
      "*" = 'To uninstall use {.code remove.packages("{check$name}", lib = "{.libPaths()[1]}")}.'
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
#' Pin files or directories that need to be available on both branches when
#' running the [touchstone_script]. During [benchmark_run_ref()] they will
#' available via [path_pinned_asset()]. This is only possible for assets
#'  *within* the git repository.
#' @param ... Any number of directories or files, as strings, that you want to
#'   access in your [touchstone_script].
#' @param ref The branch the passed assets are copied from.
#' @inheritParams fs::path
#' @inheritParams fs::dir_copy
#' @details When passing nested directories or files within nested directories
#'   the path will be copied recursively. See examples.
#' @return The asset directory invisibly.
#' @examples
#' \dontrun{
#' # In the touchstone script within the repo "/home/user/pkg"
#'
#' pin_assets(c("/home/user/pkg/bench", "inst/setup.R", "some/nested/dir"))
#'
#' source(path_pinned_asset("inst/setup.R"))
#' load(path_pinned_asset("some/nested/dir/data.RData"))
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
  asset_dir <- get_asset_dir(ref)

  local_git_checkout(ref)
  dirs <- rlang::list2(...)
  valid_dirs <- dirs %>% purrr::map_lgl(fs::file_exists)

  if (!all(valid_dirs)) {
    cli::cli_warn(paste0(
      "The following asset{?s} could not be found and will ",
      "not be copied: {.path {dirs[!valid_dirs]}}"
    ))
  }

  create_and_copy <- function(asset) {
    git_root <- fs::path_real(get_git_root())

    asset <- fs::path_real(asset)
    if (!fs::path_has_parent(asset, git_root)) {
      cli::cli_abort(c(
        "Can only pin assets within the git repository!",
        "i" = "Current repo: {.path {git_root}}"
      ))
    }

    rel_asset <- fs::path_rel(asset, git_root)
    new_path <- fs::path_join(c(asset_dir, rel_asset))

    if (fs::is_dir(asset)[[1]]) {
      fs::dir_copy(
        asset,
        new_path,
        overwrite = overwrite
      )
    } else if (fs::is_file(asset)[[1]]) {
      fs::path_dir(rel_asset) %>%
        fs::path(asset_dir, .) %>%
        fs::dir_create()

      fs::file_copy(
        asset,
        new_path,
        overwrite = overwrite
      )
    }
  }

  dirs[valid_dirs] %>% purrr::walk(create_and_copy)

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

local_asset_dir <- function(..., env = parent.frame()) {
  refs <- rlang::list2(...)
  opts <- purrr::map(refs, fs::path_temp)
  names(opts) <- purrr::map_chr(refs, ~ paste0("touchstone.dir_assets_", .x))
  withr::local_options(opts, .local_envir = env)

  invisible(opts)
}

get_asset_dir <- function(ref) {
  asset_dir <- getOption(paste0("touchstone.dir_assets_", ref))

  if (is.null(asset_dir)) {
    cli::cli_abort(c(
      "Temporary directory for ref {.arg {ref}} not found. ",
      "i" = paste0(
        "This function is only for use within the {.code ?touchstone_script},",
        " which must be called with {.fun run_script}",
        "or after running {.fun touchstone::activate}"
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


abort_string <- function() {
  rlang::abort(paste0(
    "Using a string as the named expression to benchmark or ",
    "`expr_before_benchmark` is deprecated."
  ))
}

append_rbuildignore <- function(dir) {
  ignore <- ".Rbuildignore"
  if (fs::file_exists(ignore)) {
    cat(
      glue::glue("^{dir}$"),
      sep = "\n", file = ignore, append = TRUE
    )
    cli::cli_alert_success("Added {.path {dir}} to {.file {ignore}}.")
  } else {
    cli::cli_alert_warning(
      "Could not find {.file {ignore}} to add {.path {dir}}."
    )
  }
}

find_git_root <- function(path = ".") {
  tryCatch(
    gert::git_find(path),
    error = function(err) {
      cli::cli_alert_danger(
        "Could not find git repository from current working directory!"
      )
      cli::cli_alert_info(
        "Please manually set the option {.val touchstone.git_root}."
      )
      NULL
    }
  )
}

get_git_root <- function() {
  git_root <- getOption("touchstone.git_root")

  if (is.null(git_root)) {
    cli::cli_abort(c("Option {.val touchstone.git_root} not set!",
      "i" = 'Set it with {.code options(touchstone.git_root = "path to repo")}'
    ))
  }

  git_root
}
