#' Run a benchmark iteration
#' @param expr_before_benchmark Expression to run before
#'   the benchmark is ran, will be captured with [rlang::enexpr()].
#'   So you can use quasiquotation.
#' @param n Number of iterations to run a benchmark within an iteration.
#' @param dots list of quoted expressions (length 1).
#' @inheritParams benchmark_write
#' @importFrom tibble lst tibble
#' @keywords internal
benchmark_run_iteration <- function(expr_before_benchmark,
                                    dots,
                                    branch,
                                    block,
                                    n = getOption("touchstone.n_iterations", 1)) {
  if (rlang::is_missing(expr_before_benchmark)) {
    expr_before_benchmark <- rlang::expr({})
  }

  args <- rlang::list2(
    expr_before_benchmark = expr_before_benchmark,
    dots = dots,
    branch = branch,
    block = block,
    asset_dirs = options() %>%
      names() %>%
      grep("touchstone.dir_assets_", .) %>%
      options()[.]
  )

  for (iteration in seq_len(n)) { # iterations
    callr::r(
      function(expr_before_benchmark, dots, branch, block, iteration, asset_dirs) {
        withr::local_namespace("touchstone")
        withr::local_options(asset_dirs)
        eval(expr_before_benchmark)
        benchmark <- bench::mark(eval(dots[[1]]), memory = FALSE, iterations = 1)
        benchmark_write(benchmark, names(dots), branch = branch, block = block, iteration = iteration)
      },
      args = append(args, lst(iteration)),
      libpath = c(libpath_touchstone(branch), .libPaths())
    )
  }
  cli::cli_alert_success("Ran {n} iteration{?s} of branch {.val {branch}}.")
  benchmark_read(names(dots), branch)
}

#' Run a benchmark for git branches
#'
#' @param ... Named expression of length one with code to benchmark,
#'   will be captured with [rlang::enexprs()]. So you can use quasiquotation.
#' @param branches Character vector with branch names to benchmark. The package
#'   must be built for each benchmarked branch beforehand with [branch_install()].
#'   The base branch is the target branch of the pull request in a workflow run,
#'   the head branch is the source branch of the pull request in a workflow run.
#' @param n Number of times benchmarks should be run for each `branch`.
#' @param path_pkg The path to the package to benchmark. Will be used to
#'   temporarily checkout the branch during benchmarking.
#' @inheritParams branch_install
#' @inheritParams benchmark_run_impl
#' @details
#' Runs the following loop `n` times:
#'  * removes all touchstone libraries from the library path, adding the one
#'    corresponding to `branch`.
#'  * runs setup code `exp_before_branch`.
#'  * benchmarks `expr_to_benchmark` and writes them to disk.
#'
#' @return
#' All timings in a tibble.
#' @section Caution:
#' This function will perform various git operations that affect the state of
#' the directory it is ran in, in particular different branches will be checked
#' out. Ensure a clean git working directory before invocation.
#' @export
benchmark_run <- function(expr_before_benchmark =
                            {},
                          ...,
                          branches = c(
                            branch_get_or_fail("GITHUB_BASE_REF"),
                            branch_get_or_fail("GITHUB_HEAD_REF")
                          ),
                          n = 100,
                          path_pkg = ".")
{
  force(branches)
  expr_before_benchmark <- rlang::enexpr(expr_before_benchmark)
  dots <- rlang::enexprs(...)

  if (length(dots) > 1) {
    cli::cli_abort("Expression to benchmark cannot have length greater than one.")
  }
  if (rlang::is_string(dots[[1]]) ||
    rlang::is_string(expr_before_benchmark)) {
    abort_string()
  }

  # touchstone libraries must be removed from the path temporarily
  # and the one to benchmark will be added in benchmark_run_impl()
  local_without_touchstone_lib()
  branches <- branches_upsample(branches, n = n)
  out_list <- purrr::pmap(branches, benchmark_run_impl,
    expr_before_benchmark = expr_before_benchmark,
    dots = dots,
    path_pkg = path_pkg
  )
  vctrs::vec_rbind(!!!out_list)
}

#' Checkout a branch from a repo and run an iteration
#'
#' @param path_pkg The path to the root of the package you want to benchmark.
#' @inheritParams benchmark_run_iteration
#' @keywords internal
benchmark_run_impl <- function(branch,
                               block,
                               expr_before_benchmark,
                               dots,
                               path_pkg) {
  local_git_checkout(branch, path_pkg)
  benchmark_run_iteration(
    expr_before_benchmark = expr_before_benchmark,
    dots = dots,
    branch = branch,
    block = block
  )
}
