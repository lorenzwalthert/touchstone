#' Run a benchmark iteration
#' @param expr_before_benchmark Character vector with code to run before
#'   the benchmark is ran, will be evaluated with [exprs_eval()].
#' @param n Number of iterations to run a benchmark within an iteration.
#' @param dots list of quoted expressions (length 1).
#' @inheritParams benchmark_write
#' @importFrom tibble lst tibble
#' @keywords internal
benchmark_run_iteration <- function(expr_before_benchmark,
                                    dots,
                                    ref,
                                    block,
                                    n = getOption("touchstone.n_iterations", 1)) {
  if (rlang::is_missing(expr_before_benchmark)) {
    expr_before_benchmark <- ""
  }

  args <- rlang::list2(
    expr_before_benchmark = expr_before_benchmark,
    dots = dots,
    ref = ref,
    block = block
  )
  for (iteration in seq_len(n)) { # iterations
    callr::r(
      function(expr_before_benchmark, dots, ref, block, iteration) {
        new_name <- "masked_touchstone"
        attach(loadNamespace("touchstone"), name = new_name)
        on.exit(detach(new_name, character.only = TRUE), add = TRUE)
        exprs_eval(!!expr_before_benchmark)
        benchmark <- bench::mark(exprs_eval(!!dots[[1]]), memory = FALSE, iterations = 1)
        benchmark_write(benchmark, names(dots), ref = ref, block = block, iteration = iteration)
      },
      args = append(args, lst(iteration)),
      libpath = c(libpath_touchstone(ref), .libPaths())
    )
  }
  cli::cli_alert_success("Ran {n} iteration{?s} of ref {.val {ref}}.")
  benchmark_read(names(dots), ref)
}

#' Run a benchmark for git refs
#'
#' @param ... Named expression or named character vector of length one with code to benchmark, will
#'   be evaluated with [exprs_eval()].
#' @param refs Character vector with branch names to benchmark. The package
#'   must be built for each benchmarked branch beforehand with [refs_install()].
#'   The base ref is the target branch of the pull request in a workflow run,
#'   the head ref is the source branch of the pull request in a workflow run.
#' @param n Number of times benchmarks should be run for each `ref`.
#' @param path_pkg The path to the package to benchmark. Will be used to
#'   temporarily checkout the branch during benchmarking.
#' @inheritParams refs_install
#' @inheritParams benchmark_run_ref_impl
#' @details
#' Runs the following loop `n` times:
#'  * removes all touchstone libraries from the library path, adding the one
#'    corresponding to `ref`.
#'  * runs setup code `exp_before_ref`.
#'  * benchmarks `expr_to_benchmark` and writes them to disk.
#'
#' @return
#' All timings in a tibble.
#' @section Caution:
#' This function will perform various git operations that affect the state of
#' the directory it is ran in, in particular different branches will be checked
#' out. Ensure a clean git working directory before invocation.
#' @export
benchmark_run_ref <- function(expr_before_benchmark,
                              ...,
                              refs = c(
                                ref_get_or_fail("GITHUB_BASE_REF"),
                                ref_get_or_fail("GITHUB_HEAD_REF")
                              ),
                              n = 100,
                              path_pkg = ".") {
  force(refs)
  expr_before_benchmark <- rlang::enexpr(expr_before_benchmark)
  dots <- rlang::enexprs(...)

  if (length(dots) > 1) {
    cli::cli_abort("Can only pass one expression to benchmark")
  }
  # touchstone libraries must be removed from the path temporarily
  # and the one to benchmark will be added in benchmark_run_ref_impl()
  local_without_touchstone_lib()
  # libpaths <- refs_install(refs, path_pkg, install_dependencies) # potentially not needed anymroe
  refs <- ref_upsample(refs, n = n)
  out_list <- purrr::pmap(refs, benchmark_run_ref_impl,
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
benchmark_run_ref_impl <- function(ref,
                                   block,
                                   expr_before_benchmark,
                                   dots,
                                   path_pkg) {
  local_git_checkout(ref, path_pkg)
  benchmark_run_iteration(
    expr_before_benchmark = expr_before_benchmark,
    dots = dots,
    ref = ref,
    block = block
  )
}
