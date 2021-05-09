#' Run a benchmark iteration
#' @param expr_before_benchmark Character vector with code to run before
#'   the benchmark is ran, will be evaluated with [exprs_eval()].
#' @param ... Named character vector of length one with code to benchmark, will
#'   be evaluated with [exprs_eval()].
#' @inheritParams benchmark_write
#' @importFrom tibble lst tibble
#' @export
benchmark_run_iteration <- function(expr_before_benchmark,
                                    ...,
                                    ref,
                                    n = getOption("touchstone.n_iterations", 1)) {
  if (rlang::is_missing(expr_before_benchmark)) {
    expr_before_benchmark <- ""
  }
  if (length(rlang::list2(...)) > 1) {
    rlang::abort("Can only pass one expression to benchmark")
  }
  args <- rlang::list2(
    expr_before_benchmark = expr_before_benchmark,
    ...,
    ref = ref
  )
  for (iteration in seq_len(n)) { # iterations
    callr::r(
      function(expr_before_benchmark, ..., ref, iteration) {
        new_name <- "masked_touchstone"
        attach(loadNamespace("touchstone"), name = new_name)
        on.exit(detach(new_name, character.only = TRUE), add = TRUE)
        exprs_eval(expr_before_benchmark)
        benchmark <- bench::mark(exprs_eval(...), memory = FALSE, iterations = 1)
        benchmark_write(benchmark, names(rlang::list2(...)), ref = ref, iteration = iteration)
      },
      args = append(args, lst(iteration)),
      libpath = c(libpath_touchstone(ref), .libPaths())
    )
  }
  usethis::ui_done("Ran {n} iterations of ref `{ref}`.")
  benchmark_read(names(rlang::list2(...)), ref)
}

#' Run a benchmark for git refs
#'
#' @param refs Character vector with branch names to benchmark. The package
#'   must be built for each benchmarked branch beforehand with [refs_install()].
#'   The base ref is the target branch of the pull request in a workflow run,
#'   the head ref is the source branch of the pull request in a workflow run.
#' @param n Number of times benchmarks should be run. Refers to the total of
#'   all `refs`.
#' @inheritParams refs_install
#' @inheritParams benchmark_run_ref_impl
#' @details
#' Runs the following loop `n` times:
#'  * removes all touchstone libraries from the library path, adding the one
#'    corresponding to `ref`.
#'  * runs setup code `exp_before_ref`.
#'  * benchmarks `expr_to_benchmark` and writes them to disk.
#'
#' Returns all timings.
#' @export
benchmark_run_ref <- function(expr_before_benchmark,
                              ...,
                              refs = c(
                                Sys.getenv("GITHUB_BASE_REF"),
                                Sys.getenv("GITHUB_HEAD_REF")
                              ),
                              n = 100,
                              path_pkg = ".") {
  # touchstone libraries must be removed from the path temporarily
  # and the one to benchmark will be added in benchmark_run_ref_impl()
  local_without_touchstone_lib()
  # libpaths <- refs_install(refs, path_pkg, install_dependencies) # potentially not needed anymroe
  refs <- ref_upsample(refs, n = n)
  out_list <- purrr::map(refs, benchmark_run_ref_impl,
    expr_before_benchmark = expr_before_benchmark,
    ...,
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
                                   expr_before_benchmark,
                                   ...,
                                   path_pkg) {
  local_git_checkout(ref, path_pkg)
  benchmark_run_iteration(
    expr_before_benchmark = expr_before_benchmark,
    ...,
    ref = ref
  )
}
