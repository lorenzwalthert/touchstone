#' Run a benchmark iteration
#' @param expr_before_benchmark Character vector with code to run before
#'   the benchmark is ran, will be evaluated with [expr_eval()].
#' @param expr_to_benchmark Character vector with code to benchmark, will be
#'   evaluated with [expr_eval()].
#' @param n The number of times to run a benchmark, each time with a separate
#'   call to [bench::mark()].
#' @inheritParams benchmark_write
#' @importFrom tibble lst tibble
#' @export
benchmark_run_iteration <- function(expr_before_benchmark,
                                    expr_to_benchmark,
                                    ref,
                                    n = 20) {
  if (rlang::is_missing(expr_before_benchmark)) {
    expr_before_benchmark <- ""
  }
  args <- lst(
    expr_before_benchmark,
    expr_to_benchmark,
    ref,
    # .ref = tibble::lst(expr_eval, benchmark_write)
    # touchstone namespace not available in callr. For quick testing, it's
    # easier to pass required elements via .ref to the env instead of
    # relying on the built package and use ::(:).
  )

  for (iteration in seq_len(n)) { # iterations
    callr::r(
      function(expr_before_benchmark, expr_to_benchmark, ref, iteration) {
        new_name <- "masked_touchstone"
        attach(loadNamespace("touchstone"), name = new_name)
        on.exit(detach(new_name, character.only = TRUE), add = TRUE)
        expr_eval(expr_before_benchmark)
        # library(magrittr) # infix can't be passed via .ref
        benchmark <- bench::mark(expr_eval(expr_to_benchmark), memory = FALSE, iterations = 1)
        benchmark_write(benchmark, ref, iteration)
      },
      args = append(args, lst(iteration))
    )
  }
  usethis::ui_done("Ran {n} iterations of ref `{ref}`.")
  benchmark_read(ref)
}

#' Run a benchmark for git refs
#'
#' @param refs Git refs passed as `ref` to [benchmark_iteration_prepare()].
#' @param n Number of time to run benchmark.
#' @inheritParams benchmark_iteration_prepare
#' @inheritParams benchmark_run_iteration
#' @details
#' Runs the following loop `n` times:
#'  * Installs random branch from `refs` of the package `path_profiling_pkg`.
#'  * runs setup code `exp_before_ref`.
#'  * benchmarks `expr_to_benchmark`.
#' Returns all timings.
#' @export
benchmark_run_ref <- function(refs,
                              expr_before_benchmark,
                              expr_to_benchmark,
                              n = 20,
                              path_pkg = ".",
                              install_quick = TRUE,
                              install_dependencies = FALSE) {
  refs <- ref_upsample(refs, n = n)
  out_list <- purrr::map(refs, benchmark_run_ref_impl,
    expr_before_benchmark = expr_before_benchmark,
    expr_to_benchmark = expr_to_benchmark,
    path_pkg = path_pkg,
    install_quick = install_quick,
    install_dependencies = install_dependencies
  )
  vctrs::vec_rbind(!!!out_list)
}

benchmark_run_ref_impl <- function(ref,
                                   expr_before_benchmark,
                                   expr_to_benchmark,
                                   path_pkg,
                                   install_quick,
                                   install_dependencies) {
  benchmark_iteration_prepare(
    ref,
    path_pkg,
    install_quick = install_quick,
    install_dependencies = install_dependencies
  )
  benchmark_run_iteration(
    expr_before_benchmark = expr_before_benchmark,
    expr_to_benchmark = expr_to_benchmark,
    ref = ref
  )
}
