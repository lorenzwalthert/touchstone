#' Run a benchmark iteration
#' @param expr_before_benchmark Character vector with code to run before
#'   the benchmark is ran, will be evaluated with [exprs_eval()].
#' @param ... Named character vector of length one with code to benchmark, will
#'   be evaluated with [exprs_eval()].
#' @param n The number of times to run a benchmark, each time with a separate
#'   call to [bench::mark()].
#' @inheritParams benchmark_write
#' @importFrom tibble lst tibble
#' @export
benchmark_run_iteration <- function(expr_before_benchmark,
                                    ...,
                                    ref,
                                    n = 20) {
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
    # touchstone namespace not available in callr. For quick testing, it's
    # easier to pass required elements via .ref to the env instead of
    # relying on the built package and use ::(:).
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
      args = append(args, lst(iteration))
    )
  }
  usethis::ui_done("Ran {n} iterations of ref `{ref}`.")
  benchmark_read(names(rlang::list2(...)), ref)
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
benchmark_run_ref <- function(expr_before_benchmark,
                              ...,
                              refs = c(
                                Sys.getenv("GITHUB_BASE_REF"),
                                Sys.getenv("GITHUB_HEAD_REF")
                              ),
                              n = 20,
                              path_pkg = ".",
                              install_quick = TRUE,
                              install_dependencies = FALSE) {
  refs <- ref_upsample(refs, n = n)
  out_list <- purrr::map(refs, benchmark_run_ref_impl,
    expr_before_benchmark = expr_before_benchmark,
    ...,
    path_pkg = path_pkg,
    install_quick = install_quick,
    install_dependencies = install_dependencies
  )
  vctrs::vec_rbind(!!!out_list)
}

benchmark_run_ref_impl <- function(ref,
                                   expr_before_benchmark,
                                   ...,
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
    ...,
    ref = ref
  )
}
