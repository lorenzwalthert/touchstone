#' Turn raw benchmark results into text and figures
#'
#' @details
#' Creates two side effects:
#'
#' * Density plots for each element in `refs` are written to `touchstone/plots`.
#' * A text explaining the speed diff is written to
#'   `touchstone/pr-comment/info.txt` for every registered benchmarking
#'   expression.
#' @param refs The names of the branches for which analysis should be created.
#' @export
benchmarks_analyze <- function(refs = c(
                                 Sys.getenv("GITHUB_BASE_REF"),
                                 Sys.getenv("GITHUB_HEAD_REF")
                               )) {
  purrr::walk(benchmark_ls(), benchmark_analyze, refs = refs)
}

#' @importFrom rlang .data
benchmark_analyze <- function(benchmark, refs) {
  timings <- benchmark_read(benchmark, refs)
  benchmark_plot(benchmark, timings)
  benchmark_verbalize(benchmark, timings, refs)
}

#' Create nice text from benchmarks
#'
#' `refs` must be passed because the order is relevant.
#' @inheritParams benchmark_plot
#' @keywords internal
benchmark_verbalize <- function(benchmark, timings, refs) {
  tbl <- timings %>%
    dplyr::group_by(.data$ref) %>%
    dplyr::summarise(m = mean(.data$elapsed)) %>%
    tibble::deframe()
  if (length(tbl) > 2) {
    rlang::abort("Benchmarks with more than two `refs` cannot be verbalized.")
  }
  diff_percent <- round(100 * (tbl[refs[2]] - tbl[refs[1]]) / tbl[refs[1]], 1)
  cat(
    glue::glue(
      "{benchmark}: {round(tbl[refs[1]], 2)} -> {round(tbl[refs[2]], 2)} ",
      "({diff_percent}%)"
    ),
    fill = TRUE,
    file = fs::path(dir_touchstone(), "pr-comment/info.txt"),
    append = TRUE
  )
}

#' @param timing a benchmark read with [benchmark_read()], column `name` must
#'   only contain one unique value.
#' @keywords internal
benchmark_plot <- function(benchmark, timings) {
  timings %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$elapsed, color = .data$ref)) +
    ggplot2::geom_density()
  fs::path(dir_touchstone(), "plots", benchmark) %>%
    fs::path_ext_set("png") %>%
    ggplot2::ggsave()
}
