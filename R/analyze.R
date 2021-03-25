#' Turn raw benchmark results into text and figures
#'
#' Creates two side effects:
#'
#' * Density plots for each element in `refs` are written to `touchstone/plots`.
#' * A text explaining the speed diff is written to
#'   `touchstone/pr-comment/info.txt` for every registered benchmarking
#'   expression.
#' @export
benchmarks_analyze <- function(refs = c(
                                 Sys.getenv("GITHUB_BASE_REF"),
                                 Sys.getenv("GITHUB_HEAD_REF")
                               )) {
  purrr::walk(benchmark_ls(), benchmark_analyze, refs = refs)
}

#' @importFrom dplyr .data
benchmark_analyze <- function(benchmark, refs) {
  # TODO test this
  timings <- benchmark_read(benchmark, refs)

  timings %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$elapsed, color = .data$ref)) +
    ggplot2::geom_density()
  fs::path("touchstone/plots/", benchmark) %>%
    fs::path_ext_set("png") %>%
    ggplot2::ggsave()

  tbl <- timings %>%
    dplyr::group_by(.data$ref) %>%
    dplyr::summarise(m = mean(.data$elapsed)) %>%
    tibble::deframe()

  diff_percent <- round(100 * (tbl[refs[2]] - tbl[refs[1]]) / tbl[refs[1]], 1)
  cat(
    glue::glue(
      "{benchmark}: {round(tbl[refs[1]], 2)} -> {round(tbl[refs[2]], 2)} ",
      "({diff_percent}%)"
    ),
    fill = TRUE,
    file = "touchstone/pr-comment/info.txt",
    append = TRUE
  )
}
