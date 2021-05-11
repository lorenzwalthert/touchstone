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
    dplyr::summarise(
      mean = mean(.data$elapsed),
      sd = stats::sd(.data$elapsed)
    ) %>%
    dplyr::mutate(dplyr::across(tidyselect::vars_select_helpers$where(is.numeric), round, 2)) %>%
    dplyr::inner_join(tibble::tibble(ref = refs), ., by = "ref")


  if (nrow(tbl) > 2) {
    rlang::abort("Benchmarks with more than two `refs` cannot be verbalized.")
  }
  diff_percent <- round(100 * (tbl$mean[2] - tbl$mean[1]) / tbl$mean[1], 1)
  sign <- ifelse(diff_percent > 0, "+", "")
  text <- glue::glue(
    "{benchmark} ({merge tbl$ref[2]} into {tbl$ref[1]}): ",
    "{tbl$mean[1]} (\U00B1 {tbl$sd[1]}) -> {tbl$mean[2]} ",
    "(\U00B1 {tbl$sd[2]}): ({sign}{diff_percent}%)"
  )
  cat(
    text,
    fill = TRUE, file = fs::path(dir_touchstone(), "pr-comment/info.txt"),
    append = TRUE
  )
  text
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
