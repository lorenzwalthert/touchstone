#' Turn raw benchmark results into text and figures
#'
#' @details
#' Creates two side effects:
#'
#' * Density plots for each element in `refs` are written to `touchstone/plots`.
#' * A text explaining the speed diff is written to
#'   `touchstone/pr-comment/info.txt` for every registered benchmarking
#'   expression. See `vignette("inference", package = "touchstone")` for details.
#' @param refs The names of the branches for which analysis should be created.
#' @param ci The confidence level, defaults to 95%.
#' @export
benchmarks_analyze <- function(refs = c(
                                 ref_get_or_fail("GITHUB_BASE_REF"),
                                 ref_get_or_fail("GITHUB_HEAD_REF")
                               ),
                               ci = 0.95) {
  path_info <- fs::path(dir_touchstone(), "pr-comment/info.txt")
  paste0(
    "This is how benchmark results would change (along with a ", 100 * ci,
    "% confience interval in relative change) if ",
    system2("git", c("rev-parse", "HEAD"), stdout = TRUE),
    " is merged into ", refs[1], ":", "\n"
  ) %>%
    writeLines(path_info)

  out <- purrr::walk(benchmark_ls(), benchmark_analyze, refs = refs, ci = ci)
  text <- paste(
    "\nFurther explanation regarding interpretation and methodology can be found",
    "in the [documentation](https://lorenzwalthert.github.io/touchstone/articles/inference.html)."
  )
  cat(text, fill = TRUE, file = path_info, append = TRUE)

  readLines(path_info)
}

#' @importFrom rlang .data
benchmark_analyze <- function(benchmark, refs = c(
                                ref_get_or_fail("GITHUB_BASE_REF"),
                                ref_get_or_fail("GITHUB_HEAD_REF")
                              ),
                              ci = 0.95) {
  timings <- benchmark_read(benchmark, refs)
  benchmark_plot(benchmark, timings)
  benchmark_verbalize(benchmark, timings = timings, refs = refs, ci = ci)
}

#' Create nice text from benchmarks
#'
#' `refs` must be passed because the order is relevant.
#' @inheritParams benchmark_plot
#' @keywords internal
benchmark_verbalize <- function(benchmark, timings, refs, ci) {
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
  confint <- confint_relative_get(timings, refs, tbl$mean[1], ci = ci)

  text <- glue::glue(
    "* {benchmark}: {tbl$mean[1]}s -> {tbl$mean[2]}s {confint}"
  )
  cat(
    text,
    fill = TRUE, file = fs::path(dir_touchstone(), "pr-comment/info.txt"),
    append = TRUE
  )
  text
}

set_sign <- function(x) {
  purrr::map_chr(x, ~ paste0(ifelse(.x > 0, "+", ""), .x))
}

confint_relative_get <- function(timings, refs, reference, ci) {
  timings_with_factors <- timings %>%
    dplyr::mutate(
      block = factor(.data$block), ref = factor(.data$ref, levels = refs)
    )
  stopifnot(inherits(timings_with_factors$ref, "factor"))
  fit <- stats::aov(elapsed ~ ref, data = timings_with_factors)
  var <- paste0("ref", refs[2])
  confint <- confint(fit, var, level = ci)
  paste0("[", paste0(set_sign(round(100 * confint / reference, 2)), collapse = "%, "), "%]")
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
