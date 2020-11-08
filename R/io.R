#' Write a benchmark
#'
#' @param benchmark The result of [bench::mark()], with `iterations = 1`.
#' @param append Whether to append the result to the file or not.
#' @param ref A character vector of length one to indicate the git ref (i.e.
#'   commit, tag, branch etc) of the benchmarking.
#' @param iteration An integer indicating to which iteration the benchmark
#'   refers to.
#' @export
benchmark_write <- function(benchmark, ref, iteration = NA, append = TRUE) {
  if (benchmark$n_itr > 1) {
    rlang::abort("This package only supports benchmarks with `bench::mark(..., iterations = 1`.")
  }
  ensure_touchstone_dir()
  path <- as.character(fs::path(dir_touchstone(), ref))
  tibble(elapsed = as.numeric(benchmark$median), iteration = iteration, ref = enc2utf8(ref)) %>%
    benchmark_write_impl(path = path, append = append)
}

benchmark_write_impl <- function(benchmark, path, append) {
  file_exists <- fs::file_exists(path)
  suppressWarnings(
    utils::write.table(
      benchmark, path,
      append = append, row.names = FALSE,
      fileEncoding = "UTF-8", col.names = !file_exists
    )
  )
  path
}

#' Read benchmarks
#' @inheritParams benchmark_write
#' @export
benchmark_read <- function(ref) {
  path_outputs <- fs::path(dir_touchstone(), ref)
  purrr::map_dfr(
    path_outputs,
    benchmark_read_impl
  )
}

benchmark_read_impl <- function(path) {
  utils::read.table(path, header = TRUE, colClasses = schema_disk()) %>%
    tibble::as_tibble()
}
