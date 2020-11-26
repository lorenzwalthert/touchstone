#' Write a benchmark
#'
#' @param benchmark The result of [bench::mark()], with `iterations = 1`.
#' @param name The name of the benchmark.
#' @param append Whether to append the result to the file or not.
#' @param ref A character vector of length one to indicate the git ref (i.e.
#'   commit, tag, branch etc) of the benchmarking.
#' @param iteration An integer indicating to which iteration the benchmark
#'   refers to.
#' @export
benchmark_write <- function(benchmark, name, ref, iteration = NA, append = TRUE) {
  if (benchmark$n_itr > 1) {
    rlang::abort("This package only supports benchmarks with `bench::mark(..., iterations = 1`.")
  }
  path <- path_record(name, ref)
  ensure_dir(fs::path_dir(path))
  tibble(
    elapsed = as.numeric(benchmark$median),
    iteration = iteration,
    ref = enc2utf8(ref),
    name = name
  ) %>%
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
benchmark_read <- function(name, ref) {
  path_outputs <- path_record(name, ref)
  out <- purrr::map(
    path_outputs,
    benchmark_read_impl
  )
  vctrs::vec_rbind(!!!out)
}

#' List which benchmarks were recorded
#'
#' @inheritParams benchmark_write
#' @export
benchmark_ls <- function(name = "") {
  path <- path_record(name = name)
  if (fs::file_exists(path)) {
    as.character(fs::path_file(fs::dir_ls(path)))
  } else {
    character()
  }
}

path_record <- function(name = "", ref = "") {
  as.character(fs::path(dir_touchstone(), "records", name, ref))
}

benchmark_read_impl <- function(path) {
  utils::read.table(path, header = TRUE, colClasses = schema_disk()) %>%
    tibble::as_tibble()
}
