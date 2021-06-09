#' Write a benchmark
#'
#' @param benchmark The result of [bench::mark()], with `iterations = 1`.
#' @param name The name of the benchmark.
#' @param append Whether to append the result to the file or not.
#' @param ref A character vector of length one to indicate the git ref (i.e.
#'   commit, tag, branch etc) of the benchmarking.
#' @param block All refs appear once in a block.
#' @param iteration An integer indicating to which iteration the benchmark
#'   refers to. Multiple iterations within a block always benchmark the same
#'   `ref`.
#' @return
#' Character vector of lenth one with path to the record written (invisibly).
#' @export
benchmark_write <- function(benchmark, name, ref, block = NA, iteration = NA, append = TRUE) {
  if (benchmark$n_itr > 1) {
    rlang::abort("This package only supports benchmarks with `bench::mark(..., iterations = 1`.")
  }
  path <- path_record(name, ref)
  init_touchstone()
  ensure_dir(fs::path_dir(path))
  tibble(
    elapsed = as.numeric(benchmark$median),
    iteration = iteration,
    ref = enc2utf8(ref),
    block = block,
    name = name
  ) %>%
    benchmark_write_impl(path = path, append = append)
}

init_touchstone <- function() {
  ensure_dir(dir_touchstone(), "plots")
  ensure_dir(dir_touchstone(), "pr-comment")
  ensure_dir(dir_touchstone(), "records")
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
  invisible(path)
}

#' Read benchmarks
#' @inheritParams benchmark_write
#' @return
#' A tibble with the benchmarks.
#' @export
benchmark_read <- function(name, ref) {
  path_outputs <- path_record(name, ref)
  out <- purrr::map(
    path_outputs,
    benchmark_read_impl
  )
  vctrs::vec_rbind(!!!out)
}


new_benchmark_ls_tibble <- function(name = character(), ref = character()) {
  tibble::tibble(name, ref)
}

#' List which benchmarks were recorded
#'
#' @inheritParams benchmark_write
#' @return
#' A tibble with name and refs of the existing benchmarks.
#' @export
benchmark_ls <- function(name = "") {
  path_record <- path_record()
  if (!fs::dir_exists(path_record())) {
    return(new_benchmark_ls_tibble())
  }
  path_names <- fs::dir_ls(path_record, type = "directory")
  if (length(path_names) < 1) {
    return(new_benchmark_ls_tibble())
  }
  all_names <- fs::path_file(path_names)
  path <- path_record(name = all_names)
  dirs <- fs::dir_ls(path, type = "file")
  new_benchmark_ls_tibble(
    name = fs::path_file(fs::path_dir(dirs)),
    ref = fs::path_file(dirs)
  )
}

path_record <- function(name = "", ref = "") {
  as.character(fs::path(dir_touchstone(), "records", name, ref))
}

benchmark_read_impl <- function(path) {
  utils::read.table(path, header = TRUE, colClasses = schema_disk()) %>%
    tibble::as_tibble()
}
