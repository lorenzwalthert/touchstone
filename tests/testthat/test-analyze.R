test_that("can analyze results", {
  skip_if(packageVersion("mockery") < "0.4.2.9000")
  withr::local_options(list(touchstone.skip_install = TRUE))
  branches <- c("devel", "c4")
  path_test_pkg <- local_package(branches = branches)
  purrr::walk(branches, ~ benchmark_run_iteration(
    "",
    dots = list(xx1 = "Sys.sleep(runif(1, 0, 1e-5))"),
    n = 2,
    ref = .x,
    block = 1
  ))
  mockery::stub(
    benchmark_verbalize,
    "confint_relative_get",
    list(string = "[x.xx%, y.yy%]", emoji = ":rocket:"),
    depth = 2
  )
  benchmarks_analyze(branches)
  expect_match(
    readLines("touchstone/pr-comment/info.txt")[3],
    as.character(glue::glue("xx1: .*s -> .*s \\[.*%, .*%\\]"))
  )
  expect_true(fs::file_exists("touchstone/plots/xx1.png"))
})


test_that("can validate inputs before analysing", {
  expect_error(benchmarks_analyze(refs = "just-one"), "exactly two refs")
})

test_that("can analyze results", {
  skip_if(packageVersion("mockery") < "0.4.2.9000")
  withr::local_options(list(touchstone.skip_install = TRUE))
  branches <- c("devel", "c4")
  path_test_pkg <- local_package(branches = branches)
  purrr::walk(branches, ~ benchmark_run_iteration(
    "",
    dots = list(xx1 = "Sys.sleep(runif(1, 0, 1e-5))"),
    n = 2,
    ref = .x,
    block = 1
  ))

  purrr::walk(branches, ~ benchmark_run_iteration(
    "",
    dots = list(xx2 = "Sys.sleep(runif(1, 0, 1e-5))"),
    n = 2,
    ref = .x,
    block = 1
  ))

  mockery::stub(
    benchmark_verbalize,
    "confint_relative_get",
    list(string = "[x.xx%, y.yy%]", emoji = ":rocket:"),
    depth = 2
  )
  fs::file_delete(fs::path(dir_touchstone(), "records", "xx1", branches[1]))
  expect_warning(
    out <- benchmarks_analyze(branches),
    "All benchmarks to analyse must have the two refs"
  )
  expect_match(
    out[3],
    as.character(glue::glue("xx2: .*s -> .*s \\[.*%, .*%\\]"))
  )
  expect_true(fs::file_exists("touchstone/plots/xx2.png"))
})
