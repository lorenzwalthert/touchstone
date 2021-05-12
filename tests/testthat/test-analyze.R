test_that("can analyze results", {
  withr::local_options(list(touchstone.skip_install = TRUE))
  branches <- c("devel", "c4")
  path_test_pkg <- local_package(branches = branches)
  bm <- benchmark_run_iteration(
    "",
    xx1 = "Sys.sleep(runif(1, 0, 1e-5))",
    n = 5,
    ref = branches[2],
    block = 1
  )
  mockery::stub(
    benchmark_verbalize, "confint_relative_get", "[x.xx%, y.yy%]",
    depth = 2
  )
  benchmarks_analyze(branches[2])
  expect_match(
    readLines("touchstone/pr-comment/info.txt"),
    as.character(glue::glue("xx1 \\(merge NA into {branches[2]}\\): .* -> .* \\[.*%, .*%\\]"))
  )
  expect_true(fs::file_exists("touchstone/plots/xx1.png"))
})
