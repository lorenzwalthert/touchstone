test_that("can analyze results", {
  branch <- gert::git_branch()
  # generate results
  local_test_setup()
  withr::local_options(list(touchstone.skip_install = TRUE))
  path_test_pkg <- local_package()
  bm <- benchmark_run_iteration(
    "",
    xx1 = "Sys.sleep(runif(1, 0, 1e-5))",
    ref = branch,
    n = 5,
    libpaths = .libPaths()
  )
  benchmarks_analyze(branch)
  expect_match(
    readLines("touchstone/pr-comment/info.txt"),
    "xx1: .* -> .* \\(.*%\\)"
  )
  expect_true(fs::file_exists("touchstone/plots/xx1.png"))
})
