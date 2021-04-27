test_that("can analyze results", {
  branch <- gert::git_branch()
  # generate results
  local_test_setup()
  withr::local_options(list(touchstone.skip_install = TRUE))
  branches <- c("devel", "c4")
  path_test_pkg <- local_package(branches = branches)
  bm <- benchmark_run_iteration(
    "",
    xx1 = "Sys.sleep(runif(1, 0, 1e-5))",
    n = 5,
    ref = branches[2]
  )
  benchmarks_analyze(branches[2])
  expect_match(
    readLines("touchstone/pr-comment/info.txt"),
    "xx1: .* -> .* \\(.*%\\)"
  )
  expect_true(fs::file_exists("touchstone/plots/xx1.png"))
})