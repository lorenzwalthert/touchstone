test_that("can analyze results", {
  skip_if(packageVersion("mockery") < "0.4.2.9000")
  withr::local_options(list(touchstone.skip_install = TRUE))
  branches <- c("devel", "c4")
  path_test_pkg <- local_package(branches = branches)
  purrr::walk(branches, ~ benchmark_run_iteration(
    "",
    dots = list(xx1 = "Sys.sleep(runif(1, 0, 1e-5))"),
    n = 2,
    branch = .x,
    block = 1
  ))
  mockery::stub(
    benchmark_verbalize,
    "confint_relative_get",
    list(string = "[x.xx%, y.yy%]", relative_change = 0, emoji = ":rocket:"),
    depth = 2
  )
  benchmark_analyze(branches)
  lines <- readLines("touchstone/pr-comment/info.txt")
  expect_true("| Status | Name | Current | Incoming | Interval |" %in% lines)
  expect_match(
    lines,
    "^\\| :rocket: \\| `xx1` \\| .*s \\| .*s \\| \\[x.xx%, y.yy%\\] \\|$",
    all = FALSE
  )
  # the table is surrounded by empty lines, so header and footer are not part
  # of it
  rows <- grep("^\\|", lines)
  expect_equal(lines[min(rows) - 1], "")
  expect_equal(lines[max(rows) + 1], "")
  expect_true(fs::file_exists("touchstone/plots/xx1.png"))
})


test_that("can validate inputs before analysing", {
  expect_error(benchmark_analyze(branches = "just-one"), "exactly two branches")
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
    branch = .x,
    block = 1
  ))

  purrr::walk(branches, ~ benchmark_run_iteration(
    "",
    dots = list(xx2 = "Sys.sleep(runif(1, 0, 1e-5))"),
    n = 2,
    branch = .x,
    block = 1
  ))

  mockery::stub(
    benchmark_verbalize,
    "confint_relative_get",
    list(string = "[x.xx%, y.yy%]", relative_change = 0, emoji = ":rocket:"),
    depth = 2
  )
  fs::file_delete(fs::path(dir_touchstone(), "records", "xx1", branches[1]))
  expect_warning(
    out <- benchmark_analyze(branches),
    "All benchmarks to analyse must have the two branches"
  )
  expect_match(
    out,
    "^\\| :rocket: \\| `xx2` \\| .*s \\| .*s \\| \\[x.xx%, y.yy%\\] \\|$",
    all = FALSE
  )
  expect_false(any(grepl("`xx1`", out)))
  expect_true(fs::file_exists("touchstone/plots/xx2.png"))
})

test_that("can sort the benchmark table", {
  tbl <- tibble::tibble(
    name = c("b", "a", "c"),
    emoji = c(":rocket:", ":exclamation::snail:", ":heavy_check_mark:"),
    current = c(0.242, 0.1, 0.5),
    incoming = c(0.1, 0.3, 0.5),
    relative_change = c(-55, 200, 0),
    interval = c("[-60%, -50%]", "[+190%, +210%]", "[-1%, +1%]")
  )
  expect_equal(benchmark_table_sort(tbl, "name")$name, c("a", "b", "c"))
  expect_equal(
    benchmark_table_sort(tbl, "performance_improvement")$name,
    c("b", "c", "a")
  )
  expect_equal(
    benchmark_table_sort(tbl, "performance_regression")$name,
    c("a", "c", "b")
  )
  expect_equal(
    benchmark_table_sort(tbl, "current_running_time")$name,
    c("c", "b", "a")
  )
  expect_equal(
    benchmark_table_sort(tbl, "incoming_running_time")$name,
    c("c", "a", "b")
  )
})

test_that("can format the benchmark table", {
  tbl <- tibble::tibble(
    name = c("b", "a", "c"),
    emoji = c(":rocket:", ":exclamation::snail:", ":heavy_check_mark:"),
    current = c(0.242, 0.1, 0.5),
    incoming = c(0.1, 0.3, 0.5),
    relative_change = c(-55, 200, 0),
    interval = c("[-60%, -50%]", "[+190%, +210%]", "[-1%, +1%]")
  )
  expect_snapshot(
    writeLines(benchmark_table_format(tbl))
  )
})

test_that("invalid sorting throws error", {
  expect_error(
    benchmark_analyze(branches = c("a", "b"), sorting = "fastest"),
    "must be one of"
  )
})

test_that("missing package throws error", {
  skip_if(packageVersion("mockery") < "0.4.2.9000")
  mockery::stub(benchmark_analyze, "requireNamespace", FALSE)
  expect_error(benchmark_analyze(), "additional package")
})
