test_that("can evaluate expressions for benchmarking", {
  expect_equal(
    exprs_eval("1 + 2.3"),
    1 + 2.3
  )
})

test_that("ref can be sampled", {
  withr::with_seed(
    3,
    expect_snapshot_value(ref_upsample(c("main", "issue-3")), style = "serialize")
  )
  withr::with_seed(
    3,
    expect_snapshot_value(ref_upsample(letters, n = 2), style = "serialize")
  )
})

test_that("touchstone dir can be removed", {
  local_clean_touchstone()
  fs::dir_create(dir_touchstone())
  touchstone_clear(all = TRUE)
  expect_false(fs::dir_exists(dir_touchstone()))
})

test_that("can checkout locally", {
  local_package()
  old_branch <- gert::git_branch()
  new_branch <- "ewjlkj"
  gert::git_branch_create(new_branch, checkout = FALSE)
  gert::git_branch_list()
  test_f <- function(new_branch) {
    local_git_checkout(new_branch)
    expect_equal(gert::git_branch(), new_branch)
  }
  test_f(new_branch = new_branch)
  expect_equal(gert::git_branch(), old_branch)
})

test_that("can remove touchstone libpaths", {
  refs <- c("devel", "main")
  if (is_windows()) {
    # cannot have touchstone library in temp, as lib path comparison becomes
    # unfeasible due to short/name notation
    withr::local_options(dir_touchstone = fs::path_file(tempfile()))
  }
  path_pkg <- local_package(setwd = !is_windows())
  new_libpaths <- refs_install(refs, path_pkg, install_dependencies = FALSE)

  withr::local_libpaths(new_libpaths)

  expect_equal(.libPaths(), new_libpaths)
  local_without_touchstone_lib(path_pkg)
  after_removal <- setdiff(
    new_libpaths,
    as.character(fs::path_abs(libpath_touchstone(refs)))
  )

  expect_equal(after_removal, .libPaths())
})


test_that("Can abort with missing refs for benchmark run", {
  withr::local_envvar(GITHUB_BASE_REF = NA, GITHUB_HEAD_REF = NA)
  mockery::stub(
    benchmark_run_ref, "force",
    function(...) rlang::abort("12321")
  )
  withr::local_envvar(list(GITHUB_HEAD_REF = "feature1", GITHUB_BASE_REF = "mastero"))
  expect_error(
    benchmark_run_ref(x1 = "print('hi')"),
    "12321"
  )
})


test_that("Can abort with missing refs for benchmark run", {
  withr::local_envvar(GITHUB_BASE_REF = NA, GITHUB_HEAD_REF = NA)
  match <- "you want to benchmark against each other"
  expect_error(ref_get_or_fail("SOME_REF"), match)
  expect_error(
    benchmark_run_ref(x1 = "print('hi')"),
    match
  )
  expect_error(
    benchmark_analyze("sume2"),
    match
  )

  expect_error(
    refs_install(),
    match
  )
})
