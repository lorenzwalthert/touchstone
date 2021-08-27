test_that("can evaluate expressions for benchmarking", {
  env <- rlang::env()
  expect_equal(
    {
      exprs_eval("x <- 1 + 2.3", env = env)
      env$x
    },
    1 + 2.3
  )

  expect_equal(
    {
      exprs_eval("
      x <- 1 + 2.3
      x <- x + 10
      x
      ", env = env)
      env$x
    },
    1 + 2.3 + 10
  )

  expect_equal(
    {
      exprs_eval(y <- 1 + 2.3, env = env)
      env$y
    },
    1 + 2.3
  )

  expr <- quote(z <- 1 + 2.3)
  expect_equal(
    {
      exprs_eval(!!expr, env = env)
      env$z
    },
    1 + 2.3
  )

  expr <- rlang::expr(zz <- 1 + 2.3)
  expect_equal(
    {
      exprs_eval(!!expr, env = env)
      env$zz
    },
    1 + 2.3
  )

  expr <- rlang::quo(zzz <- 1 + 2.3)
  expect_equal(
    {
      exprs_eval(!!rlang::get_expr(expr), env = env)
      env$zzz
    },
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

test_that("library directories work", {
  dirs <- c(fs::path_temp("test_pkg", "R"), fs::path_temp("test_pkg", "bench"))
  temp_dir <- fs::path_temp()
  fs::dir_create(dirs)

  withr::with_options(list(
    touchstone.temp_dir = NULL,
    usethis.quiet = TRUE
  ), expect_error(add_lib_dirs("something"), "Temporary directory not found."))

  withr::with_options(list(
    touchstone.temp_dir = temp_dir,
    usethis.quiet = TRUE
  ), {
    expect_warning(add_lib_dirs("something"), "could not be found")
    expect_equal(add_lib_dirs(!!!dirs), temp_dir)
    expect_true(fs::is_dir(fs::path_join(c(temp_dir, "R"))))
  })
})
