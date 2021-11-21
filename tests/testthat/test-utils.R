test_that("branch can be sampled", {
  withr::with_seed(
    3,
    expect_snapshot_value(branches_upsample(c("main", "issue-3")), style = "serialize")
  )
  withr::with_seed(
    3,
    expect_snapshot_value(branches_upsample(letters, n = 2), style = "serialize")
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
  branches <- c("devel", "main")
  if (is_windows()) {
    # cannot have touchstone library in temp, as lib path comparison becomes
    # unfeasible due to short/name notation
    withr::local_options(dir_touchstone = fs::path_file(fs::file_temp()))
  }
  path_pkg <- local_package(setwd = !is_windows())
  new_libpaths <- branches_install(branches, path_pkg, install_dependencies = FALSE)

  withr::local_libpaths(new_libpaths)

  expect_equal(.libPaths(), new_libpaths)
  local_without_touchstone_lib(path_pkg)
  after_removal <- setdiff(
    new_libpaths,
    as.character(fs::path_abs(libpath_touchstone(branches)))
  )

  expect_equal(after_removal, .libPaths())
})


test_that("Can abort with missing branches for benchmark run", {
  withr::local_envvar(GITHUB_BASE_REF = NA, GITHUB_HEAD_REF = NA)
  mockery::stub(
    benchmark_run, "force",
    function(...) cli::cli_abort("12321")
  )
  withr::local_envvar(list(
    GITHUB_HEAD_REF = "feature1",
    GITHUB_BASE_REF = "mastero"
  ))

  expect_error(
    benchmark_run(x1 = print("hi")),
    "12321"
  )
})


test_that("Can abort with missing branches for benchmark run", {
  withr::local_envvar(GITHUB_BASE_REF = NA, GITHUB_HEAD_REF = NA)
  match <- "^If you don't specify"
  expect_error(branch_get_or_fail("SOME_REF"), match)
  expect_error(
    benchmark_run(x1 = print("hi")),
    match
  )
  expect_error(
    benchmark_analyze("sume2"),
    match
  )

  expect_error(
    branches_install(),
    match
  )
})

test_that("assets work on HEAD", {
  mockery::stub(pin_assets, "local_git_checkout", TRUE)
  dirs <- c(fs::path_temp("test_pkg", "R"), fs::path_temp("test_pkg", "bench"))
  files <- c(fs::path_temp("test_pkg", "data.R"), fs::path_temp("test_pkg", "utils.R"))

  temp_dir <- fs::path_temp()
  fs::dir_create(dirs)
  writeLines("test", fs::path(dirs[1], "sample.R"))
  fs::file_create(files)
  dirs <- fs::path_real(dirs)
  files <- fs::path_real(files)
  withr::local_envvar(list(
    GITHUB_BASE_REF = "main",
    GITHUB_HEAD_REF = "devel"
  ))

  withr::with_options(
    list(
      touchstone.dir_assets_devel = NULL,
      touchstone.git_root = fs::path_real(fs::path_temp("test_pkg")),
      usethis.quiet = TRUE
    ),
    {
      expect_error(pin_assets("something"), "Temporary directory for branch")
      expect_error(path_pinned_asset("something"), "Temporary directory ")
    }
  )

  withr::with_options(list(
    touchstone.dir_assets_devel = temp_dir,
    touchstone.git_root = fs::path_real(fs::path_temp("test_pkg")),
    usethis.quiet = TRUE
  ), {
    expect_warning(pin_assets("something", dirs[[1]]), "could not be found")
    expect_error(suppressWarnings(pin_assets("something")), "No valid")
    expect_equal(pin_assets(!!!dirs), temp_dir)
    expect_equal(
      readLines(
        fs::path(
          path_pinned_asset(
            fs::path_rel(dirs[1], getOption("touchstone.git_root"))
          ),
          "sample.R"
        )
      ),
      "test"
    )
    expect_equal(pin_assets(!!!files), temp_dir)
    expect_true(fs::is_dir(fs::path_join(c(temp_dir, "R"))))
    expect_true(fs::is_file(fs::path_join(c(temp_dir, "data.R"))))

    # expect_error(path_pinned_asset("something", branch = "no-branch"), "for head or base")
    expect_error(path_pinned_asset("something"), "not pinned at")
    expect_equal(path_pinned_asset("R"), fs::path(temp_dir, "R"))
    expect_equal(path_pinned_asset("data.R"), fs::path(temp_dir, "data.R"))
  })
})

test_that("assets work HEAD and BASE", {
  branches <- c("rc-1.0", "feat")
  local_asset_dir(!!!branches)
  git_root <- local_package(branches = branches)
  dirs <- c("R", "bench") %>% rlang::set_names(branches)
  files <- c("data.Rdata", "utils.R") %>% rlang::set_names(branches)
  withr::local_options(list(
    touchstone.git_root = fs::path_real(git_root)
  ))

  for (branch in branches) {
    gert::git_branch_checkout(branch)
    fs::dir_create(dirs[branch])
    fs::file_create(files[branch])
  }

  withr::local_envvar(list(
    GITHUB_BASE_REF = branches[[1]],
    GITHUB_HEAD_REF = branches[[2]]
  ))

  for (branch in branches) {
    pin_assets(dirs[branch], files[branch], branch = branch)
  }

  expect_true(
    fs::file_exists(
      path_pinned_asset("data.Rdata", branch = branches[[1]])
    )[[1]]
  )
  expect_true(
    fs::file_exists(
      path_pinned_asset("utils.R", branch = branches[[2]])
    )[[1]]
  )
})

test_that("asset paths are fetched correctly", {
  withr::local_options(list(
    touchstone.dir_assets_devel = "asset/dir",
    touchstone.dir_assets_main = NULL
  ))

  withr::local_envvar(list(
    GITHUB_BASE_REF = "main",
    GITHUB_HEAD_REF = "devel"
  ))

  # expect_error(get_asset_dir("not-main"), "head or base")
  expect_error(get_asset_dir("main"), "directory for branch")
  expect_equal(get_asset_dir("devel"), "asset/dir")
})

cli::test_that_cli("git root is found correctly", {
  no_git <- fs::path_temp("no-git")
  with_git <- fs::path_temp("with-git")
  deeper_git <- fs::path_temp("with-git", "deep", "deeper")
  fs::dir_create(c(no_git, with_git, deeper_git))

  no_git <- fs::path_real(no_git)
  with_git <- fs::path_real(with_git)
  deeper_git <- fs::path_real(deeper_git)

  withr::with_dir(with_git, {
    gert::git_init()
  })

  expect_snapshot(find_git_root(no_git))
  expect_equal(find_git_root(with_git), as.character(with_git))
  expect_equal(find_git_root(deeper_git), as.character(with_git))
})
