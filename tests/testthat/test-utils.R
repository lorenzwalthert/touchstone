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
  new_branch <- "ewjlkj"
  tmp <- withr::local_tempdir()
  fs::dir_create(tmp)
  fs::file_touch(fs::path(tmp, ".gitignore"))
  gert::git_init(tmp)
  gert::git_config_set("user.name", "GitHub Actions", repo = tmp)
  gert::git_config_set("user.email", "actions@github.com", repo = tmp)
  gert::git_add(".gitignore", repo = tmp)
  gert::git_commit("initial", repo = tmp)
  gert::git_branch_create(new_branch, repo = tmp, checkout = FALSE)
  gert::git_branch_list(repo = tmp)
  test_f <- function(tmp, new_branch) {
    local_git_checkout(new_branch, tmp)
    expect_equal(gert::git_branch(tmp), new_branch)
  }
  test_f(tmp = tmp, new_branch = new_branch)
  expect_equal(gert::git_branch(tmp), "master")
})
