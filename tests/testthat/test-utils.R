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
