test_that("can install in isolated repos", {
  name_tmp_pkg <- "bli33"
  local_package(name_tmp_pkg, r_sample = "x <- 3")
  lib_path1 <- branch_install("devel")
  expect_equal(
    withr::with_libpaths(lib_path1, bli33:::x), 3
  )
  # with root != "."
  name_tmp_pkg <- "bli44"
  local_package(name_tmp_pkg, r_sample = "x <- 55")
  expect_error(
    withr::with_libpaths(lib_path1, bli44:::x),
    "[Tt]here is no package"
  )
  lib_path2 <- branch_install("devel")
  expect_equal(
    withr::with_libpaths(lib_path2, bli44:::x), 55
  )
})

test_that("cache works", {
  ref <- "devel"
  name_tmp_pkg <- "bli44"
  path_pkg <- local_package(name_tmp_pkg, r_sample = "x <- 55")

  expect_equal(nrow(cache_get()), 0)
  expect_false(cache_up_to_date(ref, path_pkg))
  cache_update(ref, path_pkg)
  expect_equal(nrow(cache_get()), 1)
  writeLines(c("x <- 55"), "R/sample.R")
  expect_true(cache_up_to_date(ref, path_pkg))
  writeLines(c("22"), "R/sample.R")
  expect_false(cache_up_to_date(ref, path_pkg))
  expect_false(cache_up_to_date(ref, path_pkg))
  cache_update(ref, path_pkg)
  expect_true(cache_up_to_date(ref, path_pkg))

  # new ref
  ref <- "m2"
  expect_equal(nrow(cache_get()), 1)
  # prepare for case that remotes would ever have global cache across libraries
  # (currently not the case) and could think "version has not changed, just copying"
  expect_false(cache_up_to_date(ref, path_pkg))
  cache_update(ref, path_pkg)
  expect_equal(nrow(cache_get()), 2)
  cache_update(ref, path_pkg)
  expect_true(cache_up_to_date(ref, path_pkg))

  # new root
  cache <- cache_get()
  path_pkg2 <- local_package(name_tmp_pkg, r_sample = "c")
  options("touchstone.hash_source_package" = cache)
  expect_equal(nrow(cache_get()), 2)
  expect_false(cache_up_to_date(ref, path_pkg2))
  cache_update(ref, path_pkg2)
  expect_true(cache_up_to_date(ref, path_pkg2))
})
