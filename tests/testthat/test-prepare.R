test_that("can install in isolated repos", {
  name_tmp_pkg <- "bli33"
  path_pkg <- local_package(name_tmp_pkg, r_sample = "x <- 3")
  lib_path1 <- refs_install("devel")
  expect_equal(
    withr::with_libpaths(lib_path1, bli33:::x), 3
  )
  # with root != "."
  name_tmp_pkg <- "bli44"
  path_pkg <- local_package(name_tmp_pkg, r_sample = "x <- 55")
  # browser()
  expect_error(
    withr::with_libpaths(lib_path1, bli44:::x),
    "[Tt]here is no package"
  )
  lib_path2 <- refs_install("devel", path_pkg = path_pkg)
  expect_equal(
    withr::with_libpaths(lib_path2, bli44:::x), 55
  )
})
