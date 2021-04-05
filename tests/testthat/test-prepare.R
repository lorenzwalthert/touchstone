test_that("can install in isolated repos", {
  local_test_setup()
  name_tmp_pkg <- "bli33"
  path_pkg <- local_package(path_temp_pkg(name_tmp_pkg), r_sample = "x <- 3")
  lib_path1 <- refs_install("devel", path_pkg = path_pkg)
  expect_equal(
    withr::with_libpaths(lib_path1, bli33:::x), 3
  )

  name_tmp_pkg <- "bli44"
  path_pkg <- local_package(path_temp_pkg(name_tmp_pkg), r_sample = "x <- 55")
  lib_path2 <- refs_install("devel", path_pkg = path_pkg)
  expect_equal(
    withr::with_libpaths(lib_path1, bli44:::x), 55
  )
})
