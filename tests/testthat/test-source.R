test_that("can call package in script", {
  ref <- "devel"
  pkg_name <- "b32jk"
  local_test_setup()
  path_test_pkg <- local_package(path_temp_pkg(pkg_name), branches = ref)
  # install into ref, not modifying libpath permanently
  refs_install(ref, path_test_pkg, FALSE)
  path_touchstone <- path_touchstone_script(path_test_pkg)
  fs::dir_create(fs::path_dir(path_touchstone))
  writeLines(
    glue::glue(
      "library({pkg_name})", # can call package
      "touchstone::benchmark_run_ref(",
      "  refs = '{ref}', x = '1', path_pkg = '{path_test_pkg}',",
      "  n = 1",
      ")",
      .sep = "\n"
    ),
    path_touchstone
  )
  expect_error(with_touchstone_lib(path_touchstone, ref = ref), NA)
})
