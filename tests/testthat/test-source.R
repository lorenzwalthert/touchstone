test_that("can call package in script", {
  refs <- c("devel", "main")
  pkg_name <- "b32jk"
  path_test_pkg <- local_package(pkg_name, branches = refs)
  # install refs, so ref[1] will be available outside benchmark_run_ref,
  # not modifying libpath permanently
  path_touchstone <- path_touchstone_script()
  fs::dir_create(fs::path_dir(path_touchstone))
  refs_dput <- capture.output(dput(refs))
  writeLines(
    glue::glue(
      "refs_install({refs_dput}, '{path_test_pkg}', install_dependencies = FALSE)",
      "library({pkg_name})", # can call package
      "touchstone::benchmark_run_ref(",
      "  refs = {refs_dput}, x = '2', path_pkg = '{path_test_pkg}',",
      "  n = 1",
      ")",
      .sep = "\n"
    ),
    path_touchstone
  )
  expect_error(with_touchstone_lib(path_touchstone, ref = refs[2]), NA)
})
