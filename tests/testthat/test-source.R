test_that("can call package in script", {
  refs <- c("main", "devel")
  pkg_name <- "b32jk"
  path_test_pkg <- local_package(pkg_name, branches = refs)
  # install refs, so ref[1] will be available outside benchmark_run_ref,
  # not modifying libpath permanently
  path_touchstone <- path_touchstone_script()
  withr::local_options(touchstone.git_root = path_test_pkg)
  fs::dir_create(fs::path_dir(path_touchstone))
  refs_dput <- capture.output(dput(refs))
  path_wordlist <- fs::path(path_test_pkg, "inst", "WORDLIST")
  ensure_dir(fs::path_dir(path_wordlist))
  writeLines("a\n\nc", path_wordlist)

  no_assets <- glue::glue(
    "refs_install({refs_dput}, '{path_test_pkg}', install_dependencies = FALSE)",
    "library({pkg_name})", # can call package
    "touchstone::benchmark_run_ref(",
    "  refs = {refs_dput}, x = '2', path_pkg = '{path_test_pkg}',",
    "  n = 1",
    ")",
    .sep = "\n"
  )

  with_assets <- glue::glue(
    "refs_install({refs_dput}, '{path_test_pkg}', install_dependencies = FALSE)",
    "library({pkg_name})", # can call package
    "path <- 'inst/WORDLIST'",
    "touchstone::pin_assets(path, ref = 'main')",
    "touchstone::benchmark_run_ref(",
    "  expr_before_benchmark = readLines(touchstone::path_pinned_asset(!! path, ref = 'main')),",
    "  refs = {refs_dput}, x = '2', path_pkg = '{path_test_pkg}',",
    "  n = 1",
    ")",
    .sep = "\n"
  )

  writeLines(no_assets, path_touchstone)
  withr::local_envvar(GITHUB_BASE_REF = "")
  expect_error(run_script(path_touchstone, ref = refs[[2]]), "you must set the environment variable `GITHUB_BASE_REF`")

  withr::local_envvar(list(GITHUB_BASE_REF = "main", GITHUB_HEAD_REF = "devel"))
  expect_error(run_script(path_touchstone, ref = refs[[2]]), NA)
  writeLines(with_assets, path_touchstone)
  expect_error(run_script(path_touchstone), NA)
})
