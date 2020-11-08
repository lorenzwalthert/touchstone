test_that("iterations can be run", {
  local_clean_touchstone()
  bm <- benchmark_run_iteration(
    expr_before_benchmark = "library(styler)",
    expr_to_benchmark = 'styler::style_text("1 + 1")',
    ref = "benchmark_run_iteration",
    n = 2
  )
  schema <- purrr::map_chr(bm, ~ class(.x)[1])
  expect_equal(schema, schema_disk())
})


test_that("refs can be run", {
  local_clean_touchstone()
  path_test_pkg <- local_package()
  bm <- benchmark_run_ref(
    "master",
    expr_before_benchmark = "library(styler)",
    expr_to_benchmark = 'styler::style_text("1 + 1")',
    n = 2,
    path_pkg = path_test_pkg
  )
  schema <- purrr::map_chr(bm, ~ class(.x)[1])
  expect_equal(schema, schema_disk())
})
