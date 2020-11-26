test_that("iterations can be run", {
  local_clean_touchstone()
  bm <- benchmark_run_iteration(
    expr_before_benchmark = "library(testthat)",
    expr_to_benchmark = "expect_equal(Sys.sleep(1e-3), NULL)",
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
    expr_before_benchmark = "library(testthat)",
    bliblablup = "expect_equal(Sys.sleep(1e-3), NULL)",
    n = 2,
    path_pkg = path_test_pkg
  )
  schema <- purrr::map_chr(bm, ~ class(.x)[1])
  expect_equal(schema, schema_disk())
})

test_that("dynamic dots are supported", {
  local_clean_touchstone()
  path_test_pkg <- local_package()
  x <- "cc"
  bm <- benchmark_run_ref(
    "master",
    expr_before_benchmark = "",
    !!x := "Sys.sleep(0)",
    n = 1,
    path_pkg = path_test_pkg
  )
  schema <- purrr::map_chr(bm, ~ class(.x)[1])
  expect_equal(schema, schema_disk())
  vec <- c("xzy" = "Sys.sleep(0)")
  bm <- benchmark_run_ref(
    "master",
    expr_before_benchmark = "",
    !!!vec,
    n = 1,
    path_pkg = path_test_pkg
  )
  schema <- purrr::map_chr(bm, ~ class(.x)[1])
  expect_equal(schema, schema_disk())
})
