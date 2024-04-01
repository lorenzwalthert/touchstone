test_that("iterations can be run", {
  local_package()
  bm <- benchmark_run_iteration(
    expr_before_benchmark = library(testthat),
    dots = list(expr_to_benchmark = expect_equal(Sys.sleep(1e-3), NULL)),
    branch = "benchmark_run_iteration",
    block = 1,
    n = 2
  )
  schema <- purrr::map_chr(bm, ~ class(.x)[1])
  expect_equal(schema, schema_disk())
})


test_that("branches can be run", {
  path_test_pkg <- local_package()
  bm <- benchmark_run(
    expr_before_benchmark = library(testthat),
    bliblablup = expect_equal(Sys.sleep(1e-3), NULL),
    branches = "main",
    n = 2
  )
  schema <- purrr::map_chr(bm, ~ class(.x)[1])
  expect_equal(schema, schema_disk())
})


test_that("string input gives error", {
  path_test_pkg <- local_package()
  expect_error(
    benchmark_run(
      expr_before_benchmark = "library(testthat)",
      bliblablup = expect_equal(Sys.sleep(1e-3), NULL),
      branches = "main",
      n = 2
    ),
    "is deprecated."
  )
})

test_that("string input gives error", {
  path_test_pkg <- local_package()
  expect_error(
    benchmark_run(
      expr_before_benchmark = library(testthat),
      bliblablup = "expect_equal(Sys.sleep(1e-3), NULL)",
      branches = "main",
      n = 2
    ),
    "is deprecated."
  )
})


test_that("dynamic dots are supported", {
  local_package()
  x <- "cc"
  bm <- benchmark_run(
    expr_before_benchmark = {},
    !!x := rlang::expr(Sys.sleep(0)),
    branches = "main",
    n = 1
  )
  schema <- purrr::map_chr(bm, ~ class(.x)[1])
  expect_equal(schema, schema_disk())
  vec <- c(xzy = rlang::expr(Sys.sleep(0)))
  bm <- benchmark_run(
    expr_before_benchmark = {},
    !!!vec,
    branches = "main",
    n = 1
  )
  schema <- purrr::map_chr(bm, ~ class(.x)[1])
  expect_equal(schema, schema_disk())
})
