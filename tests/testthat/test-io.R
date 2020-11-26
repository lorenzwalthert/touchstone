test_that("can read, write and list benchmark", {
  local_clean_touchstone()
  ref <- "hash"
  atomic <- bench::mark(1 + 1, iterations = 1)
  expect_silent(
    benchmark_write(atomic, name = "x1", ref = ref)
  )
  bm <- benchmark_read(ref, name = "x1")
  schema <- purrr::map_chr(bm, ~ class(.x)[1])
  expect_equal(schema, schema_disk())
  expect_equal(benchmark_ls(), "x1")
  expect_equal(benchmark_ls(name = "x1"), "hash")
})

test_that("does fail infomatively if there is no benchmark", {
  local_clean_touchstone()
  expect_equal(benchmark_ls(), character())
})



test_that("fails on corrupt benchmark", {
  local_clean_touchstone()
  ref <- "malformed"
  multiple <- bench::mark(1 + 1, iterations = 5)
  expect_error(
    benchmark_write(multiple, ref = ref),
    "iterations = 1"
  )
})
