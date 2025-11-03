test_that("can read, write and list benchmark", {
  local_clean_touchstone()
  branch <- "hash"
  atomic <- bench::mark(1 + 1, iterations = 1)
  expect_silent(
    benchmark_write(atomic, name = "x1", branch = branch)
  )
  bm <- benchmark_read(branch, name = "x1")
  schema <- purrr::map_chr(bm, ~ class(.x)[1])
  expect_equal(schema, schema_disk())
  expect_equal(unique(benchmark_ls()$name), "x1")
  expect_equal(benchmark_ls(name = "x1"), tibble::tibble(name = "x1", branch = "hash"))
})

test_that("does fail infomatively if there is no benchmark", {
  local_clean_touchstone()
  expect_equal(benchmark_ls(), tibble::tibble(name = character(), branch = character()))
})


test_that("fails on corrupt benchmark", {
  local_clean_touchstone()
  branch <- "malformed"
  multiple <- bench::mark(1 + 1, iterations = 5)
  expect_error(
    benchmark_write(multiple, branch = branch),
    "only supports"
  )
})
