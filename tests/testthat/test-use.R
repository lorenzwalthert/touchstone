cli::test_that_cli("can initialize with cli", {
  local_package()
  expect_snapshot(use_touchstone())
  expect_match(conditionMessage(capture_warning(use_touchstone())), "already exists")
})

test_that("Workflow template is modified correctly", {
  file <- fs::dir_create(fs::path(".github", "workflows"))
  file <- fs::path(file, "touchstone-receive.yaml")
  use_touchstone_workflows(overwrite = TRUE)
  expect_snapshot_file(file, "receive_default.yml")

  use_touchstone_workflows(overwrite = TRUE, command = "/benchmark")
  expect_snapshot_file(file, "receive_command.yml")

  use_touchstone_workflows(overwrite = TRUE, limit_to = "OWNER")
  expect_snapshot_file(file, "receive_limit.yml")

  use_touchstone_workflows(overwrite = TRUE, command = "/benchmark", limit_to = "OWNER", force_upstream = FALSE)
  expect_snapshot_file(file, "receive_all.yml")
})
