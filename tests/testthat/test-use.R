cli::test_that_cli("can initialize with cli", {
  local_package()
  expect_snapshot(use_touchstone())
  expect_match(conditionMessage(capture_warning(use_touchstone())), "already exists")
})
