test_that("can initialize", {
  local_test_setup()
  withr::with_tempdir(
    expect_silent(use_touchstone())
  )
  withr::with_tempdir({
    use_touchstone()
    expect_match(conditionMessage(capture_warning(use_touchstone())), "not cop")
  })
})
