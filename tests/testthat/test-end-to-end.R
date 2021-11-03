test_that("end to end run - code", {
  branches <- c("manila_master", "setova_feature")
  # feature is slower
  timings <- c(0.4, 0.8) %>%
    rlang::set_names(branches)
  local_package(branches = branches)
  for (branch in branches) {
    gert::git_branch_checkout(branch)
    code <- glue::glue(
      "f <- function() Sys.sleep(runif(1, {timings[branch]} - 0.05, {timings[branch]} + 0.05))"
    )
    writeLines(code, "R/core.R")
    gert::git_add(fs::path_file(fs::dir_ls(all = TRUE)))
    gert::git_commit_all(
      glue::glue("setup branch {branch}.")
    )
  }
  bm <- benchmark_run_ref(
    expr_before_benchmark = source("R/core.R"),
    bliblablup = f(),
    refs = branches,
    n = 2
  )
  out <- benchmark_read("bliblablup", branches) %>%
    dplyr::group_by(.data$ref) %>%
    dplyr::summarise(mean = mean(elapsed), sd = sd(elapsed))
  # expect diff around 2
  diff <- max(out$mean) / min(out$mean)
  expect_lt(diff, 2.5)
  expect_gt(diff, 1.5)
  out <- benchmark_analyze("bliblablup", branches)
  expect_match(
    out,
    glue::glue("bliblablup: .* -> .*\\[.*%, .*\\]")
  )
})
