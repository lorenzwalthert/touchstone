#' Initiate {touchstone}
#'
#' @export
use_touchstone <- function() {
  # TODO needs test
  workflows <- fs::dir_create(fs::path(".github", "workflows"))
  fs::file_copy(
    system.file("touchstone.yaml", package = "touchstone"),
    workflows
  )
  fs::dir_create("touchstone")
  fs::file_copy(
    system.file("script.R", package = "touchstone"),
    fs::path("touchstone", "script.R")
  )
  fs::file_copy(
    system.file("config.json", package = "touchstone"),
    fs::path("touchstone", "config.json")
  )
  writeLines(
    c("*", "!script.R", "!config.json", "!.gitignore"),
    fs::path("touchstone", ".gitignore")
  )
}
