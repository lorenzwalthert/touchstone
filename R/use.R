#' Initiate {touchstone}
#'
#' @export
use_touchstone <- function() {
  workflows <- fs::dir_create(fs::path(".github", "workflows"))
  copy_if_not_exists(
    system.file("touchstone.yaml", package = "touchstone"),
    fs::path(workflows, "touchstone.yaml")
  )
  fs::dir_create("touchstone")
  copy_if_not_exists(
    system.file("script.R", package = "touchstone"),
    fs::path("touchstone", "script.R")
  )
  copy_if_not_exists(
    system.file("config.json", package = "touchstone"),
    fs::path("touchstone", "config.json")
  )

  copy_if_not_exists(
    system.file(".gitignore", package = "touchstone"),
    fs::path("touchstone", ".gitignore")
  )
}


copy_if_not_exists <- function(path, new_path) {
  if (!fs::file_exists(new_path)) {
    fs::file_copy(
      path, new_path
    )
  } else {
    usethis::ui_warn("File {path} already exists at {fs::path_abs(new_path)}, not copying.")
  }
}
