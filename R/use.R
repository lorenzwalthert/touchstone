#' Initiate {touchstone}
#'
#' @export
use_touchstone <- function() {
  workflows <- fs::dir_create(fs::path(".github", "workflows"))
  copy_if_not_exists(
    system.file("touchstone.yaml", package = "touchstone"),
    fs::path(workflows, "touchstone.yaml")
  )
  dir_touchstone <- dir_touchstone()
  fs::dir_create(dir_touchstone)
  copy_if_not_exists(
    system.file("script.R", package = "touchstone"),
    path_touchstone_script()
  )
  copy_if_not_exists(
    system.file("config.json", package = "touchstone"),
    fs::path(dir_touchstone, "config.json")
  )

  copy_if_not_exists(
    system.file("gitignore", package = "touchstone"),
    fs::path(dir_touchstone, ".gitignore")
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
