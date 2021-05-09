#' Initiate {touchstone}
#'
#' @param cancel Whether or not to also introduce a GitHub Actions
#' [cancel workflow](https://github.com/marketplace/actions/cancel-workflow-action)
#' for canceling old runs when new ones are started. This makes sense because
#' touchstone runs can take a lot of time and compute resources and you usually
#' don't care about old runs when you pushed new code.
#' @export
use_touchstone <- function(cancel = TRUE) {
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
  workflows <- fs::dir_create(fs::path(".github", "workflows"))
  copy_if_not_exists(
    system.file("touchstone.yaml", package = "touchstone"),
    fs::path(workflows, "touchstone.yaml")
  )
  if (cancel) {
    target <- fs::path(workflows, "cancel.yaml")
    if (fs::file_exists(target)) {
      usethis::ui_warn(paste0(
        "Could not add a file `cancel.yaml` to",
        "`.github/workflows/` as there is already a file with this name. ",
        "Please rename it and try again if you want to make sure outdated ",
        "runs get cancelled. See `use_touchstone()` for details. Proceeding. "
      ))
    } else {
      copy_if_not_exists(
        system.file("cancel.yaml", package = "touchstone"),
        target
      )

      usethis::ui_done(paste(
        "Added a cancelling action for the touchstone workflow. A new push to a ",
        "branch will stop the current benchmarking run and start benchmarking ",
        "your latest push (instead of queuing it until the previous completed). ",
        "You can manually list other Github Actions workflows in the cancel workflow ",
        "to stop running outdated actions to save compute resources and time."
      ))
    }
  } else {
    usethis::ui_info(
      "Not adding a cancelling workflow. Frequent pushes to the same branch ",
      "will queue and potentially consume unnecessary energy, built time and ",
      "create long waiting times until touchstone results are available."
    )
  }
}


copy_if_not_exists <- function(path, new_path) {
  if (!fs::file_exists(new_path)) {
    fs::file_copy(
      path, new_path
    )
    usethis::ui_done("Populated file {fs::path_file(new_path)} in {fs::path_dir(new_path)}/.")
  } else {
    usethis::ui_warn("File {fs::path_file(new_path)} already exists at {fs::path_dir(new_path)}/, not copying.")
  }
}
