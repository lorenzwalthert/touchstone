#' Initiate {touchstone}
#'
#' This function will initialize {touchstone} in your package repository, use
#' from root directory.
#' @details
#' For more information see the 'Using touchstone' vignette:
#'  `vignette("touchstone", package = "touchstone")
#' @return
#' The function is called for its side effects and returns `NULL` (invisibly).
#' @export
use_touchstone <- function() {
  dir_touchstone <- dir_touchstone()
  fs::dir_create(dir_touchstone)
  has_written_script <- copy_if_not_exists(
    system.file("script.R", package = "touchstone"),
    path_touchstone_script()
  )

  copy_if_not_exists(
    system.file("header.R", package = "touchstone"),
    fs::path(dir_touchstone, "header.R")
  )

  copy_if_not_exists(
    system.file("footer.R", package = "touchstone"),
    fs::path(dir_touchstone, "footer.R")
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
    system.file("touchstone-receive.yaml", package = "touchstone"),
    fs::path(workflows, "touchstone-receive.yaml")
  )

  copy_if_not_exists(
    system.file("touchstone-comment.yaml", package = "touchstone"),
    fs::path(workflows, "touchstone-comment.yaml")
  )

  append_rbuildignore("touchstone")

  if (has_written_script) {
    cli::cli_ul(
      "Replace the mtcars sample code in `touchstone/script.R` with code from your package you want to benchmark."
    )
  }

  cli::cli_alert_info(
    "You can modify the PR comment, see {.code ?touchstone::pr_comment}."
  )

  cli::cli_ul(paste0(
    "Commit and push to GitHub to the default branch to activate the workflow, ",
    "then make a pull request to trigger your first benchmark run."
  ))
  invisible(NULL)
}


copy_if_not_exists <- function(path, new_path) {
  if (!fs::file_exists(new_path)) {
    fs::file_copy(
      path, new_path
    )
    cli::cli_alert_success("Populated file {.file {fs::path_file(new_path)}} in {.path {fs::path_dir(new_path)}/}.")
    TRUE
  } else {
    cli::cli_warn(paste0(
      "File {.file {fs::path_file(new_path)}} already exists",
      " at {.path {fs::path_dir(new_path)}/}, not copying."
    ))
    FALSE
  }
}
