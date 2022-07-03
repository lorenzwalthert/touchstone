#' Initiate {touchstone}
#'
#' This function will initialize {touchstone} in your package repository, use
#' from root directory.
#'
#' @inheritParams use_touchstone_workflows
#' @details
#' For more information see the 'Using touchstone' vignette:
#'  `vignette("touchstone", package = "touchstone")
#' @return
#' The function is called for its side effects and returns `NULL` (invisibly).
#' @examples
#' \dontrun{
#' # within your repository
#' use_touchstone()
#' }
#' @export
use_touchstone <- function(overwrite = FALSE,
                           on_comment = FALSE,
                           command = "/benchmark",
                           limit_to = c("OWNER", "MEMBER", "COLLABORATOR"),
                           force_upstream = TRUE) {
  dir_touchstone <- dir_touchstone()
  fs::dir_create(dir_touchstone)
  has_written_script <- copy_if_not_exists(
    system.file("script.R", package = "touchstone"),
    path_touchstone_script(),
    overwrite
  )

  copy_if_not_exists(
    system.file("header.R", package = "touchstone"),
    fs::path(dir_touchstone, "header.R"),
    overwrite
  )

  copy_if_not_exists(
    system.file("footer.R", package = "touchstone"),
    fs::path(dir_touchstone, "footer.R"),
    overwrite
  )

  copy_if_not_exists(
    system.file("config.json", package = "touchstone"),
    fs::path(dir_touchstone, "config.json"),
    overwrite
  )

  copy_if_not_exists(
    system.file("gitignore", package = "touchstone"),
    fs::path(dir_touchstone, ".gitignore"),
    overwrite
  )

  use_touchstone_workflows(
    overwrite = overwrite,
    on_comment = on_comment,
    command = command,
    limit_to = limit_to,
    force_upstream = force_upstream
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
    "Commit and push to GitHub to the default branch to activate the ",
    "workflow, then ",
    ifelse(on_comment, "comment '{command}' on", "make"),
    " a pull request to trigger your first benchmark run."
  ))
  invisible(NULL)
}


copy_if_not_exists <- function(path, new_path, overwrite = FALSE) {
  if (!fs::file_exists(new_path) || overwrite) {
    fs::file_copy(
      path,
      new_path,
      overwrite
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

#' Use touchstone GitHub Actions Workflows
#'
#' This function will add (or update) the {touchstone} GitHub Actions workflows
#' to your package repository. Use in the root directory of your repository.
#' This function will be called by [touchstone::use_touchstone()], you should
#' only need to call it to update the workflows or change their parameters.
#' @param overwrite Overwrites files if they exist.
#' @param on_comment Don't benchmark every commit. Only trigger on PR
#'  comments starting with `command`.
#' @param command The command to use to trigger the benchmark with a comment.
#' @param limit_to Roles that are allowed to trigger the benchmark workflow
#'   via comment. See details for a list of roles and their definition.
#'   Set to `NULL` to allow everyone to trigger a benchmark.
#' @param force_upstream Always benchmark against the upstream base branch.
#' @return
#' @details
#' Possible roles for `limit_to`:
#' - `OWNER`: Owner of the repository, e.g. user for user/repo.
#'   - It is undocumented who holds this status in an org.
#' - `MEMBER`: Member of org for org/repo.
#' - `COLLABORATOR`: Anyone who was added as a collaborator to a repository.
#' - `CONTRIBUTOR`: Anyone who has contributed any commit to the repository.
#'
#' Each user has only one role and the check does not interpolate permissions,
#' so you have to add all roles whom you want to have permission to start the
#' benchmark. So if you only add "COLLABORATOR" the owner will not be able to
#' start the benchmark.
#'
#' GitHub will recognize additional, mostly unusual roles, see the
#' [documentation](https://docs.github.com/en/rest/issues/comments).
#' @export
use_touchstone_workflows <- function(overwrite = FALSE,
                                     on_comment = FALSE,
                                     command = "/benchmark",
                                     limit_to = c("OWNER", "MEMBER", "COLLABORATOR"),
                                     force_upstream = TRUE) {
  workflows <- fs::dir_create(fs::path(".github", "workflows"))
  copy_if_not_exists(
    system.file("touchstone-comment.yaml", package = "touchstone"),
    fs::path(workflows, "touchstone-comment.yaml"),
    overwrite
  )

  template <- readLines(
    system.file("touchstone-receive.yaml", package = "touchstone")
  )

  trigger <- "\n  pull_request:"
  ward <- ""
  force <- ifelse(force_upstream, "\n          force_upstream: true", "")

  if (!is.null(limit_to)) {
    limit <- glue::glue_collapse(
      glue::glue("        github.event.comment.author_association == '{limit_to}' "),
      sep = "||\n"
    )
    limit <- glue::glue(
      "&&\n",
      "      (\n",
      "{limit}\n",
      "      )"
    )
  } else {
    limit <- ""
  }

  if (on_comment) {
    # these have to be indented with 2 spaces per tab,
    # yaml does not allow tabs
    trigger <- glue::glue(
      "\n  issue_comment:\n",
      "    types: ['created', 'edited']",
      .trim = FALSE
    )

    ward <- glue::glue(
      "\n    if:\n",
      "      github.event.issue.pull_request &&\n",
      "      startsWith(github.event.comment.body, '{command}') ",
      limit,
      .trim = FALSE
    )
  }

  # without as.character an additional newline is added
  wf <- sub("#- trigger", as.character(trigger), template)
  wf <- sub("#- ward", as.character(ward), wf)
  wf <- sub("#- force", as.character(force), wf)

  temp_wf <- fs::file_temp("receive.yml")
  writeLines(wf, temp_wf)

  copy_if_not_exists(
    temp_wf,
    fs::path(workflows, "touchstone-receive.yaml"),
    overwrite
  )

  invisible(NULL)
}
