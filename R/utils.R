#' Touchstone managers
#'
#' Utilities to manage the touchstone database.
#' @name touchstone_managers
NULL

#' @describeIn touchstone_managers returns the directory where the touchstone database lives.
#' @aliases touchstone_managers
#' @export
dir_touchstone <- function() {
  "touchstone"
}


#' @describeIn touchstone_managers clears the touchstone database.
#' @aliases touchstone_managers
#' @param all Whether to clear the whole touchstone directory or just the
#'   records sub directory.
#' @export
touchstone_clear <- function(all = FALSE) {
  paths <- fs::path(dir_touchstone(), if (!all) "records" else "")

  paths <- paths[fs::dir_exists(paths)]
  fs::dir_delete(paths)
}

#' Evaluate an expression
#'
#' @param text Character vector with code to evaluate.
#' @keywords internal
exprs_eval <- function(...) {
  eval(parse(text = c(...)))
}

ref_upsample <- function(ref, n = 20) {
  ref <- unique(ref)
  sample(rep(ref, length.out = n))
}

ensure_dir <- function(...) {
  fs::dir_create(...)
}



schema_disk <- function() {
  c(
    elapsed = "numeric", iteration = "integer", ref = "character"
  )
}
