#' The directory where the touchstone database lives
#' @keywords internal
dir_touchstone <- function() {
  "touchstone"
}

#' Evaluate an expression
#'
#' @param text Character vector with code to evaluate.
#' @keywords internal
expr_eval <- function(text) {
  eval(parse(text = text))
}

ref_upsample <- function(ref, n = 20) {
  ref <- unique(ref)
  sample(rep(ref, length.out = n))
}

ensure_touchstone_dir <- function() {
  fs::dir_create(dir_touchstone())
}

touchstone_clear <- function() {
  paths <- dir_touchstone()
  paths <- paths[fs::dir_exists(paths)]
  fs::dir_delete(paths)
}

schema_disk <- function() {
  c(
    elapsed = "numeric", iteration = "integer", ref = "character"
  )
}
