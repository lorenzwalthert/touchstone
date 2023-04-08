#' Turn raw benchmark results into text and figures
#'
#' @details
#' Creates two side effects:
#'
#' * Density plots for each element in `branches` are written to
#'   `touchstone/plots`.
#' * A text explaining the speed diff is written to
#'   `touchstone/pr-comment/info.txt` for every registered benchmarking
#'   expression. See `vignette("inference", package = "touchstone")` for
#'   details.
#' @param branches The names of the branches for which analysis should be
#'   created.
#' @param names The names of the benchmarks to analyze. If `NULL`, all
#'   benchmarks with the `branches` are taken.
#' @param ci The confidence level, defaults to 95%.
#' @details Requires [dplyr::dplyr], [ggplot2::ggplot2] and [glue::glue].
#' @return
#' A character vector that summarizes the benchmarking results.
#' @export
benchmark_analyze <- function(branches = c(
                                branch_get_or_fail("GITHUB_BASE_REF"),
                                branch_get_or_fail("GITHUB_HEAD_REF")
                              ),
                              names = NULL,
                              ci = 0.95) {
  suggested_pkgs <- c("dplyr", "ggplot2", "glue")
  suggests_available <- purrr::map_lgl(
    suggested_pkgs,
    requireNamespace,
    quietly = TRUE
  )

  if (!all(suggests_available)) {
    missing_pkgs <- suggested_pkgs[!suggests_available]
    n_pkgs <- length(missing_pkgs)
    pkgs_str <- paste0('"', missing_pkgs, '"', collapse = ",")
    cli::cli_abort(c(
      "Analysing the benchmarks requires {n_pkgs} additional package{?s}!",
      "i" = "To install use {.code install.packages(c({pkgs_str}))}"
    ))
  }

  if (length(branches) != 2) {
    cli::cli_abort("There must be exactly two branches to comare.")
  }
  path_info <- path_pr_comment()
  default_header <- paste0(
    "This is how benchmark results would change (along with a ", 100 * ci,
    "% confidence interval in relative change) if ",
    system2("git", c("rev-parse", "HEAD"), stdout = TRUE),
    " is merged into ", branches[1], ":", "\n"
  )

  get_comment_text("header", default_header) %>%
    writeLines(path_info)

  if (is.null(names)) {
    # only select names that occur exactly twice
    names <- benchmark_ls() %>%
      dplyr::filter(.data$branch %in% !!branches) %>%
      dplyr::group_by(.data$name) %>%
      dplyr::count()

    filtered_names <- dplyr::filter(names, .data$n == 2)
    if (!identical(names, filtered_names)) {
      cli::cli_warn(c(
        "All benchmarks to analyse must have the two branches  {.val {branches[[1]]}} and {.val {branches[[2]]}}",
        "!" = "Ignoring all benchmarks that don't have exactly those two branches.",
        "i" = "To avoid this warning, inspect the existing benchmarks with {.fun touchstone::benchmark_ls}"
      ))
      names <- filtered_names
    }
  }

  out <- purrr::walk(
    names$name,
    benchmark_analyze_impl,
    branches = branches, ci = ci
  )
  default_footer <- paste(
    "\nFurther explanation regarding interpretation and methodology can be found",
    "in the [documentation](https://lorenzwalthert.github.io/touchstone/articles/inference.html)."
  )
  text <- get_comment_text("footer", default_footer)
  cat(text, fill = TRUE, file = path_info, append = TRUE)

  readLines(path_info)
}

#' @importFrom rlang .data
benchmark_analyze_impl <- function(benchmark, branches = c(
                                     branch_get_or_fail("GITHUB_BASE_REF"),
                                     branch_get_or_fail("GITHUB_HEAD_REF")
                                   ),
                                   ci = 0.95) {
  timings <- benchmark_read(benchmark, branches)
  benchmark_plot(benchmark, timings)
  benchmark_verbalize(benchmark, timings = timings, branches = branches, ci = ci)
}

#' Create nice text from benchmarks
#'
#' `branches` must be passed because the order is relevant.
#' @inheritParams benchmark_plot
#' @keywords internal
benchmark_verbalize <- function(benchmark, timings, branches, ci) {
  tbl <- timings %>%
    dplyr::group_by(.data$branch) %>%
    dplyr::summarise(
      mean = bench::as_bench_time(mean(.data$elapsed)),
      sd = stats::sd(.data$elapsed)
    ) %>%
    dplyr::inner_join(tibble::tibble(branch = branches), ., by = "branch")

  if (nrow(tbl) > 2) {
    cli::cli_abort("Benchmarks with more than two {.val branches} cannot be verbalized.")
  }
  confint <- confint_relative_get(timings, branches, as.numeric(tbl$mean[1]), ci = ci)

  text <- glue::glue(
    "* {confint$emoji}{benchmark}: {tbl$mean[1]} -> {tbl$mean[2]} {confint$string}"
  )
  cat(
    text,
    fill = TRUE, file = path_pr_comment(),
    append = TRUE
  )
  text
}

set_sign <- function(x) {
  purrr::map_chr(x, ~ paste0(ifelse(.x > 0, "+", ""), .x))
}

confint_relative_get <- function(timings, branches, reference, ci) {
  no_change <- "&nbsp;&nbsp;:ballot_box_with_check:"
  slower <- ":exclamation::snail:"
  faster <- "&nbsp;&nbsp;:rocket:"

  timings_with_factors <- timings %>%
    dplyr::mutate(
      block = factor(.data$block), branch = factor(.data$branch, levels = branches)
    )
  stopifnot(inherits(timings_with_factors$branch, "factor"))
  fit <- stats::aov(elapsed ~ branch, data = timings_with_factors)
  var <- paste0("branch", branches[2])
  confint <- confint(fit, var, level = ci)
  confint <- round(100 * confint / reference, 2)
  emoji <- if (all(confint < 0)) {
    faster
  } else if (all(confint > 0)) {
    slower
  } else {
    no_change
  }
  list(
    string = paste0(
      "[",
      paste0(set_sign(confint), collapse = "%, "),
      "%]"
    ),
    emoji = emoji
  )
}


#' @param timing a benchmark read with [benchmark_read()], column `name` must
#'   only contain one unique value.
#' @keywords internal
benchmark_plot <- function(benchmark, timings) {
  timings %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$elapsed, color = .data$branch)) +
    ggplot2::geom_density()
  fs::path(dir_touchstone(), "plots", benchmark) %>%
    fs::path_ext_set("png") %>%
    ggplot2::ggsave()
}


#' Modifying the PR Comment
#'
#' The files `touchstone/header.R` and `touchstone/footer.R` allow you to modify
#' the PR comment. The files will be evaluated in the context of
#' [benchmark_analyze()] and should return one string containing the text.
#' You can use github markdown e.g. emojis like :tada: in the string.
#'
#' @section Header:
#' Available variables for glue substitution:
#' * ci: confidence interval
#' * branches: BASE and HEAD branches benchmarked against each other.
#'
#' @section Footer:
#'   There are no special variables available in the footer.
#'   You can access the benchmark results via [path_pr_comment()].
#'
#' @name pr_comment
#' @seealso [base::eval()] [base::parse()]
NULL

get_comment_text <- function(part = c("footer", "header"), default, env = parent.frame()) {
  part <- match.arg(part)
  file <- glue::glue("{part}.R")
  path <- fs::path(dir_touchstone(), file)

  if (!fs::file_exists(path)) {
    cli::cli_alert_info("No comment {part} found. Using default.")
    text <- default
  } else {
    text <- eval(parse(path), envir = env)
    if (!is.character(text)) {
      cli::cli_warn(
        c("Parsed comment {part} is not a valid string. Using default.",
          "i" = "See {.code ?touchstone::pr_comment} for more information."
        )
      )
      text <- default
    }
  }

  text
}
