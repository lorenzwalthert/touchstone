% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prepare.R
\name{refs_install}
\alias{refs_install}
\title{Install branches}
\usage{
refs_install(
  refs = c(ref_get_or_fail("GITHUB_BASE_REF"), ref_get_or_fail("GITHUB_HEAD_REF")),
  path_pkg = ".",
  install_dependencies = FALSE
)
}
\arguments{
\item{refs}{The names of the branches in a character vector.}

\item{path_pkg}{The path to the repository to install.}

\item{install_dependencies}{Passed to ``}
}
\value{
The global and touchstone library paths in a character vector (invisibly).
}
\description{
Installs each \code{ref} in a separate library for isolation.
}
