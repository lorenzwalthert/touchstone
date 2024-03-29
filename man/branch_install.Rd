% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prepare.R
\name{branch_install}
\alias{branch_install}
\title{Install branches}
\usage{
branch_install(
  branches = c(branch_get_or_fail("GITHUB_BASE_REF"),
    branch_get_or_fail("GITHUB_HEAD_REF")),
  path_pkg = ".",
  install_dependencies = FALSE
)
}
\arguments{
\item{branches}{The names of the branches in a character vector.}

\item{path_pkg}{The path to the repository to install.}

\item{install_dependencies}{Passed to \code{\link[remotes:install_local]{remotes::install_local()}}.}
}
\value{
The global and touchstone library paths in a character vector (invisibly).
}
\description{
Installs each \code{branch} in a separate library for isolation.
}
