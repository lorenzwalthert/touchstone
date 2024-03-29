% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prepare.R
\name{branch_install_impl}
\alias{branch_install_impl}
\title{Checks out a source branch and install the package}
\usage{
branch_install_impl(
  branch = "main",
  path_pkg = ".",
  install_dependencies = FALSE
)
}
\arguments{
\item{branch}{The name of the branch which should be installed.}

\item{path_pkg}{The path to the repository to install.}

\item{install_dependencies}{Passed to \code{\link[remotes:install_local]{remotes::install_local()}}. Set to
\code{FALSE} can help when ran locally without internet connection.}
}
\value{
A character vector with library paths.
}
\description{
Checks out a source branch and install the package
}
\keyword{internal}
