% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/core.R
\name{benchmark_run_ref_impl}
\alias{benchmark_run_ref_impl}
\title{Checkout a branch from a repo and run an iteration}
\usage{
benchmark_run_ref_impl(ref, block, expr_before_benchmark, ..., path_pkg)
}
\arguments{
\item{ref}{A character vector of length one to indicate the git ref (i.e.
commit, tag, branch etc) of the benchmarking.}

\item{block}{All refs appear once in a block.}

\item{expr_before_benchmark}{Character vector with code to run before
the benchmark is ran, will be evaluated with \code{\link[=exprs_eval]{exprs_eval()}}.}

\item{...}{Named character vector of length one with code to benchmark, will
be evaluated with \code{\link[=exprs_eval]{exprs_eval()}}.}

\item{path_pkg}{The path to the root of the package you want to benchmark.}
}
\description{
Checkout a branch from a repo and run an iteration
}
\keyword{internal}
