% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{ref_upsample}
\alias{ref_upsample}
\title{Samples \code{ref}}
\usage{
ref_upsample(ref, n = 20)
}
\description{
A block is a permutation of all unique elements in \code{ref}. Then, we sample
\code{n} blocks. This is better than repeating one sample a certain number of
times because if compute resources steadily increase, the first sample will
always perform worse than the second, so the order within the blocks must be
random.
}
\keyword{internal}
