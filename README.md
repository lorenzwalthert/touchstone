
<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R build
status](https://github.com/lorenzwalthert/touchstone/workflows/R-CMD-check/badge.svg)](https://github.com/lorenzwalthert/touchstone/actions)
<!-- badges: end -->

# touchstone

{touchstone} is a developer tool for continuous benchmarking with a
focus on reliable relative measurement, uncertainty reporting and user
convenience. The results are directly reported as a comment in GitHub
Pull Requests.

![](man/figures/screenshot-pr-comment.png)

## Installation

You can install the package from CRAN:

``` r
install.packages("touchstone")
```

And the development version from
<a href="https://github.com/lorenzwalthert/touchstone" target="_blank">GitHub</a>
with:

``` r
# install.packages("devtools")
devtools::install_github("lorenzwalthert/touchstone")
```

## Getting Started

You can start using {touchstone} in your package repository with:

``` r
touchstone::use_touchstone()
```

For a detailed explanation on how to configure and use {touchstone} see
the [“Using
touchstone”](https://lorenzwalthert.github.io/touchstone/articles/touchstone.html)
vignette.

## Motivation

The motivation for touchstone is to provide accurate benchmarking
results for package developers. The following insights were the
motivation:

-   **Compute power in GitHub Action VMs varies too much for reliable
    isolated benchmarks**: Experience with styler showed that a
    variation [around 30%](https://github.com/r-lib/styler/pull/679) for
    identical benchmarking code is possible. The solution to this is to
    benchmark the two branches in one CI/CD run and look at *relative
    difference* between branches. This matters in particular when
    running one iteration of a benchmark takes long (&gt;&gt; a few
    seconds) and speed implications are not huge.

-   **Timelines of absolute changes are mostly noise:** Maintaining a
    timeline of absolute benchmark times is of limited use because of
    the first bullet, at least when benchmark results don’t vary
    significantly (&gt; 50%).

-   **Dependencies should be identical across versions:** R and package
    versions of dependencies must be fixed via
    [RSPM](https://packagemanager.rstudio.com/client/#/) to allow as
    much continuation as possible anyways. Changing the timestamp of
    RSPM can happen in PRs that are only dedicated to dependency
    updates.

-   **Pull requests are a natural unit for measuring speed:** Linking
    benchmarking to pull requests make sense because you can easily
    benchmark any revision against any other. Just open a pull request
    from one branch on another. You can also easily keep track of how
    performance of your development version evolves by opening a PR
    against a branch with the released version. No need to ever merge
    these. Once you release a new version of you package, you can change
    the target branch of the pull request to start anew. The pull
    request comments will preserve the information.

## Conceptual

For your PR branch and the target branch, {touchstone} will:

-   build the two versions of the package in isolated libraries.

-   run the code you want to benchmark, many times, in [random
    order](https://lorenzwalthert.github.io/touchstone/articles/inference.html#sampling).
    This ensures the accurate measurement of relative differences
    between the branches.

Once done with the measurements, it will

-   comment the results of the benchmarking on the PR.

-   create visualizations as Github Action artifacts that show how the
    distribution of the timings for both branches.

## Status

This package is experimental. You can see an example usage in
[{styler}](https://github.com/r-lib/styler/pull/799).
