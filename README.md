
<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![R build
status](https://github.com/lorenzwalthert/touchstone/workflows/R-CMD-check/badge.svg)](https://github.com/lorenzwalthert/touchstone/actions)
<!-- badges: end -->

# touchstone

{touchstone} is a developer tool for benchmarking R pull requests with a
focus on reliable relative measurement, uncertainty reporting and user
convenience. The results are directly reported as a comment in the pull
request.

![](man/figures/screenshot-pr-comment.png)

## Installation

You can install the package from CRAN:

``` r
install.packages("touchstone")
```

## Motivation

The motivation for touchstone is to provide accurate benchmarking
results for package developers. The following insights were the
motivation:

  - **Compute power in GitHub Action VMs varies too much for reliable
    isolated benchmarks**: Experience with styler showed that a
    variation [around 30%](https://github.com/r-lib/styler/pull/679) for
    identical benchmarking code is possible. The solution to this is to
    benchmark the two branches in one CI/CD run and look at *relative
    difference* between branches. This matters in particular when
    running one iteration of a benchmark takes long (\>\> a few seconds)
    and speed implications are not huge.

  - **Timelines of absolute changes are mostly noise:** Maintaining a
    timeline of absolute benchmark times is of limited use because of
    the first bullet, at least when benchmark results don’t vary
    significantly (\> 50%).

  - **Dependencies should be identical across versions:** R and package
    versions of dependencies must be fixed via
    [RSPM](http://packagemanager.rstudio.com) to allow as much
    continuation as possible anyways. Changing the timestamp of RSPM can
    happen in PRs that are only dedicated to dependency updates.

  - **Pull requests are a natural unit for measuring speed:** Linking
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

  - build the two versions of the package in isolated libraries.

  - run the code you want to benchmark, many times, in [random
    order](https://lorenzwalthert.github.io/touchstone/articles/inference.html#sampling).
    This ensures the accurate measurement of relative differences
    between the branches.

Once done with the measurements, it will

  - comment the results of the benchmarking on the PR.

  - create visualizations as Github Action artifacts that show how the
    distribution of the timings for both branches.

## Proposed Workflow

Initialize {touchstone} in your repo with

``` r
touchstone::use_touchstone()
```

This will:

  - create a `touchstone` directory in the repo root with:
    
      - `config.json` that defines how to run your benchmark. In
        particular, you can define a `benchmarking_repo`, that is, a
        repo you need to run your bench mark (use `""` if you don’t need
        an additional git repo checked out for your benchmark). This
        repo will be cloned into `benchmarking_path`. For example to
        benchmark {styler}, we format the {here} package with
        `style_pkg()` that is not part of the {styler} repo, but with
        the below config, will be located at `touchstone/sources/here`
        during benchmarking. The code you want to benchmark comes from
        the benchmarked repo, which in our case is the root from where
        you call `use_touchstone()` and hence does not need to be
        defined explicitly.
    
    <!-- end list -->
    
    ``` json
    
    { 
      "benchmarking_repo": "lorenzwalthert/here", 
      "benchmarking_ref": "ca9c8e69c727def88d8ba1c8b85b0e0bcea87b3f", 
      "benchmarking_path": "touchstone/sources/here", 
      "os": "ubuntu-18.04", 
      "r": "4.0.0", 
      "rspm": "https://packagemanager.rstudio.com/all/__linux__/bionic/291" 
    }
    ```
    
      - `script.R`, the script that runs the benchmark.
    
    <!-- end list -->
    
    ``` r
    touchstone::refs_install() # installs branches to benchmark
    
    # benchmark a function call from your package (two calls per branch)
    touchstone::benchmark_run_ref(
    random_test = yourpkg::fun(),
    n = 2
    )
    
    # create artifacts used downstream in the GitHub Action
    touchstone::benchmarks_analyze()
    ```

  - write the workflow files you need for touchstone into
    `.github/workflows/` to invoke your touchstone script.

Note that these files must be committed to the default branch before
{touchstone} continuous benchmarking will be triggered for new PRs.

If you want to call the script interactively, use `run_script`()`(an
enhanced version of`base::source()`) for various technical reasons
described in the help file. Note that`benchmark\_run\_ref()\` will check
out different branches and should therefore be the only process running
in the directory and only in a clean git working directory.

## Status

This package is experimental. You can see an example usage in
[{styler}](https://github.com/r-lib/styler/pull/799).
