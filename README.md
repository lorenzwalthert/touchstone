
<!-- badges: start -->

[![R build
status](https://github.com/lorenzwalthert/touchstone/workflows/R-CMD-check/badge.svg)](https://github.com/lorenzwalthert/touchstone/actions)
<!-- badges: end -->

# touchstone

<!-- badges: start -->

<!-- badges: end -->

The goal of touchstone is to benchmark code from different branches in
the same continuous benchmarking run. This is helpful because continuous
benchmarking that benchmarks just one commit is often very imprecise
since the computational power available on a service like GitHub actions
varies considerably between runs. Experience with styler showed that a
variation [around 30%](https://github.com/r-lib/styler/pull/679) is not
unusual.

## Installation

You can install the package from GitHub:

``` r
# install.packages("devtools")
devtools::install_github("lorenzwalthert/touchstone")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(touchstone)
library(magrittr)
## basic example code
timings <- benchmark_run_ref(
  refs = "main", expr_to_benchmark = "runif(100)", n = 2
)
#> ✓ Switched to branch main.
#> ✓ Installed branch main.
#> ✓ Ran 20 iterations of ref `main`.
#> ✓ Switched to branch main.
#> ✓ Installed branch main.
#> ✓ Ran 20 iterations of ref `main`.

timings %>%
  ggplot2::ggplot(ggplot2::aes(.data$elapsed, color = .data$ref)) +
  ggplot2::scale_x_continuous(trans = "log10") +
  ggplot2::geom_density()
```

<img src="man/figures/README-example-1.png" width="100%" /> We switch to
branch `main` of this package and run an expression to benchmark. In a
real-world scenario, you would:

  - Select multiple branches instead of just `main`. Benchmarking code
    will be ran on all of them, multiple time, in random order.
  - use a function that is exported from the package namespace you want
    to benchmark, because otherwise you would not be able to measure the
    performance difference between different branches.
