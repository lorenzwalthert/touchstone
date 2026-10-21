# touchstone (development version)

* `benchmark_analyze()` now writes the benchmark results to the PR comment as a
  Markdown table instead of a bullet list. The new `sorting` argument orders
  the rows by name (the default), by performance improvement or regression, or
  by current or incoming running time. (#172)

# touchstone 0.1.0-rc

This is the initial CRAN release of {touchstone}. Please see `README.md` and 
`vignette("touchstone", package = "touchstone")` on how to get started.

**Breaking Changes to dev version**

* Complete API overhaul with many breaking changes (#83).
* `with_touchstone_lib()` was renamed to `run_script()`.
* `benchmark_run_ref()` does not accept a character input for code expressions
  anymore (#62).
