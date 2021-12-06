# touchstone 0.1.0-rc

This is the initial CRAN release of {touchstone}. Please see `README.md` and 
`vignette("touchstone", package = "touchstone")` on how to get started.

**Breaking Changes to dev version**

* Complete API overhaul with many breaking changes (#83).
* `with_touchstone_lib()` was renamed to `run_script()`.
* `benchmark_run_ref()` does not accept a character input for code expressions
  anymore (#62).
