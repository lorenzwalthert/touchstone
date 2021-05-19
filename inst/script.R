# see `help(with_touchstone_lib, package = 'touchstone')` on how to run this
# interactively

touchstone::refs_install() # installs branches to benchmark

# benchmark a function call from your package (two calls per branch)
touchstone::benchmark_run_ref(
  random_test = "yourpkg::fun()",
  n = 2
)

# benchmark a function call from your package (six calls per branch)
touchstone::benchmark_run_ref(
  another_test = "yourpkg::fun2(x = 3)",
  n = 6
)


# create artifacts used downstream in the GitHub Action
touchstone::benchmarks_analyze()
