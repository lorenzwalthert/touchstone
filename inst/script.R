# see `help(run_script, package = 'touchstone')` on how to run this
# interactively

touchstone::refs_install() # installs branches to benchmark

# benchmark a function call from your package (two calls per branch)
touchstone::benchmark_run_ref(
  random_test = "yourpkg::f()", #<- TODO put the call you want to benchmark here
  n = 2
)

# benchmark any R expression (six calls per branch)
touchstone::benchmark_run_ref(
  more = "if (TRUE) {
    y <- yourpkg::f2( x= 3)
  }", #<- TODO put the call you want to benchmark here
  n = 6
)


# create artifacts used downstream in the GitHub Action
touchstone::benchmarks_analyze()
